package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

// Config holds Nix-injected configuration
type Config struct {
	AWSProfile       string
	AWSRegion        string
	BedrockThreshold int
	EnvTemplate      string
}

// ParsedArgs holds the processed command-line arguments
type ParsedArgs struct {
	explicitBedrock  bool
	hasModel         bool
	hasHelpOrVersion bool
	filteredArgs     []string // Args with --bedrock removed
}

// OAuthCredentials holds the access token from keychain
type OAuthCredentials struct {
	ClaudeAiOauth struct {
		AccessToken string `json:"accessToken"`
	} `json:"claudeAiOauth"`
}

// OAuthUsageResponse holds the API response for usage
type OAuthUsageResponse struct {
	FiveHour struct {
		Utilization float64 `json:"utilization"`
	} `json:"five_hour"`
}

func main() {
	config := readConfig()
	args := parseArgs(os.Args[1:])

	// Fast path: help/version - pass through immediately
	if args.hasHelpOrVersion {
		execClaudeUnwrapped(args.filteredArgs)
		return
	}

	// Check if Bedrock is already enabled via environment
	if os.Getenv("CLAUDE_CODE_USE_BEDROCK") != "" {
		args.explicitBedrock = true
	}

	// Auto-detection logic: only if not explicitly set
	if !args.explicitBedrock && shouldAutoDetectBedrock(config) {
		args.explicitBedrock = true
	}

	// If using Bedrock and no model specified, add default
	if args.explicitBedrock {
		os.Setenv("CLAUDE_CODE_USE_BEDROCK", "1")
		if !args.hasModel {
			// Prepend model to args
			args.filteredArgs = append(
				[]string{"--model", "us.anthropic.claude-opus-4-6-v1"},
				args.filteredArgs...,
			)
		}
	}

	// Mark as Claude session
	os.Setenv("_CLAUDE_SESSION", "1")

	// Resolve 1Password secrets
	resolveSecrets(config)

	// Exec claude-unwrapped
	execClaudeUnwrapped(args.filteredArgs)
}

func readConfig() Config {
	return Config{
		AWSProfile:       os.Getenv("AWS_PROFILE"),
		AWSRegion:        os.Getenv("AWS_REGION"),
		BedrockThreshold: getEnvInt("BEDROCK_THRESHOLD", 80),
		EnvTemplate:      os.Getenv("ENV_TEMPLATE"),
	}
}

func parseArgs(args []string) ParsedArgs {
	parsed := ParsedArgs{
		filteredArgs: make([]string, 0, len(args)),
	}

	i := 0
	for i < len(args) {
		arg := args[i]

		switch {
		case arg == "--bedrock":
			parsed.explicitBedrock = true
			// Don't add to filteredArgs (remove it)
			i++

		case arg == "--model":
			parsed.hasModel = true
			parsed.filteredArgs = append(parsed.filteredArgs, arg)
			i++
			// Also need to include the next argument (the model value)
			if i < len(args) {
				parsed.filteredArgs = append(parsed.filteredArgs, args[i])
				i++
			}

		case strings.HasPrefix(arg, "--model="):
			parsed.hasModel = true
			parsed.filteredArgs = append(parsed.filteredArgs, arg)
			i++

		case arg == "--help" || arg == "-h" || arg == "--version" || arg == "-v":
			parsed.hasHelpOrVersion = true
			parsed.filteredArgs = append(parsed.filteredArgs, arg)
			i++

		default:
			parsed.filteredArgs = append(parsed.filteredArgs, arg)
			i++
		}
	}

	return parsed
}

func shouldAutoDetectBedrock(config Config) bool {
	// Get OAuth credentials from keychain
	creds := getKeychainCredentials()
	if creds == nil {
		return false
	}

	// Query OAuth usage API
	utilization := getOAuthUtilization(creds.ClaudeAiOauth.AccessToken)
	if utilization < 0 {
		return false
	}

	return utilization >= config.BedrockThreshold
}

func getKeychainCredentials() *OAuthCredentials {
	// Run: security find-generic-password -s "Claude Code-credentials" -a "$USER" -w
	securityCmd := os.Getenv("_SECURITY_CMD")
	if securityCmd == "" {
		securityCmd = "/usr/bin/security"
	}

	cmd := exec.Command(securityCmd, "find-generic-password",
		"-s", "Claude Code-credentials",
		"-a", os.Getenv("USER"),
		"-w")

	output, err := cmd.Output()
	if err != nil {
		return nil
	}

	var creds OAuthCredentials
	if err := json.Unmarshal(output, &creds); err != nil {
		return nil
	}

	return &creds
}

func getOAuthUtilization(token string) int {
	if token == "" {
		return -1
	}

	// Use curl command (like bash version) so test mocks work
	cmd := exec.Command("curl", "-s", "--max-time", "2",
		"-H", "Authorization: Bearer "+token,
		"-H", "anthropic-beta: oauth-2025-04-20",
		"-H", "User-Agent: claude-code",
		"https://api.anthropic.com/api/oauth/usage")

	output, err := cmd.Output()
	if err != nil {
		return -1
	}

	var usage OAuthUsageResponse
	if err := json.Unmarshal(output, &usage); err != nil {
		return -1
	}

	// Round to nearest int (same as bash `jq -r '... | round'`)
	return int(usage.FiveHour.Utilization + 0.5)
}

func resolveSecrets(config Config) {
	// Run: op run --no-masking --env-file="$ENV_TEMPLATE" -- bash -c 'printf ...'
	cmd := exec.Command("op", "run", "--no-masking",
		"--env-file="+config.EnvTemplate,
		"--", "bash", "-c",
		`printf "export CLAUDE_CODE_GITHUB_TOKEN=%q\n" "$CLAUDE_CODE_GITHUB_TOKEN"
printf "export GH_TOKEN=%q\n" "$GH_TOKEN"
printf "export JIRA_API_TOKEN=%q\n" "$JIRA_API_TOKEN"`)

	output, err := cmd.Output()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to resolve 1Password secrets: %v\n", err)
		os.Exit(1)
	}

	// Parse and set environment variables
	lines := strings.Split(string(output), "\n")
	for _, line := range lines {
		if strings.HasPrefix(line, "export ") {
			line = strings.TrimPrefix(line, "export ")
			parts := strings.SplitN(line, "=", 2)
			if len(parts) == 2 {
				// Remove surrounding quotes from value
				value := parts[1]
				value = strings.TrimPrefix(value, "'")
				value = strings.TrimSuffix(value, "'")
				value = strings.TrimPrefix(value, "\"")
				value = strings.TrimSuffix(value, "\"")
				os.Setenv(parts[0], value)
			}
		}
	}
}

func execClaudeUnwrapped(args []string) {
	binary, err := exec.LookPath("claude-unwrapped")
	if err != nil {
		fmt.Fprintf(os.Stderr, "claude-unwrapped not found in PATH\n")
		os.Exit(1)
	}

	// Build argv: first element is the program name
	argv := append([]string{"claude-unwrapped"}, args...)
	env := os.Environ()

	err = syscall.Exec(binary, argv, env)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to exec claude-unwrapped: %v\n", err)
		os.Exit(1)
	}
}

// Helper function to get int from environment with default
func getEnvInt(key string, defaultValue int) int {
	val := os.Getenv(key)
	if val == "" {
		return defaultValue
	}
	var result int
	_, err := fmt.Sscanf(val, "%d", &result)
	if err != nil {
		return defaultValue
	}
	return result
}
