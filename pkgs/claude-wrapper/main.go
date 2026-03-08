package main

import (
	"fmt"
	"os"
	"strings"
	"syscall"
)

// ParsedArgs holds the processed command-line arguments
type ParsedArgs struct {
	explicitBedrock  bool
	hasModel         bool
	hasHelpOrVersion bool
	filteredArgs     []string // Args with --bedrock removed
}

// BedrockConfig holds the environment variables and extra CLI args needed
// for Bedrock mode. Returned by configureBedrock as a pure value so tests
// can assert without os.Setenv pollution.
type BedrockConfig struct {
	envVars   map[string]string
	extraArgs []string
}

func main() {
	args := parseArgs(os.Args[1:])

	// Fast path: help/version - pass through immediately
	if args.hasHelpOrVersion {
		execClaudeUnwrapped(args.filteredArgs)
		return
	}

	// Anthropic is the default. When --bedrock is passed, configure for
	// AWS Bedrock instead.
	if args.explicitBedrock {
		cfg, err := configureBedrock(args, os.Getenv, readFileString)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			os.Exit(1)
		}
		for k, v := range cfg.envVars {
			os.Setenv(k, v)
		}
		args.filteredArgs = append(args.filteredArgs, cfg.extraArgs...)
	} else {
		args.filteredArgs = configureAnthropicDefaults(args)
	}

	// Mark as Claude session
	os.Setenv("_CLAUDE_SESSION", "1")

	// Exec claude-unwrapped
	execClaudeUnwrapped(args.filteredArgs)
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

// configureBedrock builds the Bedrock configuration from environment and sops
// files. It is a pure function: all I/O is injected via getenv and readFile.
func configureBedrock(args ParsedArgs, getenv func(string) string, readFile func(string) (string, error)) (BedrockConfig, error) {
	opusFile := getenv("_CLAUDE_BEDROCK_OPUS_FILE")
	sonnetFile := getenv("_CLAUDE_BEDROCK_SONNET_FILE")
	haikuFile := getenv("_CLAUDE_BEDROCK_HAIKU_FILE")

	for _, f := range []string{opusFile, sonnetFile, haikuFile} {
		if f == "" {
			return BedrockConfig{}, fmt.Errorf("bedrock model file path not configured; is the Nix wrapper active?")
		}
	}

	type modelFile struct {
		name string
		path string
	}
	modelFiles := []modelFile{
		{"opus", opusFile},
		{"sonnet", sonnetFile},
		{"haiku", haikuFile},
	}

	arns := make([]string, len(modelFiles))
	for i, mf := range modelFiles {
		raw, err := readFile(mf.path)
		if err != nil {
			return BedrockConfig{}, fmt.Errorf("bedrock model file not found at %s\nRun 'darwin-rebuild switch' to decrypt sops secrets.", mf.path)
		}
		trimmed := strings.TrimSpace(raw)
		if trimmed == "" {
			return BedrockConfig{}, fmt.Errorf("%s model ARN is empty after reading %s", mf.name, mf.path)
		}
		arns[i] = trimmed
	}

	opusARN, sonnetARN, haikuARN := arns[0], arns[1], arns[2]

	envVars := map[string]string{
		"CLAUDE_CODE_USE_BEDROCK":      "1",
		"ANTHROPIC_MODEL":              opusARN,
		"ANTHROPIC_DEFAULT_OPUS_MODEL":   opusARN,
		"ANTHROPIC_DEFAULT_SONNET_MODEL": sonnetARN,
		"ANTHROPIC_DEFAULT_HAIKU_MODEL":  haikuARN,
	}

	profile := getenv("_CLAUDE_BEDROCK_PROFILE")
	if profile != "" {
		envVars["AWS_PROFILE"] = profile
	}
	region := getenv("_CLAUDE_BEDROCK_REGION")
	if region != "" {
		envVars["AWS_REGION"] = region
	}

	var extraArgs []string
	if !args.hasModel {
		extraArgs = append(extraArgs, "--model", "opus")
	}
	extraArgs = append(extraArgs, "--settings", `{"availableModels":["opus","sonnet","haiku"]}`)

	return BedrockConfig{envVars: envVars, extraArgs: extraArgs}, nil
}

// configureAnthropicDefaults returns the final CLI args for the Anthropic path,
// defaulting to opus when no model is explicitly specified.
func configureAnthropicDefaults(args ParsedArgs) []string {
	if args.hasModel {
		return args.filteredArgs
	}
	return append(args.filteredArgs, "--model", "opus")
}

// readFileString reads a file and returns its contents as a string.
func readFileString(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

func execClaudeUnwrapped(args []string) {
	binary := os.Getenv("_CLAUDE_UNWRAPPED")
	if binary == "" {
		fmt.Fprintf(os.Stderr, "_CLAUDE_UNWRAPPED not set\n")
		os.Exit(1)
	}

	// Build argv: first element is the program name
	argv := append([]string{"claude"}, args...)
	env := os.Environ()

	err := syscall.Exec(binary, argv, env)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to exec %s: %v\n", binary, err)
		os.Exit(1)
	}
}
