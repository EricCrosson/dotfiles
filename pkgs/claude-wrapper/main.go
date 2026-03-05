package main

import (
	"fmt"
	"os"
	"strings"
	"syscall"
)

// ParsedArgs holds the processed command-line arguments
type ParsedArgs struct {
	explicitAnthropic bool
	hasModel          bool
	hasHelpOrVersion  bool
	filteredArgs      []string // Args with --anthropic removed
}

func main() {
	args := parseArgs(os.Args[1:])

	// Fast path: help/version - pass through immediately
	if args.hasHelpOrVersion {
		execClaudeUnwrapped(args.filteredArgs)
		return
	}

	// Bedrock is the default. Set CLAUDE_CODE_USE_BEDROCK unless --anthropic
	// was passed to use the first-party Anthropic API instead.
	if !args.explicitAnthropic {
		os.Setenv("CLAUDE_CODE_USE_BEDROCK", "1")
	}
	args = applyModelDefaults(args)

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
		case arg == "--anthropic":
			parsed.explicitAnthropic = true
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

// applyModelDefaults prepends a default --model flag when the user has not
// already specified one. Bedrock (the default) uses a region-prefixed model ID.
func applyModelDefaults(args ParsedArgs) ParsedArgs {
	if args.hasModel {
		return args
	}
	model := "us.anthropic.claude-opus-4-6-v1"
	if args.explicitAnthropic {
		model = "claude-opus-4-6"
	}
	args.filteredArgs = append([]string{"--model", model}, args.filteredArgs...)
	return args
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
