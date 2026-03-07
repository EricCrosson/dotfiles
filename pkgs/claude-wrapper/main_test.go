package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestParseArgs_PositionIndependent(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want ParsedArgs
	}{
		{
			name: "anthropic first",
			args: []string{"--anthropic", "--chat"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          false,
				filteredArgs:      []string{"--chat"},
			},
		},
		{
			name: "anthropic after other flags",
			args: []string{"--chat", "--anthropic"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          false,
				filteredArgs:      []string{"--chat"},
			},
		},
		{
			name: "anthropic with explicit model (space-separated)",
			args: []string{"--model", "sonnet", "--anthropic", "--chat"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          true,
				filteredArgs:      []string{"--model", "sonnet", "--chat"},
			},
		},
		{
			name: "anthropic with explicit model (equals syntax)",
			args: []string{"--anthropic", "--model=opus"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          true,
				filteredArgs:      []string{"--model=opus"},
			},
		},
		{
			name: "no flags",
			args: []string{"--chat"},
			want: ParsedArgs{
				explicitAnthropic: false,
				hasModel:          false,
				filteredArgs:      []string{"--chat"},
			},
		},
		{
			name: "multiple flags with anthropic in middle",
			args: []string{"--chat", "--anthropic", "--verbose"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          false,
				filteredArgs:      []string{"--chat", "--verbose"},
			},
		},
		{
			name: "help flag",
			args: []string{"--help"},
			want: ParsedArgs{
				explicitAnthropic: false,
				hasModel:          false,
				hasHelpOrVersion:  true,
				filteredArgs:      []string{"--help"},
			},
		},
		{
			name: "version flag with anthropic",
			args: []string{"--anthropic", "--version"},
			want: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          false,
				hasHelpOrVersion:  true,
				filteredArgs:      []string{"--version"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseArgs(tt.args)

			if got.explicitAnthropic != tt.want.explicitAnthropic {
				t.Errorf("explicitAnthropic = %v, want %v", got.explicitAnthropic, tt.want.explicitAnthropic)
			}
			if got.hasModel != tt.want.hasModel {
				t.Errorf("hasModel = %v, want %v", got.hasModel, tt.want.hasModel)
			}
			if got.hasHelpOrVersion != tt.want.hasHelpOrVersion {
				t.Errorf("hasHelpOrVersion = %v, want %v", got.hasHelpOrVersion, tt.want.hasHelpOrVersion)
			}

			// Compare filteredArgs
			if !reflect.DeepEqual(got.filteredArgs, tt.want.filteredArgs) {
				t.Errorf("filteredArgs = %v, want %v", got.filteredArgs, tt.want.filteredArgs)
			}
		})
	}
}

func TestBuildSettings(t *testing.T) {
	tests := []struct {
		name   string
		envVal string
		want   []string
	}{
		{
			name:   "three models",
			envVal: "opus,sonnet,haiku",
			want:   []string{"--settings", `{"availableModels":["opus","sonnet","haiku"]}`},
		},
		{
			name:   "empty string",
			envVal: "",
			want:   nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			getenv := func(key string) string {
				if key == "_CLAUDE_AVAILABLE_MODELS" {
					return tt.envVal
				}
				return ""
			}
			got := buildSettings(getenv)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("buildSettings() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSettingsInjection_Integration(t *testing.T) {
	tests := []struct {
		name               string
		args               []string
		availableModels    string
		wantSettingsInArgs bool
		wantModelDefault   bool
	}{
		{
			name:               "bedrock with available models",
			args:               []string{"--chat"},
			availableModels:    "opus,sonnet,haiku",
			wantSettingsInArgs: true,
			wantModelDefault:   true,
		},
		{
			name:               "bedrock without available models",
			args:               []string{"--chat"},
			availableModels:    "",
			wantSettingsInArgs: false,
			wantModelDefault:   true,
		},
		{
			name:               "anthropic ignores available models",
			args:               []string{"--anthropic", "--chat"},
			availableModels:    "opus,sonnet,haiku",
			wantSettingsInArgs: false,
			wantModelDefault:   false,
		},
		{
			name:               "bedrock with explicit model skips default",
			args:               []string{"--model", "haiku", "--chat"},
			availableModels:    "opus,sonnet,haiku",
			wantSettingsInArgs: true,
			wantModelDefault:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parsed := parseArgs(tt.args)

			// Simulate the main() logic for settings and model injection
			var finalArgs []string
			if parsed.explicitAnthropic {
				finalArgs = parsed.filteredArgs
			} else {
				finalArgs = parsed.filteredArgs
				if !parsed.hasModel {
					finalArgs = append(finalArgs, "--model", "opus")
				}
				getenv := func(key string) string {
					if key == "_CLAUDE_AVAILABLE_MODELS" {
						return tt.availableModels
					}
					return ""
				}
				if extra := buildSettings(getenv); extra != nil {
					finalArgs = append(finalArgs, extra...)
				}
			}

			hasSettings := false
			hasModelDefault := false
			for i, arg := range finalArgs {
				if strings.Contains(arg, "availableModels") {
					hasSettings = true
				}
				if arg == "--model" && i+1 < len(finalArgs) && finalArgs[i+1] == "opus" && !parsed.hasModel {
					hasModelDefault = true
				}
			}

			if hasSettings != tt.wantSettingsInArgs {
				t.Errorf("settings in args = %v, want %v (args: %v)", hasSettings, tt.wantSettingsInArgs, finalArgs)
			}
			if hasModelDefault != tt.wantModelDefault {
				t.Errorf("model default in args = %v, want %v (args: %v)", hasModelDefault, tt.wantModelDefault, finalArgs)
			}
		})
	}
}

