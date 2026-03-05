package main

import (
	"reflect"
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

func TestApplyModelDefaults(t *testing.T) {
	tests := []struct {
		name             string
		input            ParsedArgs
		wantFilteredArgs []string
	}{
		{
			name: "Bedrock (default), no model",
			input: ParsedArgs{
				explicitAnthropic: false,
				hasModel:          false,
				filteredArgs:      []string{"--chat"},
			},
			wantFilteredArgs: []string{"--model", "us.anthropic.claude-opus-4-6-v1", "--chat"},
		},
		{
			name: "Bedrock (default), with model",
			input: ParsedArgs{
				explicitAnthropic: false,
				hasModel:          true,
				filteredArgs:      []string{"--model", "my-model", "--chat"},
			},
			wantFilteredArgs: []string{"--model", "my-model", "--chat"},
		},
		{
			name: "Anthropic API, no model",
			input: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          false,
				filteredArgs:      []string{"--chat"},
			},
			wantFilteredArgs: []string{"--model", "claude-opus-4-6", "--chat"},
		},
		{
			name: "Anthropic API, with model",
			input: ParsedArgs{
				explicitAnthropic: true,
				hasModel:          true,
				filteredArgs:      []string{"--model", "my-model", "--chat"},
			},
			wantFilteredArgs: []string{"--model", "my-model", "--chat"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := applyModelDefaults(tt.input)
			if !reflect.DeepEqual(got.filteredArgs, tt.wantFilteredArgs) {
				t.Errorf("filteredArgs = %v, want %v", got.filteredArgs, tt.wantFilteredArgs)
			}
		})
	}
}
