package main

import (
	"fmt"
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
			name: "bedrock first",
			args: []string{"--bedrock", "--chat"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        false,
				filteredArgs:    []string{"--chat"},
			},
		},
		{
			name: "bedrock after other flags",
			args: []string{"--chat", "--bedrock"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        false,
				filteredArgs:    []string{"--chat"},
			},
		},
		{
			name: "bedrock with explicit model (space-separated)",
			args: []string{"--model", "sonnet", "--bedrock", "--chat"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        true,
				filteredArgs:    []string{"--model", "sonnet", "--chat"},
			},
		},
		{
			name: "bedrock with explicit model (equals syntax)",
			args: []string{"--bedrock", "--model=opus"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        true,
				filteredArgs:    []string{"--model=opus"},
			},
		},
		{
			name: "no flags",
			args: []string{"--chat"},
			want: ParsedArgs{
				explicitBedrock: false,
				hasModel:        false,
				filteredArgs:    []string{"--chat"},
			},
		},
		{
			name: "multiple flags with bedrock in middle",
			args: []string{"--chat", "--bedrock", "--verbose"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        false,
				filteredArgs:    []string{"--chat", "--verbose"},
			},
		},
		{
			name: "help flag",
			args: []string{"--help"},
			want: ParsedArgs{
				explicitBedrock: false,
				hasModel:        false,
				hasHelpOrVersion: true,
				filteredArgs:     []string{"--help"},
			},
		},
		{
			name: "version flag with bedrock",
			args: []string{"--bedrock", "--version"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        false,
				hasHelpOrVersion: true,
				filteredArgs:     []string{"--version"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseArgs(tt.args)

			if got.explicitBedrock != tt.want.explicitBedrock {
				t.Errorf("explicitBedrock = %v, want %v", got.explicitBedrock, tt.want.explicitBedrock)
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

func TestDetectTheme(t *testing.T) {
	t.Run("dark when command succeeds", func(t *testing.T) {
		got := detectTheme(func(name string, args ...string) error {
			return nil
		})
		if got != "dark" {
			t.Errorf("detectTheme() = %q, want %q", got, "dark")
		}
	})

	t.Run("light when command fails", func(t *testing.T) {
		got := detectTheme(func(name string, args ...string) error {
			return fmt.Errorf("exit status 1")
		})
		if got != "light" {
			t.Errorf("detectTheme() = %q, want %q", got, "light")
		}
	})
}

func TestConfigureBedrock(t *testing.T) {
	mockGetenv := func(vals map[string]string) func(string) string {
		return func(key string) string {
			return vals[key]
		}
	}

	mockReadFile := func(contents map[string]string) func(string) (string, error) {
		return func(path string) (string, error) {
			if v, ok := contents[path]; ok {
				return v, nil
			}
			return "", &fileNotFoundError{path: path}
		}
	}

	tests := []struct {
		name     string
		args     ParsedArgs
		env      map[string]string
		files    map[string]string
		wantCfg  BedrockConfig
		wantErr  string
	}{
		{
			name: "success with default model",
			args: ParsedArgs{explicitBedrock: true, filteredArgs: []string{"--chat"}},
			env: map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
				"_CLAUDE_BEDROCK_PROFILE":     "bitgo-ai",
				"_CLAUDE_BEDROCK_REGION":      "us-west-2",
			},
			files: map[string]string{
				"/sops/opus":   "arn:aws:bedrock:opus-arn\n",
				"/sops/sonnet": "arn:aws:bedrock:sonnet-arn\n",
				"/sops/haiku":  "arn:aws:bedrock:haiku-arn\n",
			},
			wantCfg: BedrockConfig{
				envVars: map[string]string{
					"CLAUDE_CODE_USE_BEDROCK":      "1",
					"AWS_PROFILE":                  "bitgo-ai",
					"AWS_REGION":                   "us-west-2",
					"ANTHROPIC_MODEL":              "arn:aws:bedrock:opus-arn",
					"ANTHROPIC_DEFAULT_OPUS_MODEL":   "arn:aws:bedrock:opus-arn",
					"ANTHROPIC_DEFAULT_SONNET_MODEL": "arn:aws:bedrock:sonnet-arn",
					"ANTHROPIC_DEFAULT_HAIKU_MODEL":  "arn:aws:bedrock:haiku-arn",
				},
				extraArgs: []string{
					"--model", "opus",
					"--settings", `{"availableModels":["opus","sonnet","haiku"],"theme":"dark"}`,
				},
			},
		},
		{
			name: "explicit model skips default",
			args: ParsedArgs{explicitBedrock: true, hasModel: true, filteredArgs: []string{"--model", "haiku", "--chat"}},
			env: map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
				"_CLAUDE_BEDROCK_PROFILE":     "bitgo-ai",
				"_CLAUDE_BEDROCK_REGION":      "us-west-2",
			},
			files: map[string]string{
				"/sops/opus":   "arn:aws:bedrock:opus-arn\n",
				"/sops/sonnet": "arn:aws:bedrock:sonnet-arn\n",
				"/sops/haiku":  "arn:aws:bedrock:haiku-arn\n",
			},
			wantCfg: BedrockConfig{
				envVars: map[string]string{
					"CLAUDE_CODE_USE_BEDROCK":      "1",
					"AWS_PROFILE":                  "bitgo-ai",
					"AWS_REGION":                   "us-west-2",
					"ANTHROPIC_MODEL":              "arn:aws:bedrock:opus-arn",
					"ANTHROPIC_DEFAULT_OPUS_MODEL":   "arn:aws:bedrock:opus-arn",
					"ANTHROPIC_DEFAULT_SONNET_MODEL": "arn:aws:bedrock:sonnet-arn",
					"ANTHROPIC_DEFAULT_HAIKU_MODEL":  "arn:aws:bedrock:haiku-arn",
				},
				extraArgs: []string{
					"--settings", `{"availableModels":["opus","sonnet","haiku"],"theme":"dark"}`,
				},
			},
		},
		{
			name: "missing opus file path env var",
			args: ParsedArgs{explicitBedrock: true, filteredArgs: []string{"--chat"}},
			env: map[string]string{
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
			},
			files:   map[string]string{},
			wantErr: "not configured",
		},
		{
			name: "file not found on disk",
			args: ParsedArgs{explicitBedrock: true, filteredArgs: []string{"--chat"}},
			env: map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
			},
			files:   map[string]string{},
			wantErr: "darwin-rebuild switch",
		},
		{
			name: "empty ARN after trim",
			args: ParsedArgs{explicitBedrock: true, filteredArgs: []string{"--chat"}},
			env: map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
			},
			files: map[string]string{
				"/sops/opus":   "  \n",
				"/sops/sonnet": "arn:aws:bedrock:sonnet-arn\n",
				"/sops/haiku":  "arn:aws:bedrock:haiku-arn\n",
			},
			wantErr: "empty",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cfg, err := configureBedrock(tt.args, "dark", mockGetenv(tt.env), mockReadFile(tt.files))

			if tt.wantErr != "" {
				if err == nil {
					t.Fatalf("expected error containing %q, got nil", tt.wantErr)
				}
				if !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("expected error containing %q, got %q", tt.wantErr, err.Error())
				}
				return
			}

			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if !reflect.DeepEqual(cfg.envVars, tt.wantCfg.envVars) {
				t.Errorf("envVars = %v, want %v", cfg.envVars, tt.wantCfg.envVars)
			}
			if !reflect.DeepEqual(cfg.extraArgs, tt.wantCfg.extraArgs) {
				t.Errorf("extraArgs = %v, want %v", cfg.extraArgs, tt.wantCfg.extraArgs)
			}
		})
	}
}

// fileNotFoundError simulates os.ReadFile behavior for missing files
type fileNotFoundError struct {
	path string
}

func (e *fileNotFoundError) Error() string {
	return "open " + e.path + ": no such file or directory"
}

func TestConfigureAnthropicDefaults(t *testing.T) {
	tests := []struct {
		name     string
		args     ParsedArgs
		theme    string
		wantArgs []string
	}{
		{
			name:     "no model specified - injects opus and theme",
			args:     ParsedArgs{filteredArgs: []string{"--chat"}},
			theme:    "dark",
			wantArgs: []string{"--chat", "--model", "opus", "--settings", `{"theme":"dark"}`},
		},
		{
			name:     "explicit model preserved with theme",
			args:     ParsedArgs{hasModel: true, filteredArgs: []string{"--model", "sonnet", "--chat"}},
			theme:    "light",
			wantArgs: []string{"--model", "sonnet", "--chat", "--settings", `{"theme":"light"}`},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := configureAnthropicDefaults(tt.args, tt.theme)
			if !reflect.DeepEqual(got, tt.wantArgs) {
				t.Errorf("configureAnthropicDefaults() = %v, want %v", got, tt.wantArgs)
			}
		})
	}
}

func TestDefaultPath_NoBedrockConfig(t *testing.T) {
	// The default (Anthropic) path must not produce any Bedrock env vars,
	// model injection, or settings injection.
	args := parseArgs([]string{"--chat"})

	if args.explicitBedrock {
		t.Fatal("default path should not have explicitBedrock set")
	}

	// Simulate main() logic: no --bedrock means no configureBedrock call,
	// so no env vars, no extra args
	if args.hasModel {
		t.Error("default path should not have hasModel set")
	}

	// filteredArgs should be exactly the original args
	want := []string{"--chat"}
	if !reflect.DeepEqual(args.filteredArgs, want) {
		t.Errorf("filteredArgs = %v, want %v", args.filteredArgs, want)
	}
}

func TestBedrockPath_Integration(t *testing.T) {
	tests := []struct {
		name               string
		args               []string
		wantSettingsInArgs bool
		wantModelDefault   bool
	}{
		{
			name:               "anthropic default (no flags) - injects opus model",
			args:               []string{"--chat"},
			wantSettingsInArgs: true,
			wantModelDefault:   true,
		},
		{
			name:               "anthropic with explicit model - preserves model",
			args:               []string{"--model", "sonnet", "--chat"},
			wantSettingsInArgs: true,
			wantModelDefault:   false,
		},
		{
			name:               "bedrock with default model",
			args:               []string{"--bedrock", "--chat"},
			wantSettingsInArgs: true,
			wantModelDefault:   true,
		},
		{
			name:               "bedrock with explicit model skips default",
			args:               []string{"--bedrock", "--model", "haiku", "--chat"},
			wantSettingsInArgs: true,
			wantModelDefault:   false,
		},
	}

	env := map[string]string{
		"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
		"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
		"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
		"_CLAUDE_BEDROCK_PROFILE":     "bitgo-ai",
		"_CLAUDE_BEDROCK_REGION":      "us-west-2",
	}
	files := map[string]string{
		"/sops/opus":   "arn:opus\n",
		"/sops/sonnet": "arn:sonnet\n",
		"/sops/haiku":  "arn:haiku\n",
	}
	getenv := func(key string) string { return env[key] }
	readFile := func(path string) (string, error) {
		if v, ok := files[path]; ok {
			return v, nil
		}
		return "", &fileNotFoundError{path: path}
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parsed := parseArgs(tt.args)

			var finalArgs []string
			if parsed.explicitBedrock {
				cfg, err := configureBedrock(parsed, "dark", getenv, readFile)
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				finalArgs = append(parsed.filteredArgs, cfg.extraArgs...)

				// Verify CLAUDE_CODE_USE_BEDROCK is set
				if cfg.envVars["CLAUDE_CODE_USE_BEDROCK"] != "1" {
					t.Error("CLAUDE_CODE_USE_BEDROCK should be '1' in bedrock mode")
				}
			} else {
				finalArgs = configureAnthropicDefaults(parsed, "dark")
			}

			hasSettings := false
			hasModelDefault := false
			for i, arg := range finalArgs {
				if arg == "--settings" {
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
