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
		{
			name: "anthropic flag",
			args: []string{"--anthropic", "--chat"},
			want: ParsedArgs{
				explicitAnthropic: true,
				filteredArgs:      []string{"--chat"},
			},
		},
		{
			name: "anthropic after other flags",
			args: []string{"--chat", "--anthropic"},
			want: ParsedArgs{
				explicitAnthropic: true,
				filteredArgs:      []string{"--chat"},
			},
		},
		{
			name: "both bedrock and anthropic flags",
			args: []string{"--bedrock", "--anthropic", "--chat"},
			want: ParsedArgs{
				explicitBedrock:   true,
				explicitAnthropic: true,
				filteredArgs:      []string{"--chat"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseArgs(tt.args)

			if got.explicitBedrock != tt.want.explicitBedrock {
				t.Errorf("explicitBedrock = %v, want %v", got.explicitBedrock, tt.want.explicitBedrock)
			}
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
					"--model", "opusplan[1m]",
					"--settings", `{"availableModels":["opus","sonnet","haiku"]}`,
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
					"--settings", `{"availableModels":["opus","sonnet","haiku"]}`,
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
			cfg, err := configureBedrock(tt.args, mockGetenv(tt.env), mockReadFile(tt.files))

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

func TestResolveBackend(t *testing.T) {
	tests := []struct {
		name        string
		args        ParsedArgs
		envDefault  string
		wantBackend string
		wantErr     string
	}{
		{
			name:        "explicit bedrock overrides anthropic default",
			args:        ParsedArgs{explicitBedrock: true},
			envDefault:  "anthropic",
			wantBackend: "bedrock",
		},
		{
			name:        "explicit anthropic overrides bedrock default",
			args:        ParsedArgs{explicitAnthropic: true},
			envDefault:  "bedrock",
			wantBackend: "anthropic",
		},
		{
			name:        "explicit bedrock is no-op when default is bedrock",
			args:        ParsedArgs{explicitBedrock: true},
			envDefault:  "bedrock",
			wantBackend: "bedrock",
		},
		{
			name:        "explicit anthropic is no-op when default is anthropic",
			args:        ParsedArgs{explicitAnthropic: true},
			envDefault:  "anthropic",
			wantBackend: "anthropic",
		},
		{
			name:       "conflicting flags produce error",
			args:       ParsedArgs{explicitBedrock: true, explicitAnthropic: true},
			envDefault: "anthropic",
			wantErr:    "conflicting",
		},
		{
			name:        "no flag uses env default (anthropic)",
			args:        ParsedArgs{},
			envDefault:  "anthropic",
			wantBackend: "anthropic",
		},
		{
			name:        "no flag uses env default (bedrock)",
			args:        ParsedArgs{},
			envDefault:  "bedrock",
			wantBackend: "bedrock",
		},
		{
			name:        "empty env defaults to anthropic",
			args:        ParsedArgs{},
			envDefault:  "",
			wantBackend: "anthropic",
		},
		{
			name:       "invalid env value produces error",
			args:       ParsedArgs{},
			envDefault: "gcp-vertex",
			wantErr:    "invalid",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			getenv := func(key string) string {
				if key == "_CLAUDE_DEFAULT_BACKEND" {
					return tt.envDefault
				}
				return ""
			}
			got, err := resolveBackend(tt.args, getenv)
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
			if got != tt.wantBackend {
				t.Errorf("resolveBackend() = %q, want %q", got, tt.wantBackend)
			}
		})
	}
}

func TestConfigureAnthropicDefaults(t *testing.T) {
	tests := []struct {
		name     string
		args     ParsedArgs
		wantArgs []string
	}{
		{
			name:     "no model specified - injects opusplan[1m] default",
			args:     ParsedArgs{filteredArgs: []string{"--chat"}},
			wantArgs: []string{"--chat", "--model", "opusplan[1m]"},
		},
		{
			name:     "explicit model preserved",
			args:     ParsedArgs{hasModel: true, filteredArgs: []string{"--model", "sonnet", "--chat"}},
			wantArgs: []string{"--model", "sonnet", "--chat"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := configureAnthropicDefaults(tt.args)
			if !reflect.DeepEqual(got, tt.wantArgs) {
				t.Errorf("configureAnthropicDefaults() = %v, want %v", got, tt.wantArgs)
			}
		})
	}
}

func TestConfigurePlugins(t *testing.T) {
	tests := []struct {
		name string
		env  string
		want []string
	}{
		{
			name: "empty env returns nil",
			env:  "",
			want: nil,
		},
		{
			name: "single path",
			env:  "/nix/store/abc-context-mode",
			want: []string{"--plugin-dir", "/nix/store/abc-context-mode"},
		},
		{
			name: "multiple colon-separated paths",
			env:  "/path/a:/path/b:/path/c",
			want: []string{"--plugin-dir", "/path/a", "--plugin-dir", "/path/b", "--plugin-dir", "/path/c"},
		},
		{
			name: "trailing colon ignored",
			env:  "/path/a:",
			want: []string{"--plugin-dir", "/path/a"},
		},
		{
			name: "leading colon ignored",
			env:  ":/path/a",
			want: []string{"--plugin-dir", "/path/a"},
		},
		{
			name: "empty segments ignored",
			env:  "/path/a::/path/b",
			want: []string{"--plugin-dir", "/path/a", "--plugin-dir", "/path/b"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			getenv := func(key string) string {
				if key == "_CLAUDE_PLUGIN_DIRS" {
					return tt.env
				}
				return ""
			}
			got := configurePlugins(getenv)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("configurePlugins() = %v, want %v", got, tt.want)
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
	if args.explicitAnthropic {
		t.Fatal("default path should not have explicitAnthropic set")
	}

	// Simulate main() logic: no flags means resolveBackend reads env default,
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
		defaultBackend     string
		wantSettingsInArgs bool
		wantModelDefault   bool
	}{
		{
			name:               "anthropic default (no flags) - injects opus model",
			args:               []string{"--chat"},
			defaultBackend:     "anthropic",
			wantSettingsInArgs: false,
			wantModelDefault:   true,
		},
		{
			name:               "anthropic with explicit model - preserves model",
			args:               []string{"--model", "sonnet", "--chat"},
			defaultBackend:     "anthropic",
			wantSettingsInArgs: false,
			wantModelDefault:   false,
		},
		{
			name:               "bedrock via explicit flag",
			args:               []string{"--bedrock", "--chat"},
			defaultBackend:     "anthropic",
			wantSettingsInArgs: true,
			wantModelDefault:   true,
		},
		{
			name:               "bedrock with explicit model skips default",
			args:               []string{"--bedrock", "--model", "haiku", "--chat"},
			defaultBackend:     "anthropic",
			wantSettingsInArgs: true,
			wantModelDefault:   false,
		},
		{
			name:               "bedrock via env default (no flag)",
			args:               []string{"--chat"},
			defaultBackend:     "bedrock",
			wantSettingsInArgs: true,
			wantModelDefault:   true,
		},
		{
			name:               "anthropic via explicit flag overriding bedrock default",
			args:               []string{"--anthropic", "--chat"},
			defaultBackend:     "bedrock",
			wantSettingsInArgs: false,
			wantModelDefault:   true,
		},
	}

	files := map[string]string{
		"/sops/opus":   "arn:opus\n",
		"/sops/sonnet": "arn:sonnet\n",
		"/sops/haiku":  "arn:haiku\n",
	}
	readFile := func(path string) (string, error) {
		if v, ok := files[path]; ok {
			return v, nil
		}
		return "", &fileNotFoundError{path: path}
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			env := map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
				"_CLAUDE_BEDROCK_PROFILE":     "bitgo-ai",
				"_CLAUDE_BEDROCK_REGION":      "us-west-2",
				"_CLAUDE_DEFAULT_BACKEND":     tt.defaultBackend,
			}
			getenv := func(key string) string { return env[key] }

			parsed := parseArgs(tt.args)

			backend, err := resolveBackend(parsed, getenv)
			if err != nil {
				t.Fatalf("unexpected resolveBackend error: %v", err)
			}

			var finalArgs []string
			if backend == "bedrock" {
				cfg, err := configureBedrock(parsed, getenv, readFile)
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				finalArgs = append(parsed.filteredArgs, cfg.extraArgs...)

				// Verify CLAUDE_CODE_USE_BEDROCK is set
				if cfg.envVars["CLAUDE_CODE_USE_BEDROCK"] != "1" {
					t.Error("CLAUDE_CODE_USE_BEDROCK should be '1' in bedrock mode")
				}
			} else {
				finalArgs = configureAnthropicDefaults(parsed)
			}

			hasSettings := false
			hasModelDefault := false
			for i, arg := range finalArgs {
				if arg == "--settings" {
					hasSettings = true
				}
				if arg == "--model" && i+1 < len(finalArgs) && finalArgs[i+1] == "opusplan[1m]" && !parsed.hasModel {
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

func TestPlugins_Integration(t *testing.T) {
	tests := []struct {
		name           string
		args           []string
		defaultBackend string
		pluginDirs     string
		wantPluginArgs []string
	}{
		{
			name:           "plugins injected alongside bedrock args",
			args:           []string{"--bedrock", "--chat"},
			defaultBackend: "anthropic",
			pluginDirs:     "/nix/store/abc-plugin-a:/nix/store/def-plugin-b",
			wantPluginArgs: []string{"--plugin-dir", "/nix/store/abc-plugin-a", "--plugin-dir", "/nix/store/def-plugin-b"},
		},
		{
			name:           "plugins injected alongside anthropic args",
			args:           []string{"--chat"},
			defaultBackend: "anthropic",
			pluginDirs:     "/nix/store/abc-plugin-a",
			wantPluginArgs: []string{"--plugin-dir", "/nix/store/abc-plugin-a"},
		},
		{
			name:           "no plugins when env is empty",
			args:           []string{"--chat"},
			defaultBackend: "anthropic",
			pluginDirs:     "",
			wantPluginArgs: nil,
		},
	}

	files := map[string]string{
		"/sops/opus":   "arn:opus\n",
		"/sops/sonnet": "arn:sonnet\n",
		"/sops/haiku":  "arn:haiku\n",
	}
	readFile := func(path string) (string, error) {
		if v, ok := files[path]; ok {
			return v, nil
		}
		return "", &fileNotFoundError{path: path}
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			env := map[string]string{
				"_CLAUDE_BEDROCK_OPUS_FILE":   "/sops/opus",
				"_CLAUDE_BEDROCK_SONNET_FILE": "/sops/sonnet",
				"_CLAUDE_BEDROCK_HAIKU_FILE":  "/sops/haiku",
				"_CLAUDE_BEDROCK_PROFILE":     "bitgo-ai",
				"_CLAUDE_BEDROCK_REGION":      "us-west-2",
				"_CLAUDE_DEFAULT_BACKEND":     tt.defaultBackend,
				"_CLAUDE_PLUGIN_DIRS":         tt.pluginDirs,
			}
			getenv := func(key string) string { return env[key] }

			parsed := parseArgs(tt.args)

			backend, err := resolveBackend(parsed, getenv)
			if err != nil {
				t.Fatalf("unexpected resolveBackend error: %v", err)
			}

			var finalArgs []string
			if backend == "bedrock" {
				cfg, err := configureBedrock(parsed, getenv, readFile)
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				finalArgs = append(parsed.filteredArgs, cfg.extraArgs...)
			} else {
				finalArgs = configureAnthropicDefaults(parsed)
			}

			// Inject plugins (mirrors main() wiring)
			finalArgs = append(finalArgs, configurePlugins(getenv)...)

			// Verify plugin args appear in the final args
			if tt.wantPluginArgs == nil {
				for _, arg := range finalArgs {
					if arg == "--plugin-dir" {
						t.Errorf("unexpected --plugin-dir in args: %v", finalArgs)
					}
				}
			} else {
				found := false
				for i := range finalArgs {
					if i+len(tt.wantPluginArgs) <= len(finalArgs) {
						if reflect.DeepEqual(finalArgs[i:i+len(tt.wantPluginArgs)], tt.wantPluginArgs) {
							found = true
							break
						}
					}
				}
				if !found {
					t.Errorf("expected plugin args %v in final args %v", tt.wantPluginArgs, finalArgs)
				}
			}
		})
	}
}
