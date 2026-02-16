package main

import (
	"os"
	"reflect"
	"testing"
)

func TestParseArgs_PositionIndependent(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		want    ParsedArgs
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
			name: "no bedrock",
			args: []string{"--chat"},
			want: ParsedArgs{
				explicitBedrock: false,
				hasModel:        false,
				filteredArgs:    []string{"--chat"},
			},
		},
		{
			name: "model before bedrock",
			args: []string{"--model", "opus", "--bedrock", "--chat"},
			want: ParsedArgs{
				explicitBedrock: true,
				hasModel:        true,
				filteredArgs:    []string{"--model", "opus", "--chat"},
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
				explicitBedrock:  false,
				hasModel:         false,
				hasHelpOrVersion: true,
				filteredArgs:     []string{"--help"},
			},
		},
		{
			name: "version flag with bedrock",
			args: []string{"--bedrock", "--version"},
			want: ParsedArgs{
				explicitBedrock:  true,
				hasModel:         false,
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

func TestReadConfig(t *testing.T) {
	// Save original env vars
	origProfile := os.Getenv("AWS_PROFILE")
	origRegion := os.Getenv("AWS_REGION")
	origThreshold := os.Getenv("BEDROCK_THRESHOLD")
	origWeeklyThreshold := os.Getenv("BEDROCK_WEEKLY_THRESHOLD")
	origTemplate := os.Getenv("ENV_TEMPLATE")

	// Set test values
	os.Setenv("AWS_PROFILE", "test-profile")
	os.Setenv("AWS_REGION", "us-west-2")
	os.Setenv("BEDROCK_THRESHOLD", "85")
	os.Setenv("BEDROCK_WEEKLY_THRESHOLD", "70")
	os.Setenv("ENV_TEMPLATE", "/path/to/template")

	// Restore original values after test
	defer func() {
		if origProfile != "" {
			os.Setenv("AWS_PROFILE", origProfile)
		} else {
			os.Unsetenv("AWS_PROFILE")
		}
		if origRegion != "" {
			os.Setenv("AWS_REGION", origRegion)
		} else {
			os.Unsetenv("AWS_REGION")
		}
		if origThreshold != "" {
			os.Setenv("BEDROCK_THRESHOLD", origThreshold)
		} else {
			os.Unsetenv("BEDROCK_THRESHOLD")
		}
		if origWeeklyThreshold != "" {
			os.Setenv("BEDROCK_WEEKLY_THRESHOLD", origWeeklyThreshold)
		} else {
			os.Unsetenv("BEDROCK_WEEKLY_THRESHOLD")
		}
		if origTemplate != "" {
			os.Setenv("ENV_TEMPLATE", origTemplate)
		} else {
			os.Unsetenv("ENV_TEMPLATE")
		}
	}()

	config := readConfig()

	if config.AWSProfile != "test-profile" {
		t.Errorf("AWSProfile = %v, want test-profile", config.AWSProfile)
	}
	if config.AWSRegion != "us-west-2" {
		t.Errorf("AWSRegion = %v, want us-west-2", config.AWSRegion)
	}
	if config.BedrockThreshold != 85 {
		t.Errorf("BedrockThreshold = %v, want 85", config.BedrockThreshold)
	}
	if config.WeeklyBedrockThreshold != 70 {
		t.Errorf("WeeklyBedrockThreshold = %v, want 70", config.WeeklyBedrockThreshold)
	}
	if config.EnvTemplate != "/path/to/template" {
		t.Errorf("EnvTemplate = %v, want /path/to/template", config.EnvTemplate)
	}
}

func TestReadConfig_Defaults(t *testing.T) {
	// Save original env vars
	origThreshold := os.Getenv("BEDROCK_THRESHOLD")
	origWeeklyThreshold := os.Getenv("BEDROCK_WEEKLY_THRESHOLD")

	// Unset thresholds to test defaults
	os.Unsetenv("BEDROCK_THRESHOLD")
	os.Unsetenv("BEDROCK_WEEKLY_THRESHOLD")

	// Restore original values after test
	defer func() {
		if origThreshold != "" {
			os.Setenv("BEDROCK_THRESHOLD", origThreshold)
		}
		if origWeeklyThreshold != "" {
			os.Setenv("BEDROCK_WEEKLY_THRESHOLD", origWeeklyThreshold)
		}
	}()

	config := readConfig()

	if config.BedrockThreshold != 80 {
		t.Errorf("BedrockThreshold = %v, want default of 80", config.BedrockThreshold)
	}
	if config.WeeklyBedrockThreshold != 65 {
		t.Errorf("WeeklyBedrockThreshold = %v, want default of 65", config.WeeklyBedrockThreshold)
	}
}

func TestGetEnvInt(t *testing.T) {
	tests := []struct {
		name         string
		envValue     string
		defaultValue int
		want         int
	}{
		{
			name:         "valid integer",
			envValue:     "42",
			defaultValue: 10,
			want:         42,
		},
		{
			name:         "empty string uses default",
			envValue:     "",
			defaultValue: 10,
			want:         10,
		},
		{
			name:         "invalid integer uses default",
			envValue:     "not-a-number",
			defaultValue: 10,
			want:         10,
		},
		{
			name:         "negative integer",
			envValue:     "-5",
			defaultValue: 10,
			want:         -5,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Set temporary env var
			testKey := "TEST_ENV_INT"
			if tt.envValue != "" {
				os.Setenv(testKey, tt.envValue)
				defer os.Unsetenv(testKey)
			}

			got := getEnvInt(testKey, tt.defaultValue)
			if got != tt.want {
				t.Errorf("getEnvInt() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParseOAuthUsage(t *testing.T) {
	tests := []struct {
		name string
		data []byte
		want *Utilization
	}{
		{
			name: "both fields present",
			data: []byte(`{"five_hour":{"utilization":72.4},"seven_day":{"utilization":55.8}}`),
			want: &Utilization{FiveHour: 72, SevenDay: 56},
		},
		{
			name: "only five_hour present",
			data: []byte(`{"five_hour":{"utilization":42.0}}`),
			want: &Utilization{FiveHour: 42, SevenDay: 0},
		},
		{
			name: "rounding up at 0.5",
			data: []byte(`{"five_hour":{"utilization":79.5},"seven_day":{"utilization":64.5}}`),
			want: &Utilization{FiveHour: 80, SevenDay: 65},
		},
		{
			name: "rounding down below 0.5",
			data: []byte(`{"five_hour":{"utilization":79.4},"seven_day":{"utilization":64.4}}`),
			want: &Utilization{FiveHour: 79, SevenDay: 64},
		},
		{
			name: "invalid JSON",
			data: []byte(`not json`),
			want: nil,
		},
		{
			name: "empty JSON object",
			data: []byte(`{}`),
			want: &Utilization{FiveHour: 0, SevenDay: 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseOAuthUsage(tt.data)
			if tt.want == nil {
				if got != nil {
					t.Errorf("parseOAuthUsage() = %v, want nil", got)
				}
				return
			}
			if got == nil {
				t.Fatalf("parseOAuthUsage() = nil, want %v", tt.want)
			}
			if got.FiveHour != tt.want.FiveHour {
				t.Errorf("FiveHour = %v, want %v", got.FiveHour, tt.want.FiveHour)
			}
			if got.SevenDay != tt.want.SevenDay {
				t.Errorf("SevenDay = %v, want %v", got.SevenDay, tt.want.SevenDay)
			}
		})
	}
}

func TestShouldUseBedrock(t *testing.T) {
	config := Config{
		BedrockThreshold:       80,
		WeeklyBedrockThreshold: 65,
	}

	tests := []struct {
		name        string
		utilization *Utilization
		want        bool
	}{
		{
			name:        "nil utilization",
			utilization: nil,
			want:        false,
		},
		{
			name:        "both below threshold",
			utilization: &Utilization{FiveHour: 50, SevenDay: 40},
			want:        false,
		},
		{
			name:        "five_hour at threshold",
			utilization: &Utilization{FiveHour: 80, SevenDay: 40},
			want:        true,
		},
		{
			name:        "seven_day at threshold",
			utilization: &Utilization{FiveHour: 50, SevenDay: 65},
			want:        true,
		},
		{
			name:        "both above threshold",
			utilization: &Utilization{FiveHour: 90, SevenDay: 75},
			want:        true,
		},
		{
			name:        "both just below threshold",
			utilization: &Utilization{FiveHour: 79, SevenDay: 64},
			want:        false,
		},
		{
			name:        "five_hour above seven_day below",
			utilization: &Utilization{FiveHour: 85, SevenDay: 30},
			want:        true,
		},
		{
			name:        "five_hour below seven_day above",
			utilization: &Utilization{FiveHour: 30, SevenDay: 70},
			want:        true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := shouldUseBedrock(tt.utilization, config)
			if got != tt.want {
				t.Errorf("shouldUseBedrock() = %v, want %v", got, tt.want)
			}
		})
	}
}
