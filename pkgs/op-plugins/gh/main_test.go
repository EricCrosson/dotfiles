package main

import (
	"testing"

	"github.com/1Password/shell-plugins/sdk/plugintest"
)

func TestNeedsAuth(t *testing.T) {
	plugintest.TestNeedsAuth(t, ghCLI().NeedsAuth, map[string]plugintest.NeedsAuthCase{
		"auth logout": {
			Args:              []string{"auth", "logout"},
			ExpectedNeedsAuth: false,
		},
		"auth status": {
			Args:              []string{"auth", "status"},
			ExpectedNeedsAuth: false,
		},
		"pr list": {
			Args:              []string{"pr", "list"},
			ExpectedNeedsAuth: true,
		},
		"api user": {
			Args:              []string{"api", "user"},
			ExpectedNeedsAuth: true,
		},
		"help flag": {
			Args:              []string{"--help"},
			ExpectedNeedsAuth: false,
		},
		"version flag": {
			Args:              []string{"--version"},
			ExpectedNeedsAuth: false,
		},
	})
}
