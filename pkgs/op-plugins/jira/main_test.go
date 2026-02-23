package main

import (
	"testing"

	"github.com/1Password/shell-plugins/sdk/plugintest"
)

func TestNeedsAuth(t *testing.T) {
	plugintest.TestNeedsAuth(t, jiraCLI().NeedsAuth, map[string]plugintest.NeedsAuthCase{
		"no args": {
			Args:              []string{},
			ExpectedNeedsAuth: false,
		},
		"with args": {
			Args:              []string{"issue", "list"},
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
