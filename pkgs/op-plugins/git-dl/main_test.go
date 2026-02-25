package main

import (
	"testing"

	"github.com/1Password/shell-plugins/sdk/plugintest"
)

func TestNeedsAuth(t *testing.T) {
	plugintest.TestNeedsAuth(t, gitDlCLI().NeedsAuth, map[string]plugintest.NeedsAuthCase{
		"no args": {
			Args:              []string{},
			ExpectedNeedsAuth: true,
		},
		"with args": {
			Args:              []string{"bitgo/relay-ai-agent"},
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
