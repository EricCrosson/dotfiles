GITHUB_TOKEN="$(cat "$HOME/.config/sops-nix/secrets/github_token_bitgo" 2>/dev/null || echo "")"
export GITHUB_TOKEN
JIRA_API_TOKEN="$(cat "$HOME/.config/sops-nix/secrets/jira_token_bitgo" 2>/dev/null || echo "")"
export JIRA_API_TOKEN
export NIX_CONFIG="access-tokens = github.com=${GITHUB_TOKEN}"
