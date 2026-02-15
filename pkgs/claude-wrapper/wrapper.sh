# Flag dispatch
case "${1:-}" in
  --help|-h|--version|-v) exec claude-unwrapped "$@" ;;
  --bedrock)
    shift
    export CLAUDE_CODE_USE_BEDROCK=1
    ;;
esac

# Auto-detect: route to Bedrock when plan utilization is high
if [ -z "${CLAUDE_CODE_USE_BEDROCK:-}" ]; then
  _creds=$(${_SECURITY_CMD:-/usr/bin/security} find-generic-password \
    -s "Claude Code-credentials" -a "$USER" -w 2>/dev/null) || true
  _token=$(printf '%s' "${_creds:-}" \
    | jq -r '.claudeAiOauth.accessToken // empty' 2>/dev/null) || true

  if [ -n "${_token:-}" ]; then
    _usage=$(curl -s --max-time 2 \
      -H "Authorization: Bearer $_token" \
      -H "anthropic-beta: oauth-2025-04-20" \
      -H "User-Agent: claude-code" \
      "https://api.anthropic.com/api/oauth/usage" 2>/dev/null) || true
    _util=$(printf '%s' "${_usage:-}" \
      | jq -r '.five_hour.utilization // empty | round' 2>/dev/null) || true

    if [ -n "${_util:-}" ] && [ "${_util:-0}" -ge 98 ] 2>/dev/null; then
      export CLAUDE_CODE_USE_BEDROCK=1
    fi
  fi
fi

export _CLAUDE_SESSION=1

# Resolve 1Password secrets in a single biometric prompt
_op_secrets=$(op run \
  --no-masking \
  --env-file="$_ENV_TEMPLATE" \
  -- bash -c '
    printf "export CLAUDE_CODE_GITHUB_TOKEN=%q\n" "$CLAUDE_CODE_GITHUB_TOKEN"
    printf "export GH_TOKEN=%q\n" "$GH_TOKEN"
    printf "export JIRA_API_TOKEN=%q\n" "$JIRA_API_TOKEN"
  ') || exit 1
eval "$_op_secrets"

exec claude-unwrapped "$@"
