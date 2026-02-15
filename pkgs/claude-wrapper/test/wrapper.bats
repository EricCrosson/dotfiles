#!/usr/bin/env bats

bats_load_library bats-support
bats_load_library bats-assert

setup() {
  MOCK_DIR="$(mktemp -d)"
  export PATH="$MOCK_DIR:$PATH"
  export _STATE_DUMP="$MOCK_DIR/state_dump"

  # Mock: claude-unwrapped — dumps sorted env vars and args to $_STATE_DUMP
  cat > "$MOCK_DIR/claude-unwrapped" <<'MOCK'
#!/usr/bin/env bash
{
  printf 'ARGS=%s\n' "$*"
  printf 'AWS_PROFILE=%s\n' "${AWS_PROFILE:-}"
  printf 'AWS_REGION=%s\n' "${AWS_REGION:-}"
  printf 'CLAUDE_CODE_GITHUB_TOKEN=%s\n' "${CLAUDE_CODE_GITHUB_TOKEN:-}"
  printf 'CLAUDE_CODE_USE_BEDROCK=%s\n' "${CLAUDE_CODE_USE_BEDROCK:-}"
  printf 'GH_TOKEN=%s\n' "${GH_TOKEN:-}"
  printf 'JIRA_API_TOKEN=%s\n' "${JIRA_API_TOKEN:-}"
  printf '_CLAUDE_SESSION=%s\n' "${_CLAUDE_SESSION:-}"
} | sort > "$_STATE_DUMP"
MOCK
  chmod +x "$MOCK_DIR/claude-unwrapped"

  # Mock: op — prints export statements with synthetic tokens
  cat > "$MOCK_DIR/op" <<'MOCK'
#!/usr/bin/env bash
printf 'export CLAUDE_CODE_GITHUB_TOKEN=%q\n' "test-github-token"
printf 'export GH_TOKEN=%q\n' "test-github-token"
printf 'export JIRA_API_TOKEN=%q\n' "test-jira-token"
MOCK
  chmod +x "$MOCK_DIR/op"

  # Mock: security — default: fail (no keychain creds)
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
exit 1
MOCK
  chmod +x "$MOCK_DIR/security"

  # Mock: curl — default: low utilization
  cat > "$MOCK_DIR/curl" <<'MOCK'
#!/usr/bin/env bash
printf '{"five_hour":{"utilization":42}}'
MOCK
  chmod +x "$MOCK_DIR/curl"

  # Simulated nix-injected preamble variables
  export AWS_PROFILE=test-profile
  export AWS_REGION=us-west-2
  _ENV_TEMPLATE="$MOCK_DIR/env_template"
  export _ENV_TEMPLATE
  touch "$_ENV_TEMPLATE"
  export _BEDROCK_THRESHOLD=80
  export _SECURITY_CMD=security
}

teardown() {
  rm -rf "$MOCK_DIR"
}

assert_snapshot() {
  local snapshot="snapshots/$1.snapshot"
  assert [ -f "$snapshot" ]
  diff -u "$snapshot" "$_STATE_DUMP"
}

# -------------------------------------------------------------------
# Test cases
# -------------------------------------------------------------------

@test "--help passthrough: exec immediately, no session, no secrets" {
  # claude-unwrapped mock already on PATH; --help triggers exec before
  # _CLAUDE_SESSION or secrets are set
  run bash wrapper.sh --help
  assert_success
  assert_snapshot help
}

@test "-v passthrough: exec immediately, no session, no secrets" {
  run bash wrapper.sh -v
  assert_success
  assert_snapshot version
}

@test "--bedrock flag: explicit bedrock, skips auto-detect" {
  run bash wrapper.sh --bedrock --model opus
  assert_success
  assert_snapshot bedrock-flag
  refute_output --partial "routing to Bedrock"
}

@test "low utilization: no bedrock, no warning" {
  # security mock returns valid OAuth creds
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
printf '{"claudeAiOauth":{"accessToken":"fake-token-abc"}}'
MOCK
  chmod +x "$MOCK_DIR/security"

  run bash wrapper.sh --chat
  assert_success
  assert_snapshot low-utilization
  refute_output --partial "routing to Bedrock"
}

@test "high utilization: bedrock enabled, warning on stderr" {
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
printf '{"claudeAiOauth":{"accessToken":"fake-token-abc"}}'
MOCK
  chmod +x "$MOCK_DIR/security"

  cat > "$MOCK_DIR/curl" <<'MOCK'
#!/usr/bin/env bash
printf '{"five_hour":{"utilization":99}}'
MOCK
  chmod +x "$MOCK_DIR/curl"

  run bash wrapper.sh --chat
  assert_success
  assert_snapshot high-utilization
}

@test "boundary 80%: triggers bedrock" {
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
printf '{"claudeAiOauth":{"accessToken":"fake-token-abc"}}'
MOCK
  chmod +x "$MOCK_DIR/security"

  cat > "$MOCK_DIR/curl" <<'MOCK'
#!/usr/bin/env bash
printf '{"five_hour":{"utilization":80}}'
MOCK
  chmod +x "$MOCK_DIR/curl"

  run bash wrapper.sh --chat
  assert_success
  assert_snapshot boundary-80
}

@test "no keychain creds: falls through to plan check" {
  # default security mock exits 1
  run bash wrapper.sh --chat
  assert_success
  assert_snapshot no-keychain
}

@test "API failure: curl exits 1, falls through" {
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
printf '{"claudeAiOauth":{"accessToken":"fake-token-abc"}}'
MOCK
  chmod +x "$MOCK_DIR/security"

  cat > "$MOCK_DIR/curl" <<'MOCK'
#!/usr/bin/env bash
exit 1
MOCK
  chmod +x "$MOCK_DIR/curl"

  run bash wrapper.sh --chat
  assert_success
  assert_snapshot api-failure
}

@test "malformed API response: falls through" {
  cat > "$MOCK_DIR/security" <<'MOCK'
#!/usr/bin/env bash
printf '{"claudeAiOauth":{"accessToken":"fake-token-abc"}}'
MOCK
  chmod +x "$MOCK_DIR/security"

  cat > "$MOCK_DIR/curl" <<'MOCK'
#!/usr/bin/env bash
printf 'this is not json'
MOCK
  chmod +x "$MOCK_DIR/curl"

  run bash wrapper.sh --chat
  assert_success
  assert_snapshot malformed-response
}

@test "pre-set CLAUDE_CODE_USE_BEDROCK: auto-detect skipped" {
  export CLAUDE_CODE_USE_BEDROCK=1
  run bash wrapper.sh --chat
  assert_success
  assert_snapshot preset-bedrock
  refute_output --partial "routing to Bedrock"
}
