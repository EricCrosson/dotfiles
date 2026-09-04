{pkgs}: let
  omp-mcp-sync = pkgs.callPackage ../pkgs/omp-mcp-sync {};
in
  pkgs.runCommand "omp-mcp-sync-test" {
    nativeBuildInputs = [
      omp-mcp-sync
      pkgs.coreutils
      pkgs.diffutils
      pkgs.jq
    ];
  } ''
    set -eu

    work=$PWD/work
    mkdir -p "$work"

    cat > "$work/base.json" <<'EOF'
    {
      "$schema": "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json",
      "mcpServers": {
        "slack": {
          "type": "http",
          "url": "https://mcp.slack.com/mcp",
          "oauth": {"clientId": "1601185624273.8899143856786", "callbackPort": 3118}
        }
      }
    }
    EOF

    omp-mcp-sync "$work/base.json" "$work/mcp.json"
    test -f "$work/mcp.json"
    test ! -L "$work/mcp.json"
    test -w "$work/mcp.json"
    test "$(stat -c %a "$work/mcp.json")" = 600
    test "$(jq -r '.mcpServers.slack.url' "$work/mcp.json")" = "https://mcp.slack.com/mcp"
    test "$(jq -r '."$schema"' "$work/mcp.json")" = "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json"

    cat > "$work/managed.json" <<'EOF'
    {
      "$schema": "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json",
      "mcpServers": {
        "slack": {
          "type": "http",
          "url": "https://hand-written.example/mcp",
          "oauth": {"clientId": "noob-client", "callbackPort": 9999}
        },
        "ghost": {"type": "http", "url": "https://ghost.example/mcp"}
      },
      "disabledServers": ["context7"],
      "enabledServers": ["linear"]
    }
    EOF
    chmod 0444 "$work/managed.json"
    rm "$work/mcp.json"
    ln -s "$work/managed.json" "$work/mcp.json"

    cat > "$work/base.json" <<'EOF'
    {
      "$schema": "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json",
      "mcpServers": {
        "slack": {
          "type": "http",
          "url": "https://mcp.slack.com/mcp",
          "oauth": {"clientId": "1601185624273.8899143856786", "callbackPort": 3118}
        }
      }
    }
    EOF

    omp-mcp-sync "$work/base.json" "$work/mcp.json"
    test -f "$work/mcp.json"
    test ! -L "$work/mcp.json"
    test -w "$work/mcp.json"
    test "$(stat -c %a "$work/mcp.json")" = 600
    merged=$(jq . "$work/mcp.json")
    test "$(printf '%s' "$merged" | jq -r '.mcpServers.slack.url')" = "https://mcp.slack.com/mcp"
    test "$(printf '%s' "$merged" | jq -r '.mcpServers.slack.oauth.clientId')" = "1601185624273.8899143856786"
    test "$(printf '%s' "$merged" | jq -r '.mcpServers.slack.oauth.callbackPort')" = 3118
    test "$(printf '%s' "$merged" | jq -r '.mcpServers.ghost // "missing"')" = missing
    test "$(printf '%s' "$merged" | jq -r '.disabledServers | type')" = array
    test "$(printf '%s' "$merged" | jq -c '.disabledServers')" = '["context7"]'
    test "$(printf '%s' "$merged" | jq -r '.enabledServers | type')" = array
    test "$(printf '%s' "$merged" | jq -c '.enabledServers')" = '["linear"]'
    test "$(printf '%s' "$merged" | jq -r '."$schema"')" = "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json"
    if cmp -s "$work/managed.json" "$work/mcp.json"; then
      echo "expected symlink source to remain untouched" >&2
      exit 1
    fi
    printf '{"mcpServers": {}}\n' > "$work/mcp.json"
    omp-mcp-sync "$work/base.json" "$work/mcp.json"
    test "$(jq -r '.disabledServers // "missing"' "$work/mcp.json")" = missing
    test "$(jq -r '.enabledServers // "missing"' "$work/mcp.json")" = missing

    printf 'not valid json\n' > "$work/malformed.json"
    cp "$work/malformed.json" "$work/malformed.before"
    if omp-mcp-sync "$work/base.json" "$work/malformed.json"; then
      echo "expected malformed target to fail" >&2
      exit 1
    fi
    cmp "$work/malformed.before" "$work/malformed.json"

    printf 'null\n' > "$work/nonobject.json"
    cp "$work/nonobject.json" "$work/nonobject.before"
    if omp-mcp-sync "$work/base.json" "$work/nonobject.json"; then
      echo "expected non-object target to fail" >&2
      exit 1
    fi
    cmp "$work/nonobject.before" "$work/nonobject.json"

    omp-mcp-sync "$work/base.json" "$work/mcp.json"
    cp "$work/mcp.json" "$work/mcp.first"
    omp-mcp-sync "$work/base.json" "$work/mcp.json"
    cmp "$work/mcp.first" "$work/mcp.json"

    touch "$out"
  ''
