{pkgs}: let
  codex-config-sync = pkgs.callPackage ../pkgs/codex-config-sync {};
in
  pkgs.runCommand "codex-config-sync-test" {
    nativeBuildInputs = [
      codex-config-sync
      pkgs.coreutils
      pkgs.yq-go
    ];
  } ''
    set -eu

    work=$PWD/work
    mkdir -p "$work"

    cat > "$work/base.toml" <<'EOF'
    [mcp_servers.primary]
    command = "/nix/store/initial/bin/server"
    EOF

    codex-config-sync "$work/base.toml" "$work/config.toml"
    test -f "$work/config.toml"
    test ! -L "$work/config.toml"
    test -w "$work/config.toml"
    test "$(yq -p toml -r '.mcp_servers.primary.command' "$work/config.toml")" = "/nix/store/initial/bin/server"
    test "$(yq -p toml -r '.projects // "missing"' "$work/config.toml")" = "missing"

    cat > "$work/managed.toml" <<'EOF'
    model = "runtime-choice"

    [mcp_servers.old]
    command = "/nix/store/old/bin/server"

    [notice]
    hide_full_access_warning = true

    [projects."/repo/dynamic"]
    trust_level = "trusted"

    [projects."/repo/declarative"]
    trust_level = "trusted"
    EOF
    chmod 0444 "$work/managed.toml"
    rm "$work/config.toml"
    ln -s "$work/managed.toml" "$work/config.toml"

    cat > "$work/base.toml" <<'EOF'
    [mcp_servers.primary]
    command = "/nix/store/updated/bin/server"

    [projects."/repo/declarative"]
    trust_level = "untrusted"
    EOF

    codex-config-sync "$work/base.toml" "$work/config.toml"
    test -f "$work/config.toml"
    test ! -L "$work/config.toml"
    test -w "$work/config.toml"
    test "$(yq -p toml -r '.mcp_servers.primary.command' "$work/config.toml")" = "/nix/store/updated/bin/server"
    test "$(yq -p toml -r '.mcp_servers.old // "missing"' "$work/config.toml")" = "missing"
    test "$(yq -p toml -r '.model // "missing"' "$work/config.toml")" = "missing"
    test "$(yq -p toml -r '.notice // "missing"' "$work/config.toml")" = "missing"
    test "$(yq -p toml -r '.projects["/repo/dynamic"].trust_level' "$work/config.toml")" = "trusted"
    test "$(yq -p toml -r '.projects["/repo/declarative"].trust_level' "$work/config.toml")" = "untrusted"

    cat > "$work/base.toml" <<'EOF'
    [mcp_servers.primary]
    command = "/nix/store/rebuilt/bin/server"
    EOF
    codex-config-sync "$work/base.toml" "$work/config.toml"
    test "$(yq -p toml -r '.mcp_servers.primary.command' "$work/config.toml")" = "/nix/store/rebuilt/bin/server"
    test "$(yq -p toml -r '.projects["/repo/dynamic"].trust_level' "$work/config.toml")" = "trusted"
    test "$(yq -p toml -r '.projects["/repo/declarative"].trust_level' "$work/config.toml")" = "untrusted"

    printf 'not = valid = toml\n' > "$work/malformed.toml"
    cp "$work/malformed.toml" "$work/malformed.before"
    if codex-config-sync "$work/base.toml" "$work/malformed.toml"; then
      echo "expected malformed target to fail" >&2
      exit 1
    fi
    cmp "$work/malformed.before" "$work/malformed.toml"

    touch "$out"
  ''
