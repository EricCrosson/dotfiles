{pkgs}: let
  omp-config-sync = pkgs.callPackage ../pkgs/omp-config-sync {};
in
  pkgs.runCommand "omp-config-sync-test" {
    nativeBuildInputs = [
      omp-config-sync
      pkgs.coreutils
      pkgs.jq
      pkgs.yj
    ];
  } ''
    set -eu

    work=$PWD/work
    mkdir -p "$work"

    cat > "$work/base.yml" <<'EOF'
    advisor:
      enabled: true
    modelProviderOrder:
      - openrouter
    EOF

    omp-config-sync "$work/base.yml" "$work/config.yml"
    test -f "$work/config.yml"
    test ! -L "$work/config.yml"
    test -w "$work/config.yml"
    test "$(yj -yj < "$work/config.yml" | jq -r '.advisor.enabled')" = true
    test "$(yj -yj < "$work/config.yml" | jq -r '.theme // "missing"')" = missing

    cat > "$work/managed.yml" <<'EOF'
    theme:
      dark: titanium
    defaultThinkingLevel: auto
    EOF
    chmod 0444 "$work/managed.yml"
    rm "$work/config.yml"
    ln -s "$work/managed.yml" "$work/config.yml"

    cat > "$work/base.yml" <<'EOF'
    advisor:
      enabled: false
    modelProviderOrder:
      - anthropic
    EOF

    omp-config-sync "$work/base.yml" "$work/config.yml"
    test -f "$work/config.yml"
    test ! -L "$work/config.yml"
    test -w "$work/config.yml"
    merged=$(yj -yj < "$work/config.yml")
    test "$(printf '%s' "$merged" | jq -r '.theme.dark')" = titanium
    test "$(printf '%s' "$merged" | jq -r '.defaultThinkingLevel')" = auto
    test "$(printf '%s' "$merged" | jq -r '.advisor.enabled')" = false
    test "$(printf '%s' "$merged" | jq -r '.modelProviderOrder[0]')" = anthropic

    printf 'theme: [not valid\n' > "$work/malformed.yml"
    cp "$work/malformed.yml" "$work/malformed.before"
    if omp-config-sync "$work/base.yml" "$work/malformed.yml"; then
      echo "expected malformed target to fail" >&2
      exit 1
    fi
    cmp "$work/malformed.before" "$work/malformed.yml"

    touch "$out"
  ''
