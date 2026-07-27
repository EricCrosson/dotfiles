{
  coreutils,
  jq,
  writeShellApplication,
  yj,
}:
writeShellApplication {
  name = "codex-config-sync";
  runtimeInputs = [
    coreutils
    jq
    yj
  ];
  text = ''
    if [[ $# -ne 2 ]]; then
      echo "usage: codex-config-sync BASE_TOML TARGET_TOML" >&2
      exit 2
    fi

    base_config=$1
    target_config=$2

    if [[ ! -r "$base_config" ]]; then
      echo "codex-config-sync: cannot read base config: $base_config" >&2
      exit 1
    fi

    target_dir=$(dirname -- "$target_config")
    mkdir -p -- "$target_dir"
    temp_dir=$(mktemp -d "$target_dir/.codex-config-sync.XXXXXX")
    trap 'rm -rf -- "$temp_dir"' EXIT

    yj -tj < "$base_config" > "$temp_dir/base.json"

    if [[ -e "$target_config" || -L "$target_config" ]]; then
      yj -tj < "$target_config" > "$temp_dir/current.json"
    else
      printf '{}\n' > "$temp_dir/current.json"
    fi

    jq --slurp \
      '(if (.[1].projects? | type) == "object" then {projects: .[1].projects} else {} end) * .[0]' \
      "$temp_dir/base.json" \
      "$temp_dir/current.json" > "$temp_dir/merged.json"

    yj -jt -i < "$temp_dir/merged.json" > "$temp_dir/config.toml"
    chmod 0600 "$temp_dir/config.toml"
    mv -f -- "$temp_dir/config.toml" "$target_config"
  '';
}
