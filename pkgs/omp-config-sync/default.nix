{
  coreutils,
  jq,
  writeShellApplication,
  yj,
}:
writeShellApplication {
  name = "omp-config-sync";
  runtimeInputs = [
    coreutils
    jq
    yj
  ];
  text = ''
    if [[ $# -ne 2 ]]; then
      echo "usage: omp-config-sync BASE_YAML TARGET_YAML" >&2
      exit 2
    fi

    base_config=$1
    target_config=$2

    if [[ ! -r "$base_config" ]]; then
      echo "omp-config-sync: cannot read base config: $base_config" >&2
      exit 1
    fi

    target_dir=$(dirname -- "$target_config")
    mkdir -p -- "$target_dir"
    temp_dir=$(mktemp -d "$target_dir/.omp-config-sync.XXXXXX")
    trap 'rm -rf -- "$temp_dir"' EXIT

    yj -yj < "$base_config" > "$temp_dir/base.json"

    if [[ -e "$target_config" || -L "$target_config" ]]; then
      yj -yj < "$target_config" > "$temp_dir/current.json"
    else
      printf '{}\n' > "$temp_dir/current.json"
    fi

    jq --slurp '.[1] * .[0]' \
      "$temp_dir/base.json" "$temp_dir/current.json" > "$temp_dir/merged.json"

    yj -jy < "$temp_dir/merged.json" > "$temp_dir/config.yml"
    chmod 0600 "$temp_dir/config.yml"
    mv -f -- "$temp_dir/config.yml" "$target_config"
  '';
}
