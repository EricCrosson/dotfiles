{
  coreutils,
  jq,
  writeShellApplication,
}:
writeShellApplication {
  name = "omp-mcp-sync";
  runtimeInputs = [
    coreutils
    jq
  ];
  text = ''
    if [[ $# -ne 2 ]]; then
      echo "usage: omp-mcp-sync BASE_JSON TARGET_JSON" >&2
      exit 2
    fi

    base_config=$1
    target_config=$2

    if [[ ! -r "$base_config" ]]; then
      echo "omp-mcp-sync: cannot read base config: $base_config" >&2
      exit 1
    fi

    target_dir=$(dirname -- "$target_config")
    # omp expects its agent dir to be 0700; don't fabricate it looser than that.
    install -d -m 0700 -- "$target_dir"
    temp_dir=$(mktemp -d "$target_dir/.omp-mcp-sync.XXXXXX")

    jq . < "$base_config" > "$temp_dir/base.json"

    if [[ -e "$target_config" || -L "$target_config" ]]; then
      jq . < "$target_config" > "$temp_dir/current.json"
    else
      printf '{}\n' > "$temp_dir/current.json"
    fi

    jq --slurp '
      (if (.[1] | type) == "object"
       then .[1]
       else error("omp-mcp-sync: target must be a JSON object")
       end) as $cur
      | (if ($cur.disabledServers? | type) == "array" then {disabledServers: $cur.disabledServers} else {} end)
        * (if ($cur.enabledServers? | type) == "array" then {enabledServers: $cur.enabledServers} else {} end)
        * .[0]' \
      "$temp_dir/base.json" "$temp_dir/current.json" > "$temp_dir/merged.json"

    chmod 0600 "$temp_dir/merged.json"
    mv -f -- "$temp_dir/merged.json" "$target_config"
  '';
}
