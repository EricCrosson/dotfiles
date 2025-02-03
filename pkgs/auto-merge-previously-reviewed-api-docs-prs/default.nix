{pkgs}:
pkgs.writeShellApplication {
  name = "auto-merge-previously-reviewed-api-docs-prs";
  runtimeInputs = with pkgs; [
    curl
    gh
    jq
  ];
  text = ''
    if [[ -z "''${GITHUB_TOKEN_PATH:-}" ]]; then
      echo "GITHUB_TOKEN_PATH must be set" >&2
      exit 1
    fi

    export GITHUB_TOKEN
    GITHUB_TOKEN="$(cat "$GITHUB_TOKEN_PATH")"
    if [[ -z "$GITHUB_TOKEN" ]]; then
      echo "Failed to read GitHub token from $GITHUB_TOKEN_PATH" >&2
      exit 1
    fi

    ${builtins.readFile ./auto-merge-previously-reviewed-api-docs-prs.sh}
  '';
}
