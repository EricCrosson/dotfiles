{pkgs}:
pkgs.writeShellApplication {
  name = "auto-merge-prs-that-only-bump-openapi-spec-version-numbers";
  runtimeInputs = with pkgs; [
    curl
    gh
    jq
    yq-go
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

    ${builtins.readFile ./auto-merge-prs-that-only-bump-openapi-spec-version-numbers.sh}
  '';
}
