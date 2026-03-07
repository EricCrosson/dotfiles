{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  drv = pkgs.writeShellApplication {
    name = "claude-format-on-edit";
    runtimeInputs = [pkgs.jq pkgs.alejandra pkgs.nodePackages.prettier];
    text = builtins.readFile ../claude/hooks/format-on-edit.sh;
  };

  script = builtins.readFile "${drv}/bin/claude-format-on-edit";

  # === Contract assertions ===

  # format-on-edit.sh content must be embedded
  test-alejandra-invocation = assert assertContains "alejandra-call" script "alejandra --quiet"; true;

  test-prettier-invocation = assert assertContains "prettier-call" script "prettier --write"; true;

  # All three tools must be on PATH via runtimeInputs (match package names in store paths)
  test-jq-on-path = assert assertContains "jq-in-path" script "jq-"; true;

  test-alejandra-on-path = assert assertContains "alejandra-in-path" script "alejandra-"; true;

  test-prettier-on-path = assert assertContains "prettier-in-path" script "prettier-"; true;
in
  assert test-alejandra-invocation;
  assert test-prettier-invocation;
  assert test-jq-on-path;
  assert test-alejandra-on-path;
  assert test-prettier-on-path; "all tests passed"
