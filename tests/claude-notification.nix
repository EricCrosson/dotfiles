{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  drv = pkgs.writeShellApplication {
    name = "claude-notification";
    runtimeInputs = [pkgs.jq];
    text = builtins.readFile ../claude/hooks/notification.sh;
  };

  script = builtins.readFile "${drv}/bin/claude-notification";

  # === Contract assertions ===

  # notification.sh content must be embedded (not just sourced)
  test-notification-content = assert assertContains "osascript-call" script "osascript";
  assert assertContains "jq-usage" script "jq -r"; true;

  # jq must be on PATH via runtimeInputs (match package name in store path)
  test-jq-on-path = assert assertContains "jq-in-path" script "jq-"; true;
in
  assert test-notification-content;
  assert test-jq-on-path; "all tests passed"
