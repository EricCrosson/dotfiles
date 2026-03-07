{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  claudeNotificationIcon = ../claude/assets/claude-icon.png;

  drv = pkgs.writeShellApplication {
    name = "claude-notification";
    runtimeInputs = [pkgs.jq pkgs.terminal-notifier];
    text =
      ''
        export CLAUDE_NOTIFICATION_ICON=${claudeNotificationIcon}
      ''
      + builtins.readFile ../claude/hooks/notification.sh;
  };

  script = builtins.readFile "${drv}/bin/claude-notification";

  # === Contract assertions ===

  # The icon path must be injected into the script
  test-icon = assert assertContains "icon-export" script "CLAUDE_NOTIFICATION_ICON="; true;

  # notification.sh content must be embedded (not just sourced)
  test-notification-content = assert assertContains "terminal-notifier-call" script "terminal-notifier";
  assert assertContains "jq-usage" script "jq -r"; true;

  # jq must be on PATH via runtimeInputs (match package name in store path)
  test-jq-on-path = assert assertContains "jq-in-path" script "jq-"; true;

  # terminal-notifier must be on PATH via runtimeInputs
  test-terminal-notifier-on-path = assert assertContains "terminal-notifier-in-path" script "terminal-notifier-"; true;
in
  assert test-icon;
  assert test-notification-content;
  assert test-jq-on-path;
  assert test-terminal-notifier-on-path; "all tests passed"
