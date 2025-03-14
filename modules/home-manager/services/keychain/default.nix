{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.services.keychain;
in {
  options.services.keychain = {
    enable = mkEnableOption "SSH key management with keychain";

    keys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "SSH keys to add to the agent";
      example = literalExpression ''[ "$HOME/.ssh/id_rsa" "$HOME/.ssh/id_rsa_personal" ]'';
    };

    extraArgs = mkOption {
      type = types.listOf types.str;
      default = ["--agents" "ssh" "--inherit" "any" "--systemd"];
      description = "Extra arguments to pass to keychain";
    };

    logging = {
      stdout = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/keychain.log";
        description = "Path to stdout log file";
      };

      stderr = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/Library/Logs/keychain.error.log";
        description = "Path to stderr log file";
      };
    };
  };

  config = mkIf cfg.enable {
    launchd-with-logs.services.keychain = {
      command = "${pkgs.keychain}/bin/keychain";
      args = cfg.extraArgs ++ cfg.keys;
      runAtLoad = true;
      logging = {
        stdout = cfg.logging.stdout;
        stderr = cfg.logging.stderr;
      };
    };
  };
}
