{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.bitgo.ssh;
in {
  options.bitgo.ssh = {
    enable = mkEnableOption "BitGo SSH configuration for work";

    socketDir = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/.ssh/sockets";
      description = "Directory for SSH control master sockets";
    };

    gpgAuthSock = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/.gnupg/S.gpg-agent.ssh";
      description = "Path to GPG agent SSH socket for authentication";
    };

    github = {
      controlMaster = mkOption {
        type = types.str;
        default = "auto";
        description = "SSH ControlMaster setting for GitHub";
      };

      controlPersist = mkOption {
        type = types.str;
        default = "30m";
        description = "How long to keep GitHub SSH connections alive";
      };

      serverAliveInterval = mkOption {
        type = types.int;
        default = 120;
        description = "Interval for SSH keepalive packets to GitHub";
      };
    };
  };

  config = mkIf cfg.enable {
    home.activation.createSshSocketDirectory = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run install -d -m 700 ${cfg.socketDir}
    '';

    home.sessionVariables = {
      SSH_AUTH_SOCK = cfg.gpgAuthSock;
    };

    launchd.agents = {
      ssh-agent.enable = false;
      "com.openssh.ssh-agent".enable = false;
    };

    programs = {
      gpg = {
        scdaemonSettings = {
          disable-ccid = true;
        };
      };

      ssh = {
        enable = true;
        enableDefaultConfig = false;
        matchBlocks = {
          # When running under Claude, don't use 1Password for github.com.
          # Claude's SSH check (O24) probes git@github.com to decide SSH vs HTTPS
          # for marketplace/plugin operations. These are public repos where HTTPS
          # works fine, so we skip 1Password to avoid the Touch ID prompt.
          # See: https://github.com/anthropics/claude-code/issues/21108
          "claude-github" = {
            match = ''host "github.com" exec "test -n \"$_CLAUDE_SESSION\""'';
            identityAgent = "none";
          };
          "github.com-bitgo" = {
            host = "github.com-bitgo";
            hostname = "github.com";
            # Enable ControlMaster for work since this is a work machine
            # and work operations are frequent. YubiKey touch is expected.
            inherit (cfg.github) controlMaster controlPersist serverAliveInterval;
            controlPath = "${cfg.socketDir}/%C-%i-bitgo";
          };
          "github.com" = {
            hostname = "github.com";
            identityAgent = "\"${config.home.homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";
            # Disable ControlMaster for personal since personal operations
            # are rare on this work machine
            controlMaster = "no";
            inherit (cfg.github) serverAliveInterval;
          };
        };
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        defaultCacheTtl = 1800;
        defaultCacheTtlSsh = 1800;
        enableSshSupport = false; # prevents gpgconf subprocess in zshenv (~16ms saved)
        enableZshIntegration = false; # managed manually via zsh-defer in bitgo profile
        extraConfig = "enable-ssh-support"; # keep gpg-agent serving SSH keys
        maxCacheTtl = 7200;
        maxCacheTtlSsh = 7200;
        pinentry = {
          package = pkgs.pinentry_mac;
          program = "pinentry-mac";
        };
      };
    };
  };
}
