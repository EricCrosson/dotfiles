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
        settings = {
          "github.com-bitgo" = {
            HostName = "github.com";
            # Pin work authentication to the YubiKey-backed gpg-agent instead
            # of inheriting SSH_AUTH_SOCK from a different agent.
            IdentityAgent = cfg.gpgAuthSock;
            # Enable ControlMaster for work since this is a work machine
            # and work operations are frequent. YubiKey touch is expected.
            ControlMaster = cfg.github.controlMaster;
            ControlPersist = cfg.github.controlPersist;
            ServerAliveInterval = cfg.github.serverAliveInterval;
            ControlPath = "${cfg.socketDir}/%C-%i-bitgo";
          };
          "github.com" = {
            HostName = "github.com";
            IdentityAgent = "\"${config.home.homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";
            # Disable ControlMaster for personal since personal operations
            # are rare on this work machine
            ControlMaster = "no";
            ServerAliveInterval = cfg.github.serverAliveInterval;
          };
          "athens" = {
            HostName = "192.168.1.123";
            User = "eric";
            ServerAliveInterval = 120;
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

    # HM's darwin launchd agent runs `gpg-agent --supervised` behind a launchd
    # Sockets dict, but launchd never sets LISTEN_FDS/LISTEN_PID, so
    # --supervised aborts ("Fatal: file descriptor 3 must be valid") and
    # launchd crash-loops the job (5000+ spawns, exit 2). Supervised mode also
    # serves sockets under /private/var/run, which the pinned IdentityAgent
    # (~/.gnupg/S.gpg-agent.ssh) never points at. HM's launchd layer itself
    # wraps ProgramArguments in a wait4path/`sh -c` boot-delay wrapper, so
    # pass bare argv here.
    #
    # Instead, run `gpgconf --launch gpg-agent` on a timer: it is idempotent
    # (exits 0 when an agent already listens) and otherwise spawns gpg-agent
    # as a proper detached daemon owning the standard sockets in GNUPGHOME —
    # exactly the socket SSH_AUTH_SOCK and the work IdentityAgent point at.
    # RunAtLoad covers login; StartInterval re-checks every 30s, so a dead
    # agent is replaced within half a minute instead of breaking work pulls
    # until a gpg command happens to run.
    launchd.agents.gpg-agent.config = {
      ProgramArguments = mkForce [
        "${config.programs.gpg.package}/bin/gpgconf"
        "--launch"
        "gpg-agent"
      ];
      Sockets = mkForce {};
      RunAtLoad = mkForce true;
      StartInterval = 30;
      StandardErrorPath = "${config.home.homeDirectory}/.gnupg/gpg-agent-launchd.log";
      # StartInterval owns the scheduling; never restart on exit.
      KeepAlive = mkForce {
        Crashed = false;
        SuccessfulExit = false;
      };
    };
  };
}
