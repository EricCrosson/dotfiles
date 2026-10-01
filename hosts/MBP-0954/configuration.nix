{pkgs, ...}: {
  config = {
    environment = {
      shells = [pkgs.zsh];
      # Handy is NOT installable from the cjpais/handy flake on darwin — that
      # flake only supports Linux. Install nixpkgs' flake-built handy package
      # instead of the Homebrew cask.
      systemPackages = [pkgs.handy];

      variables = {
        SHELL = "${pkgs.zsh}/bin/zsh";
        LANG = "en_US.UTF-8";
      };
    };

    fonts = {
      packages = with pkgs; [
        hack-font
        nerd-fonts.jetbrains-mono
      ];
    };

    homebrew = {
      enable = true;
      greedyCasks = true;
      onActivation = {
        autoUpdate = true;
        cleanup = "uninstall";
        extraFlags = ["--force"];
        upgrade = true;
      };

      taps = [
        "kunobi-ninja/kunobi"
      ];

      brews = [
        "kunobi-ninja/kunobi/kache"
        "md5sha1sum"
        "terminal-notifier"
        "xcodegen"
      ];

      caskArgs = {
        require_sha = true;
      };

      casks = [
        "1password"
        "ghostty"
        "postman"
      ];
    };

    launchd.daemons.limit-maxfiles = {
      serviceConfig = {
        Label = "limit.maxfiles";
        ProgramArguments = [
          "/bin/launchctl"
          "limit"
          "maxfiles"
          "524288"
          "10485760"
        ];
        RunAtLoad = true;
        LaunchOnlyOnce = true;
      };
    };

    launchd.daemons.ssh-agent = {
      serviceConfig.Disabled = true;
    };

    services.colima = {
      enable = true;
      cpus = 8;
      memory = 8;
      arch = "aarch64";
      enableBuildKit = true;
      username = "ericcrosson";
      homeDirectory = "/Users/ericcrosson";
    };

    programs = {
      zsh = {
        enable = true;
        enableCompletion = false; # home-manager runs compinit in ~/.zshrc
        enableBashCompletion = false;
        enableGlobalCompInit = false; # prevent compinit in /etc/zshrc
        promptInit = ""; # starship handles the prompt
      };
    };

    nix = {
      linux-builder = {
        enable = true;
      };

      extraOptions = ''
        experimental-features = nix-command flakes
        extra-trusted-users = ericcrosson
        keep-derivations = true
        keep-outputs = true

        min-free = ${toString (2 * 1024 * 1024 * 1024)}
        max-free = ${toString (10 * 1024 * 1024 * 1024)}

        builders-use-substitutes = true
      '';

      gc = {
        automatic = true;
        interval = {
          Hour = 3;
          Minute = 0;
        };
        options = "--delete-older-than 7d";
      };

      optimise = {
        automatic = true;
        interval = {
          Hour = 4;
          Minute = 15;
        };
      };

      # Required to use flakes, which are an experimental module
      package = pkgs.nixVersions.nix_2_34;

      settings = {
        trusted-users = [
          "@admin"
        ];
      };
    };

    security = {
      pam.services.sudo_local.touchIdAuth = true;
    };

    system = {
      defaults = {
        ActivityMonitor = {
          IconType = 5; # CPU Usage
        };
        dock = {
          autohide = true;
          wvous-tr-corner = null;
        };
        finder = {
          AppleShowAllExtensions = true;
          FXPreferredViewStyle = "Nlsv";
          FXRemoveOldTrashItems = true;
          ShowStatusBar = true;
          _FXSortFoldersFirst = true;
          _FXSortFoldersFirstOnDesktop = true;
        };
        spaces = {
          # Rift requires "Displays have separate Spaces" (System Settings →
          # Desktop & Dock → Mission Control) and exits at launch otherwise;
          # nix-darwin's spans-displays = true is the inverse setting.
          spans-displays = false;
        };
        NSGlobalDomain = {
          ApplePressAndHoldEnabled = false;
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          InitialKeyRepeat = 15;
          KeyRepeat = 2;
          NSAutomaticCapitalizationEnabled = false;
          NSAutomaticPeriodSubstitutionEnabled = false;
          NSAutomaticQuoteSubstitutionEnabled = false;
          NSAutomaticSpellingCorrectionEnabled = false;
          NSWindowShouldDragOnGesture = true;
        };
      };

      primaryUser = "ericcrosson";

      # Used for backwards compatibility, please read the changelog before changing:
      # $ darwin-rebuild changelog
      stateVersion = 4;
    };

    # The platform the configuration will be used on.
    nixpkgs.hostPlatform = "aarch64-darwin";
  };
}
