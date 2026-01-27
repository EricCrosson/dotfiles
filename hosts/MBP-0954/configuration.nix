{pkgs, ...}: {
  config = {
    environment = {
      shells = [pkgs.zsh];

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
      onActivation = {
        autoUpdate = true;
        cleanup = "uninstall";
        upgrade = true;
      };

      brews = [
        "ffmpeg"
        "imagemagick"
        "jiratui"
        "md5sha1sum"
        "xcodegen"
        "ykman"
      ];

      caskArgs = {
        no_quarantine = true;
        require_sha = true;
      };

      casks = [
        "kitty"
        "postman"
        "tldev/tap/posturr"
      ];

      taps = [
        "tldev/tap"
      ];
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
      };
    };

    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
        extra-trusted-users = ericcrosson
        keep-derivations = true
        keep-outputs = true

        min-free = ${toString (2 * 1024 * 1024 * 1024)}
        max-free = ${toString (10 * 1024 * 1024 * 1024)}
      '';

      gc = {
        automatic = true;
        interval = {
          Weekday = 0;
          Hour = 0;
          Minute = 0;
        };
        options = "--delete-older-than 7d";
      };

      optimise = {
        automatic = true;
      };

      # Required to use flakes, which are an experimental module
      package = pkgs.nixVersions.nix_2_31;

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
