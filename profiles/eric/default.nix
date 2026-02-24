{
  pkgs,
  profile,
  inputs,
  ...
}: let
  inherit (pkgs) stdenv;
  darwinImports = [
    ../../os/darwin
  ];
  linuxImports = [
    ../../os/linux
  ];
  fzfAltCCommand = pkgs.writeShellScript "fzf-alt-c-command" ''
    { zoxide query -l 2>/dev/null; fd --type d --absolute-path; } | awk -v home="$HOME" '!seen[$0]++ { sub("^" home, "~"); print }'
  '';
in {
  imports =
    [
      ./modules/git.nix
      ./modules/kitty.nix
      ./modules/zsh.nix
    ]
    ++ (
      if stdenv.isDarwin
      then darwinImports
      else linuxImports
    );

  home = {
    username = "${profile.username}";
    homeDirectory = "${profile.homeDirectory}";
    stateVersion = "22.05";

    sessionVariables = {
      EDITOR = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
      FZF_ALT_C_COMMAND = "${fzfAltCCommand}";
      FZF_DEFAULT_COMMAND = "fd --type f";
      FZF_CTRL_T_COMMAND = "fd --type f";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
      # Though home-manager [sets] this environment variable, it isn't
      # sourced by Xorg and awesome-wm for some reason (possibly [this
      # one]).
      #
      # [sets]: https://github.com/nix-community/home-manager/blob/ee5673246de0254186e469935909e821b8f4ec15/modules/programs/ripgrep.nix#L38
      # [this one]: https://github.com/nix-community/home-manager/issues/1011
      # TODO: test if RIPGREP_CONFIG_PATH is set in darwin without this line
      RIPGREP_CONFIG_PATH = "${profile.homeDirectory}/.config/ripgrep/ripgreprc";
      SMART_CD_ONLY_IF_FITS_RATIO = 66;
      ZSH_WAKATIME_BIN = "/etc/profiles/per-user/${profile.username}/bin/wakatime-cli";
    };

    packages = with pkgs; [
      inputs.atlas.packages.${pkgs.system}.default
      inputs.cortex.packages.${pkgs.system}.default
      inputs.bell.packages.${pkgs.system}.default
      inputs.retry.packages.${pkgs.system}.default

      age-plugin-yubikey
      amber
      atuin-desktop
      bottom
      broot
      comma
      curl
      dust
      entr
      fd
      ffmpeg

      fx
      git
      git-absorb
      git-extras
      gnupg
      gron
      htmlq
      htop
      hyperfine
      imagemagick
      jq
      moreutils
      mprocs
      pass
      pueue
      sd
      spacer
      units
      viddy
      viu
      vim
      watchexec
      wget
      yubikey-manager

      # yt-dlp # derivation temporarily broken

      # for shell
      eza
      fzf
      python3
      starship
      wakatime-cli
    ];

    file = {
      ".config/fd/ignore" = {
        text = ''
          .direnv/
          .git/
          build/
          dist/
          node_modules/
          target/
        '';
      };
    };
  };

  programs = {
    atuin = {
      enable = true;
      enableZshIntegration = false; # sourced via zsh-defer in initContent
      flags = [
        "--disable-up-arrow"
      ];
      settings = {
        dialect = "us";
        auto_sync = true;
        sync_frequency = "5m";
        update_check = false;
        search_mode = "fuzzy";
      };
    };

    zoxide = {
      enable = true;
      enableZshIntegration = false; # sourced via zsh-defer in initContent
    };

    bat = {
      enable = true;
      config = {
        theme = "Catppuccin ${profile.preferences.theme}";
        style = "plain";
        paging = "never";
      };
      themes = let
        catppuccin-bat = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "bat";
          rev = "699f60fc8ec434574ca7451b444b880430319941";
          sha256 = "sha256-6fWoCH90IGumAMc4buLRWL0N61op+AuMNN9CAR9/OdI=";
        };
      in {
        "Catppuccin Frappe" = {
          src = catppuccin-bat;
          file = "themes/Catppuccin Frappe.tmTheme";
        };
        "Catppuccin Latte" = {
          src = catppuccin-bat;
          file = "themes/Catppuccin Latte.tmTheme";
        };
        "Catppuccin Macchiato" = {
          src = catppuccin-bat;
          file = "themes/Catppuccin Macchiato.tmTheme";
        };
        "Catppuccin Mocha" = {
          src = catppuccin-bat;
          file = "themes/Catppuccin Mocha.tmTheme";
        };
      };
    };

    broot = {
      enable = true;
      enableZshIntegration = false; # sourced via zsh-defer in initContent
    };

    direnv = {
      enable = true;
      enableZshIntegration = false; # sourced via zsh-defer in initContent
      config = {
        global = {
          hide_env_diff = true;
        };
      };
      nix-direnv.enable = true;
    };

    # Temporarily marked as broken
    ghostty = {
      enable = false;
      enableZshIntegration = true;
      installBatSyntax = true;
      installVimSyntax = true;
      settings = {
        theme = "catppuccin-${pkgs.lib.strings.toLower profile.preferences.theme}";
      };
    };

    home-manager.enable = true; # Let Home Manager install and manage itself.

    ripgrep = {
      enable = true;
      arguments = [
        "-."
        "--glob=!.git/"
        "--no-heading"
        "--smart-case"
      ];
    };

    starship = {
      enable = true;
      enableZshIntegration = false; # sourced from pre-generated cache in initContent
      settings = {
        format = pkgs.lib.concatStrings [
          "$username"
          "$hostname"
          "$directory"
          "$git_branch"
          "$git_state"
          "$git_status"
          "$cmd_duration"
          "$line_break"
          "$character"
        ];
        character = {
          success_symbol = "[;](yellow)";
          error_symbol = "[;](red)";
          vicmd_symbol = "[;](green)";
        };
        directory = {
          style = "blue";
          truncation_length = 100;
        };
        cmd_duration = {
          format = "[$duration]($style) ";
          style = "yellow";
        };
        git_branch = {
          format = "[$branch]($style)";
          style = "bright-black";
        };
        git_status = {
          format = "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style)";
          style = "cyan";
          conflicted = "​";
          untracked = "​";
          modified = "​";
          staged = "​";
          renamed = "​";
          deleted = "​";
          stashed = "≡";
        };
        git_state = {
          format = "\([$state( $progress_current/$progress_total)]($style)\) ";
          style = "bright-black";
        };
        python = {
          format = "[$virtualenv]($style) ";
          style = "bright-black";
        };
      };
    };
  };

  xdg.userDirs = {
    createDirectories = true;
    desktop = "${profile.homeDirectory}/tmp";
    download = "${profile.homeDirectory}/tmp";
    documents = "${profile.homeDirectory}/files";
    music = "${profile.homeDirectory}/files/media";
    pictures = "${profile.homeDirectory}/files/media";
    videos = "${profile.homeDirectory}/files/media";
    extraConfig = {
      XDG_DATA_HOME = "${profile.homeDirectory}/.local/share";
    };
  };
}
