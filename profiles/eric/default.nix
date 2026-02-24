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
  # Create an inline derivation for the Homebrew-installed Kitty
  homebrewKitty = pkgs.runCommand "homebrew-kitty" {} ''
    mkdir -p $out/bin
    ln -s /opt/homebrew/bin/kitty $out/bin/kitty
  '';

  # Fetch catppuccin delta theme
  catppuccinDelta = pkgs.fetchFromGitHub {
    owner = "catppuccin";
    repo = "delta";
    rev = "e9e21cffd98787f1b59e6f6e42db599f9b8ab399";
    sha256 = "sha256-04po0A7bVMsmYdJcKL6oL39RlMLij1lRKvWl5AUXJ7Q=";
  };

  fzfAltCCommand = pkgs.writeShellScript "fzf-alt-c-command" ''
    { zoxide query -l 2>/dev/null; fd --type d --absolute-path; } | awk -v home="$HOME" '!seen[$0]++ { sub("^" home, "~"); print }'
  '';
in {
  imports =
    [
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
      ".config/git/allowed_signers" = {
        text = ''
          eric.s.crosson@utexas.edu ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J
        '';
      };

      # Add catppuccin delta theme configuration
      ".config/git/catppuccin.gitconfig".source = "${catppuccinDelta}/catppuccin.gitconfig";

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

    delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        line-numbers = true;
        features = "catppuccin-${pkgs.lib.strings.toLower profile.preferences.theme}";
      };
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

    gh = {
      enable = true;
      extensions = [
        inputs.gh-agent.packages.${pkgs.system}.gh-agent
        inputs.gh-arm.packages.${pkgs.system}.default
        inputs.gh-automerge.packages.${pkgs.system}.default
      ];
      gitCredentialHelper = {
        enable = false;
      };
      settings = {
        git_protocol = "https";
        prompt = "enabled";
        pager = "delta";
        aliases = {
          co = "pr checkout";
        };
      };
    };

    git = {
      enable = true;
      ignores = [
        ".DS_Store"
        "/.claude/settings.local.json"
        "/.direnv"
        "/.pre-commit-config.yaml"
        "/CLAUDE.local.md"
        "/scratch/"
      ];
      includes = [
        # Color theme for git diff, but makes it harder to read.
        # {
        #   path = "~/.config/git/catppuccin.gitconfig";
        # }
      ];
      settings = {
        advice = {
          skippedCherryPicks = false;
        };
        alias = {
          a = "add";
          b = "branch";
          c = "commit";
          co = "checkout";
          # https://stackoverflow.com/a/70205254
          continue = "-c core.editor=true rebase --continue";
          d = "diff";
          default-branch = "!git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'";
          ds = "diff --cached";
          f = "fetch";
          l = "log --graph --pretty=format:'%C(yellow)%h%C(cyan)%d%Creset %s %C(white)- %an, %ar%Creset'";
          p = "pull";
          fwl = "push --force-with-lease";
          re = "restore";
          rs = "restore --staged";
          s = "status";
          # [c]heck[o]ut [f]uzzy
          cof = ''
            !f() { \
              git branch --no-color --sort=-committerdate --format='%(refname:short)' | fzf --header 'git checkout' | xargs git checkout
            }; f
          '';
          pr = ''
            !f() { \
              export GIT_PR_ALL=$(gh pr list | column -ts'	') && \
              export GIT_PR_MINE=$(gh pr list --author "@me" | column -ts'	') && \
              echo "$GIT_PR_ALL" | fzf \
                --prompt 'All PRs> ' \
                --header 'CTRL-T: toggle all / my PRs' \
                --bind "ctrl-t:transform:[[ \$FZF_PROMPT =~ All ]] && \
                  echo \"change-prompt(My PRs> )+reload(printenv GIT_PR_MINE)\" || \
                  echo \"change-prompt(All PRs> )+reload(printenv GIT_PR_ALL)\"" \
              | awk '{print $(NF-5)}' | xargs git checkout
            }; f
          '';
          su = "submodule update";

          exec = "!exec ";

          # After `git reset --soft HEAD~1`, commit with the same commit message
          # Source: https://stackoverflow.com/a/25930432
          recommit = "commit --reuse-message=HEAD@{1}";
        };
        branch = {
          sort = "-committerdate";
        };
        color = {
          ui = true;
          interactive = "auto";
        };
        column = {
          ui = "auto";
        };
        commit = {
          gpgSign = true;
          verbose = true;
        };
        core = {
          autocrlf = false;
          editor = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
          fsmonitor = true;
          untrackedCache = true;
        };
        credential = {
          username = "EricCrosson";
        };
        diff = {
          algorithm = "histogram";
          colorMoved = "plain";
          mnemonicPrefix = true;
          renames = true;
        };
        fetch = {
          all = true;
          parallel = 10;
          prune = true;
        };
        github = {
          user = "${profile.email}";
        };
        gpg = {
          format = "ssh";
        };
        "gpg \"ssh\"" = {
          program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
          allowedSignersFile = "${profile.homeDirectory}/.config/git/allowed_signers";
        };
        init = {
          defaultBranch = "master";
        };
        merge = {
          conflictStyle = "zdiff3";
        };
        pull = {
          rebase = true;
        };
        push = {
          autoSetupRemote = true;
          default = "simple";
        };
        rebase = {
          autoSquash = true;
          autoStash = true;
          updateRefs = true;
        };
        rerere = {
          autoupdate = true;
          enabled = true;
        };
        tag = {
          gpgSign = true;
          sort = "version:refname";
        };
        user = {
          name = "Eric Crosson";
          email = "eric.s.crosson@utexas.edu";
          signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J";
        };
      };
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

    kitty = {
      enable = true;
      package =
        if pkgs.stdenv.isDarwin
        then homebrewKitty
        else pkgs.kitty;
      font = {
        name = "JetBrains Mono";
        size = 12;
      };
      keybindings = {
        "ctrl+shift+o" = "toggle_layout stack";
      };
      settings = {
        cursor_blink_interval = 0;
        cursor_shape = "block";
        macos_option_as_alt = "yes";
        scrollback_lines = 50000;
        shell_integration = "no-cursor";
      };
    };

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
