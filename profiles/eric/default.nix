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
in {
  imports =
    if stdenv.isDarwin
    then darwinImports
    else linuxImports;

  home = {
    username = "${profile.username}";
    homeDirectory = "${profile.homeDirectory}";
    stateVersion = "22.05";

    sessionVariables = {
      EDITOR = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
      FZF_ALT_C_COMMAND = "fd --type d";
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
      inputs.bell.packages.${pkgs.system}.default
      inputs.retry.packages.${pkgs.system}.default

      age-plugin-yubikey
      amber
      bottom
      broot
      comma
      curl
      du-dust
      entr
      fd
      fx
      git
      git-absorb
      git-extras
      gnupg
      gron
      htmlq
      htop
      hyperfine
      jq
      keychain
      moreutils
      mprocs
      pass
      pueue
      rm-improved
      sd
      spacer
      units
      viddy
      viu
      vim
      watchexec
      wget

      yt-dlp

      # for shell
      eza
      fzf
      python3
      starship
      wakatime
    ];

    file = {
      # Add catppuccin delta theme configuration
      ".config/git/catppuccin.gitconfig".source = "${catppuccinDelta}/catppuccin.gitconfig";
    };
  };

  programs = {
    atuin = {
      enable = true;
      package = inputs.atuin.packages.${pkgs.system}.default;
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
        inputs.gh-arm.packages.${pkgs.system}.default
        inputs.gh-automerge.packages.${pkgs.system}.default
      ];
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
          ds = "diff --cached";
          f = "fetch";
          l = "log --graph --pretty=format:'%C(yellow)%h%C(cyan)%d%Creset %s %C(white)- %an, %ar%Creset'";
          p = "pull";
          fsl = "push --force-with-lease";
          re = "restore";
          rs = "restore --staged";
          s = "status";
          # [c]heck[o]ut [f]uzzy
          cof = ''
            !f() { \
              git branch --no-color --sort=-committerdate --format='%(refname:short)' | fzf --header 'git checkout' | xargs git checkout
            }; f
          '';
          # [c]heck[o]ut [p]ull request
          cop = ''
            !f() { \
              gh pr list --author "@me" | fzf --header 'checkout PR' | awk '{print $(NF-5)}' | xargs git checkout
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
          gpgSign = false;
          verbose = true;
        };
        core = {
          autocrlf = false;
          editor = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
          fsmonitor = true;
          sshCommand = "ssh -i ~/.ssh/id_rsa_personal";
          untrackedCache = true;
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
          sort = "version:refname";
        };
        user = {
          name = "Eric Crosson";
          email = "eric.s.crosson@utexas.edu";
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

    zsh = {
      enable = true;
      # My global NixOS config does `compinit` already.
      # Disabling it in my user's `.zshrc` because calling it multiple
      # times causes startup delay, see
      # https://github.com/nix-community/home-manager/blob/990b82ecd31f6372bc4c3f39a9171961bc370a22/modules/programs/zsh.nix#L518-L524
      enableCompletion = false;

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreAllDups = true;
        ignoreDups = false;
        share = false;
      };

      initContent = builtins.readFile ../../zsh/login-shell.zsh;

      plugins = [
        {
          name = "esc-zsh/smart-cd";
          file = "smart-cd.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "esc-zsh";
            repo = "smart-cd";
            rev = "57051138141179c293dcaef2da659e42ad4f9eeb";
            sha256 = "sha256-TgWwvJqQvIjRXpYuSVZ4ZqJCqLF7a5IIqLPzyYNWaTs=";
          };
        }
        {
          name = "hlissner/zsh-autopair";
          file = "zsh-autopair.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "hlissner";
            repo = "zsh-autopair";
            rev = "396c38a7468458ba29011f2ad4112e4fd35f78e6";
            sha256 = "sha256-PXHxPxFeoYXYMOC29YQKDdMnqTO0toyA7eJTSCV6PGE=";
          };
        }
        {
          name = "jreese/zsh-titles";
          file = "titles.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "jreese";
            repo = "zsh-titles";
            rev = "116324bb384cc10b66eea5875782051e492e27e1";
            sha256 = "sha256-f22ND+A01/4uPwZf4N5zsJRjVgJTgXu3UVGuSe/Atn0=";
          };
        }
        {
          name = "lukechilds/zsh-better-npm-completion";
          file = "zsh-better-npm-completion.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "lukechilds";
            repo = "zsh-better-npm-completion";
            rev = "47e5987ca422de43784f9d76311d764f82af2717";
            sha256 = "sha256-ruQZ3R0Efbe2jnw/WBvTukdtSWoX/kx2mcafnJNoN1k=";
          };
        }
        {
          name = "esc-zsh/jq-zsh-plugin";
          file = "jq.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "esc-zsh";
            repo = "jq-zsh-plugin";
            rev = "205675c7fdc0a2ad3c3fab1b9bcf6d8fd0e4c585";
            sha256 = "sha256-q/xQZ850kifmd8rCMW+aAEhuA43vB9ZAW22sss9e4SE=";
          };
        }
        {
          name = "sobolevn/wakatime-zsh-plugin";
          file = "wakatime.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "sobolevn";
            repo = "wakatime-zsh-plugin";
            rev = "69c6028b0c8f72e2afcfa5135b1af29afb49764a";
            sha256 = "sha256-pA1VOkzbHQjmcI2skzB/OP5pXn8CFUz5Ok/GLC6KKXQ=";
          };
        }
        {
          # does incur a runtime slowdown, especially during paste
          name = "zsh-users/zsh-autosuggestions";
          file = "zsh-autosuggestions.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-autosuggestions";
            rev = "c3d4e576c9c86eac62884bd47c01f6faed043fc5";
            sha256 = "sha256-B+Kz3B7d97CM/3ztpQyVkE6EfMipVF8Y4HJNfSRXHtU=";
          };
        }
        {
          name = "zsh-users/zsh-completions";
          file = "zsh-completions.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-completions";
            rev = "f7c3173886f4f56bf97d622677c6d46ab005831f";
            sha256 = "sha256-sZCHI4ZFfRjcG1XF/3ABf9+zv7f2Di8Xrh4Dr+qt4Us=";
          };
        }
        {
          name = "zsh-users/zsh-history-substring-search";
          file = "zsh-history-substring-search.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-history-substring-search";
            rev = "8dd05bfcc12b0cd1ee9ea64be725b3d9f713cf64";
            sha256 = "sha256-houujb1CrRTjhCc+dp3PRHALvres1YylgxXwjjK6VZA=";
          };
        }
        {
          name = "peterhurford/up.zsh";
          file = "up.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "peterhurford";
            repo = "up.zsh";
            rev = "c8cc0d0edd6be2d01f467267e3ed385c386a0acb";
            sha256 = "sha256-yUWmKi95l7UFcjk/9Cfy/dDXQD3K/m2Q+q72YLZvZak=";
          };
        }
        {
          name = "esc-zsh/mc";
          file = "mc.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "esc-zsh";
            repo = "mc";
            rev = "53f446969e5ddf8f7d0c42cdfe476203f1871414";
            sha256 = "sha256-Ll4gEV38nmtuLzu00JUDpDxm8Uq6oxDrOebio1zGV7A=";
          };
        }
        {
          name = "esc-zsh/rh";
          file = "rh.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "esc-zsh";
            repo = "rh";
            rev = "2e7ba9f0e71fc7090c22e7cf1872592361296d48";
            sha256 = "sha256-vWHbPnGPNQT3VytHzy1vS63C0vl26x+5lYIumDC2ei4=";
          };
        }
      ];

      shellAliases = {
        g = "git";
        grip = "grip --pass $GITHUB_TOKEN";
        h = "hx --vsplit";
        l = "eza -lg --git --time-style=long-iso";
        npx = "npx --no-install";
        rip = "rip --graveyard $HOME/.local/share/Trash";
        ssh = "ssh -t";
        viddy = "viddy --differences";
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
