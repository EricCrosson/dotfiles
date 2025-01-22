{
  pkgs,
  user,
  inputs,
  ...
}:
# TODO: set font to Hack
# FIXME: screen tearing
let
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
in {
  imports =
    if stdenv.isDarwin
    then darwinImports
    else linuxImports;

  home = {
    username = "${user.username}";
    homeDirectory = "${user.homeDirectory}";
    stateVersion = "22.05";

    sessionVariables = {
      DIRENV_LOG_FORMAT = "";
      EDITOR = "hx";
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
      RIPGREP_CONFIG_PATH = "${user.homeDirectory}/.config/ripgrep/ripgreprc";
      SMART_CD_ONLY_IF_FITS_RATIO = 66;
      ZSH_WAKATIME_BIN = "/etc/profiles/per-user/${user.username}/bin/wakatime-cli";
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
      delta
      du-dust
      entr
      fd
      fx
      git
      git-absorb
      git-extras
      gnupg
      htop
      hyperfine
      jq
      keychain
      moreutils
      mprocs
      pass
      passage
      pueue
      rm-improved
      sd
      spacer
      units
      viddy
      viu
      vim
      wget

      # TODO: move to an "extras" profile, when space is plentiful
      # yt-dlp

      # for shell
      eza
      fzf
      hub
      python3Full
      starship
      wakatime
    ];

    file = {
      # REFACTOR: this only _needs_ to be present on work machines,
      # though it doesn't interfere with anything on personal machines.
      ".config/git/personal-config".source = ../../.config/git/personal-config;

      # Not sure why setting `xsession.profileExtra` doesn't seem to write .xprofile.
      # Well, this isn't ideal, but it's working, so no need to bugger with it.
      ".xprofile".source = ../../.xprofile;
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

    # Note: I had to run "bat cache --build" to get this theme to be picked up.
    # Closed issue: https://github.com/nix-community/home-manager/issues/2482
    bat = {
      enable = true;
      config = {
        theme = "catppuccin-${user.preferences.theme}";
        style = "plain";
        paging = "never";
      };
      themes = let
        catppuccin-bat = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "bat";
          rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
          sha256 = "sha256-6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
        };
      in {
        catppuccin-frappe = {
          src = catppuccin-bat;
          file = "Catppuccin-frappe.tmTheme";
        };
        catppuccin-latte = {
          src = catppuccin-bat;
          file = "Catppuccin-latte.tmTheme";
        };
        catppuccin-macchiato = {
          src = catppuccin-bat;
          file = "Catppuccin-macchiato.tmTheme";
        };
        catppuccin-mocha = {
          src = catppuccin-bat;
          file = "Catppuccin-mocha.tmTheme";
        };
      };
    };

    broot = {
      enable = true;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    gh = {
      enable = true;
      extensions = [
        # Not yet available from nixpkgs
        # pkgs.gh-copilot
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
      userName = "Eric Crosson";
      userEmail = "${user.email}";
      aliases = {
        a = "add";
        b = "branch";
        c = "commit";
        co = "checkout";
        # https://stackoverflow.com/a/70205254
        continue = "-c core.editor=true rebase --continue";
        d = "diff";
        di = "diff ':(exclude):package-lock.json' ':(exclude)./**/package-lock.json' ':(exclude)yarn.lock' ':(exclude)./**/yarn.lock'";
        ds = "diff --cached";
        dsi = "diff --cached ':(exclude):package-lock.json' ':(exclude)**/package-lock.json' ':(exclude)yarn.lock' ':(exclude)**/yarn.lock'";
        dn = "diff --name-only";
        f = "fetch";
        l = "log --graph --pretty=format:'%C(yellow)%h%C(cyan)%d%Creset %s %C(white)- %an, %ar%Creset'";
        p = "pull";
        fa = "fetch --all --prune --jobs=10";
        fsl = "push --force-with-lease";
        re = "restore";
        rs = "restore --staged";
        s = "status";
        step-towards = ''
          !f() { \
            commit=$(git rev-list --ancestry-path HEAD..$1 | tail -n $2 | head -n 1); \
            if [ -n \"$commit\" ]; then \
              git checkout $commit; \
            else \
              echo \"No more commits to step towards $1\"; \
            fi
          }; f
        '';
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

        # After `git reset --soft HEAD^1`, commit with the same commit message
        # Source: https://stackoverflow.com/a/25930432
        recommit = "commit --reuse-message=HEAD@{1}";

        automerge = "!f() { gh pr merge --auto --merge \"$1\"; }; f";

        alias = "!git config --list | grep \"alias\\\\.\" | sed \"s/alias\\\\.\\\\([^=]*\\\\)=\\\\(.*\\\\)/\\\\1\\\\\\t => \\\\2/\" | sort";
      };
      delta = {
        enable = true;
        options = {
          line-numbers = true;
        };
      };
      ignores = [
        ".DS_Store"
        "/.aider.*"
        "/.direnv"
        "/scratch/"
      ];
      includes = [
        {
          condition = "gitdir:~/workspace/EricCrosson/";
          path = "~/.config/git/personal-config";
        }
        {
          condition = "gitdir:~/workspace/semantic-release-action/";
          path = "~/.config/git/personal-config";
        }
        {
          condition = "gitdir:~/workspace/typescript-tools/";
          path = "~/.config/git/personal-config";
        }
      ];
      extraConfig = {
        advice = {
          skippedCherryPicks = false;
        };
        color = {
          ui = true;
          interactive = "auto";
        };
        core = {
          editor = "hx";
          autocrlf = false;

          diff-highlight = {
            oldNormal = "red bold";
            oldHighlight = "red bold reverse";
            newNormal = "green bold";
            newHighlight = "green bold reverse";
          };

          diff = {
            meta = 11;
            frag = "magenta bold";
            commit = "yellow bold";
            old = "red bold";
            new = "green bold";
            whitespace = "red reverse";
          };
        };
        github = {
          user = "${user.email}";
        };
        init = {
          defaultBranch = "master";
        };
        pull = {
          rebase = true;
        };
        push = {
          default = "simple";
        };
        rerere = {
          enabled = true;
        };
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
        name = "DejaVu Sans Mono";
        size = 14;
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

      initExtra = builtins.readFile ../../zsh/login-shell.zsh;

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
        # I migrated this but it tried to write a file into the read-only
        # Nix store. I've disabled it to see if I can tell what it was used
        # for. If this comment ages and there's no smoking gun, delete this
        # plugin.
        #
        # {
        #   name = "robsis/zsh-completion-generator";
        #   file = "zsh-completion-generator.plugin.zsh";
        #   src = pkgs.fetchFromGitHub {
        #     owner = "robsis";
        #     repo = "zsh-completion-generator";
        #     rev = "d01700d037e87db97f51c5da201d63a3fef7e9f6";
        #     sha256 = "sha256-OoK+LMUaFYxLrGG6awb5fU97jXNT0SFACO3AbLheZNU=";
        #   };
        # }
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
        g = "hub";
        git = "hub";
        grip = "grip --pass $GITHUB_TOKEN";
        h = "hx --vsplit";
        j = "jira";
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
    desktop = "${user.homeDirectory}/tmp";
    download = "${user.homeDirectory}/tmp";
    documents = "${user.homeDirectory}/files";
    music = "${user.homeDirectory}/files/media";
    pictures = "${user.homeDirectory}/files/media";
    videos = "${user.homeDirectory}/files/media";
    extraConfig = {
      XDG_DATA_HOME = "${user.homeDirectory}/.local/share";
    };
  };
}
