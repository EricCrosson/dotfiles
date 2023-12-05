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
in {
  imports =
    if stdenv.isDarwin
    then darwinImports
    else linuxImports;

  home = {
    username = "${user.username}";
    homeDirectory = "${user.homeDirectory}";
    stateVersion = "22.05";

    packages = with pkgs; [
      inputs.bell.packages.${pkgs.system}.default
      inputs.retry.packages.${pkgs.system}.default
      inputs.spacer.packages.${pkgs.system}.default

      age-plugin-yubikey
      amber
      bottom
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
      python310Packages.grip
      rm-improved
      sd
      units
      viddy
      viu
      vim
      wget

      # for shell
      eza
      fzf
      hub
      python3Full
      starship
      wakatime
    ];

    file = {
      ".xprofile".source = ../../.xprofile;
      # Shell
      # REFACTOR: use shellAliases
      ".zshenv".source = ../../.zshenv;
      ".zshrc".source = ../../.zshrc;
    };
  };

  programs = {
    # FIXME: atuin is not running without the zsh hook
    atuin = {
      enable = true;
      package = inputs.atuin.packages.${pkgs.system}.default;
      # FIXME: update home-manager for this option
      # flags = [
      #   "--disable-up-arrow"
      # ];
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
        cl = "clone";
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
        fsl = "push --force-with-lease";
        re = "restore";
        rs = "restore --staged";
        s = "status";
        su = "submodule update";

        exec = "!exec ";

        # After `git reset --soft HEAD^1`, commit with the same commit message
        # Source: https://stackoverflow.com/a/25930432
        recommit = "commit --reuse-message=HEAD@{1}";

        alias = "!git config --list | grep \"alias\\\\.\" | sed \"s/alias\\\\.\\\\([^=]*\\\\)=\\\\(.*\\\\)/\\\\1\\\\\\t => \\\\2/\" | sort";
      };
      delta = {
        enable = true;
        options = {
          line-numbers = true;
        };
      };
      ignores = [
        "/scratch/"
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
          excludesfile = "~/.config/git/ignore";
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
      font = {
        name = "DejaVu Sans Mono";
        size =
          if pkgs.stdenv.isLinux
          then 14
          else 18;
      };
      settings = {
        cursor_blink_interval = 0;
        scrollback_lines = 50000;
        macos_option_as_alt = "yes";
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
  };
}
