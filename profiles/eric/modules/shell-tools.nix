{
  pkgs,
  profile,
  ...
}: {
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
}
