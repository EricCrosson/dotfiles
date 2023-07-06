{
  config,
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
      inputs.ast-grep.packages.${pkgs.system}.default
      inputs.bash-barrier.packages.${pkgs.system}.default
      inputs.bell.packages.${pkgs.system}.default
      inputs.git-diff-regex.packages.${pkgs.system}.default
      inputs.git-disjoint.packages.${pkgs.system}.default
      inputs.git-dl.packages.${pkgs.system}.default
      inputs.git-review.packages.${pkgs.system}.default
      inputs.npm-dep-version.packages.${pkgs.system}.default
      inputs.nurl.packages.${pkgs.system}.default
      inputs.retry.packages.${pkgs.system}.default

      age-plugin-yubikey
      amber
      bottom
      cargo-watch
      comma
      curl
      delta
      dtrx
      du-dust
      entr
      fd
      git
      git-absorb
      git-extras
      gnupg
      htop
      hyperfine
      jq
      mprocs
      passage
      pueue
      python310Packages.grip
      ripgrep
      rm-improved
      sd
      viddy
      viu
      vim
      wget
      yq-go

      # Still missing
      # kubectx

      # for shell
      exa
      fzf
      go-jira
      hub
      python
      starship
      wakatime
    ];

    file = {
      # DISCUSS: can we use a nix-provided path to this file?
      ".direnvrc" = {
        text = "source /run/current-system/sw/share/nix-direnv/direnvrc";
      };

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
        catppuccin-frappe = builtins.readFile (catppuccin-bat + "/Catppuccin-frappe.tmTheme");
        catppuccin-latte = builtins.readFile (catppuccin-bat + "/Catppuccin-latte.tmTheme");
        catppuccin-macchiato = builtins.readFile (catppuccin-bat + "/Catppuccin-macchiato.tmTheme");
        catppuccin-mocha = builtins.readFile (catppuccin-bat + "/Catppuccin-mocha.tmTheme");
      };
    };

    firefox = {
      enable = true;
      # DISCUSS: is the nightly firefox flake compatible with darwin?
      package =
        if stdenv.isDarwin
        then pkgs.firefox-bin
        else inputs.firefox.packages.${pkgs.system}.firefox-nightly-bin;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        onepassword-password-manager
        ublock-origin
      ];
      profiles = {
        default = {
          id = 0;
          name = "Default";
          isDefault = true;
          bookmarks = [
            {
              name = "Discord";
              keyword = "discord";
              url = "https://discord.com/channels/@me";
            }
            {
              name = "GitHub Notifications";
              keyword = "n";
              url = "https://github.com/notifications?query=is%3Aunread";
            }
            {
              name = "GitHub Pull Requests";
              keyword = "p";
              url = "https://github.com/pulls";
            }
            {
              name = "Helix Editor Keymap";
              keyword = "hx";
              url = "https://docs.helix-editor.com/master/keymap.html";
            }
          ];
          search = {
            default = "Google";
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "type";
                        value = "packages";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["np"];
              };
              "Amazon.com".metaData.hidden = true;
              "Bing".metaData.hidden = true;
              "DuckDuckGo".metaData.hidden = true;
              "eBay".metaData.hidden = true;
              "Wikipedia (en)".metaData.alias = "w";
            };
            force = true;
          };
          settings = {
            "app.normandy.enabled" = false;
            "browser.contentblocking.category" = "strict";
            "browser.startup.page" = 3; # Restore previous windows and tabs on startup.
            "extensions.htmlaboutaddons.inline-options.enabled" = false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "extensions.pocket.enabled" = false;
            "extensions.pocket.showHome" = false;
            "privacy.donottrackheader.enabled" = true;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.snippets" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
            "browser.newtabpage.activity-stream.section.highlights.rows" = false;
            "browser.newtabpage.activity-stream.section.topstories.rows" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.topSitesRows" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
            "media.eme.enabled" = true;

            # Opt out of all telemetry
            "browser.newtabpage.activity-stream.feeds.telemetry" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            "browser.ping-centre.telemetry" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "datareporting.sessions.current.clean" = true;
            "devtools.onboarding.telemetry.logged" = false;
            "toolkit.telemetry.archive.enabled" = false;
            "toolkit.telemetry.bhrPing.enabled" = false;
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.firstShutdownPing.enabled" = false;
            "toolkit.telemetry.hybridContent.enabled" = false;
            "toolkit.telemetry.newProfilePing.enabled" = false;
            "toolkit.telemetry.reportingpolicy.firstRun" = false;
            "toolkit.telemetry.shutdownPingSender.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "toolkit.telemetry.updatePing.enabled" = false;
          };
        };
      };
    };

    gh = {
      enable = true;
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
        di = "diff ':(exclude)./**/package-lock.json' ':(exclude)./**/yarn.lock'";
        dc = "diff --cached";
        dci = "diff --cached ':(exclude)**/package-lock.json' ':(exclude)**/yarn.lock'";
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
        gpg = {
          program = "${pkgs.gnupg}";
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
        # example: git clone gh:ericcrosson/dotfiles
        url = {
          "git@github.com" = {
            insteadOf = "gh:";
            PushInsteadOf = "gh:";
          };
        };
      };
    };

    home-manager.enable = true; # Let Home Manager install and manage itself.

    kitty = {
      enable = true;
      font = {
        name = "DejaVu Sans Mono";
        size = 14;
      };
      settings = {
        cursor_blink_interval = 0;
        scrollback_lines = 50000;
      };
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
