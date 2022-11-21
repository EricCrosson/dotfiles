{
  config,
  pkgs,
  system,
  user,
  inputs,
  ...
}:
# TODO: set font to Hack
# FIXME: screen tearing
# FIXME: why does polybar not start with bspwm?
# FIXME: what happened to my virtual desktops with bspwm?
{
  home = {
    username = "${user.username}";
    homeDirectory = "/home/${user.username}";
    stateVersion = "22.05";

    packages = with pkgs; [
      inputs.ast-grep.packages.${system}.default
      inputs.bash-barrier.packages.${system}.default
      inputs.git-diff-regex.packages.${system}.default
      inputs.git-disjoint.packages.${system}.default

      amber
      bottom
      cargo-watch
      curl
      delta
      du-dust
      entr
      evtest
      fd
      git
      git-absorb
      git-extras
      gnupg
      htop
      python310Packages.grip
      ripgrep
      rm-improved
      sd
      viddy
      viu
      vim
      wget

      # Still missing
      # kubectx
      # ltex-ls

      # Helix and supporting tools
      inputs.jsonnet-language-server.packages.${system}.jsonnet-tool
      nil
      nodePackages.bash-language-server
      nodePackages.dockerfile-language-server-nodejs
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      rust-analyzer
      shellcheck
      taplo-lsp # TOML

      # for shell
      exa
      fzf
      go-jira
      hub
      python
      starship
      wakatime

      # for window manager
      polybar
      rofi
      pamixer
      pavucontrol # Graphival audio control
      playerctl
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

      # Window Manager
      ".config/sxhkd/sxhkdrc" = {
        onChange = "pkill -USR1 -x sxhkd";
        text = ''
          #
          # wm independent hotkeys
          #

          # terminal emulator
          super + Return
          	kitty

          # program launcher
          super + @space
            rofi -show combi -modes combi -combi-modes "window,drun,run"

          # make sxhkd reload its configuration files:
          super + Escape
          	pkill -USR1 -x sxhkd

          #
          # bspwm hotkeys
          #

          # quit/restart bspwm
          super + alt + {q,r}
          	bspc {quit,wm -r}

          # close and kill
          super + {_,shift + }w
          	bspc node -{c,k}

          # alternate between the tiled and monocle layout
          super + m
          	bspc desktop -l next

          # send the newest marked node to the newest preselected node
          super + y
          	bspc node newest.marked.local -n newest.!automatic.local

          # swap the current node and the biggest window
          super + g
          	bspc node -s biggest.window

          #
          # state/flags
          #

          # set the window state
          super + {t,shift + t,s,f}
          	bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

          # set the node flags
          super + ctrl + {m,x,y,z}
          	bspc node -g {marked,locked,sticky,private}

          #
          # focus/swap
          #

          # focus the node in the given direction
          super + {_,shift + }{h,j,k,l}
          	bspc node -{f,s} {west,south,north,east}

          # focus the node for the given path jump
          super + {p,b,comma,period}
          	bspc node -f @{parent,brother,first,second}

          # focus the next/previous window in the current desktop
          super + {_,shift + }c
          	bspc node -f {next,prev}.local.!hidden.window

          # focus the next/previous desktop in the current monitor
          super + bracket{left,right}
          	bspc desktop -f {prev,next}.local

          # focus the last node/desktop
          super + {grave,Tab}
          	bspc {node,desktop} -f last

          # focus the older or newer node in the focus history
          super + {o,i}
          	bspc wm -h off; \
          	bspc node {older,newer} -f; \
          	bspc wm -h on

          # focus or send to the given desktop
          super + {_,shift + }{1-9,0}
          	bspc {desktop -f,node -d} '^{1-9,10}'

          #
          # preselect
          #

          # preselect the direction
          super + ctrl + {h,j,k,l}
          	bspc node -p {west,south,north,east}

          # preselect the ratio
          super + ctrl + {1-9}
          	bspc node -o 0.{1-9}

          # cancel the preselection for the focused node
          super + ctrl + space
          	bspc node -p cancel

          # cancel the preselection for the focused desktop
          super + ctrl + shift + space
          	bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

          #
          # move/resize
          #

          # expand a window by moving one of its side outward
          super + alt + {h,j,k,l}
          	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

          # contract a window by moving one of its side inward
          super + alt + shift + {h,j,k,l}
          	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

          # move a floating window
          super + {Left,Down,Up,Right}
          	bspc node -v {-20 0,0 20,0 -20,20 0}

          # volume control
          XF86AudioMute
            pamixer --toggle-mute
          XF86AudioLowerVolume
            pamixer --decrease 5
          XF86AudioRaiseVolume
            pamixer --increase 5
          XF86AudioPlay
            playerctl --player=%any play-pause
        '';
      };

      ".config/bspwm/bspwmrc" = {
        onChange = "bspc wm -r";
        text = ''
          #!${pkgs.bash}/bin/sh

          pgrep -x sxhkd > /dev/null || sxhkd &

          bspc monitor HDMI-0 -d 1 2 3 4 5
          bspc monitor DP-2 -d 6 7 8 9 0

          bspc config border_width         1
          bspc config window_gap           4

          bspc config split_ratio          0.52
          bspc config borderless_monocle   true
          bspc config gapless_monocle      true

          bspc rule -a Gimp state=floating follow=on
          bspc rule -a Screenkey manage=off

          $HOME/.config/polybar/launch.sh &
        '';
      };

      ".config/polybar/launch.sh" = {
        executable = true;
        onChange = "~/.config/polybar/launch.sh";
        text = ''
          #!${pkgs.bash}/bin/bash

          # Terminate already running bar instances.
          polybar-msg cmd quit

          # Launch bar1 and bar2.
          echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
          polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
          polybar bar2 2>&1 | tee -a /tmp/polybar2.log & disown
        '';
      };

      ".config/polybar/config.ini" = {
        onChange = "~/.config/polybar/launch.sh";
        text = ''
          ;==========================================================
          ;
          ;
          ;   ██████╗  ██████╗ ██╗  ██╗   ██╗██████╗  █████╗ ██████╗
          ;   ██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██╔══██╗██╔══██╗██╔══██╗
          ;   ██████╔╝██║   ██║██║   ╚████╔╝ ██████╔╝███████║██████╔╝
          ;   ██╔═══╝ ██║   ██║██║    ╚██╔╝  ██╔══██╗██╔══██║██╔══██╗
          ;   ██║     ╚██████╔╝███████╗██║   ██████╔╝██║  ██║██║  ██║
          ;   ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝
          ;
          ;
          ;   To learn more about how to configure Polybar
          ;   go to https://github.com/polybar/polybar
          ;
          ;   The README contains a lot of information
          ;
          ;==========================================================

          [colors]
          background = #282A2E
          background-alt = #373B41
          foreground = #C5C8C6
          primary = #F0C674
          secondary = #8ABEB7
          alert = #A54242
          disabled = #707880

          [base/bar]
          width = 100%
          height = 10pt
          radius = 6

          ; dpi = 96

          background = ''${colors.background}
          foreground = ''${colors.foreground}

          line-size = 1pt

          border-size = 2pt
          border-color = #00000000

          padding-left = 0
          padding-right = 1

          module-margin = 1

          separator = |
          separator-foreground = ''${colors.disabled}

          font-0 = monospace:size=10.5;2

          modules-left = xworkspaces xwindow
          modules-right = filesystem pulseaudio xkeyboard memory cpu wlan eth date

          cursor-click = pointer
          cursor-scroll = ns-resize

          enable-ipc = true

          ; tray-position = right

          wm-restack = bspwm
          override-redirect = true                      ; Allow full-screen windows to cover the bar

          [bar/bar1]
          inherit = base/bar
          monitor = HDMI-0

          [bar/bar2]
          inherit = base/bar
          monitor = DP-2

          [module/xworkspaces]
          type = internal/xworkspaces

          label-active = %name%
          label-active-background = ''${colors.background-alt}
          label-active-underline= ''${colors.primary}
          label-active-padding = 1

          label-occupied = %name%
          label-occupied-padding = 1

          label-urgent = %name%
          label-urgent-background = ''${colors.alert}
          label-urgent-padding = 1

          label-empty = %name%
          label-empty-foreground = ''${colors.disabled}
          label-empty-padding = 1

          [module/xwindow]
          type = internal/xwindow
          label = %title:0:60:...%

          [module/filesystem]
          type = internal/fs
          interval = 60

          mount-0 = /

          label-mounted = %{F#F0C674}%mountpoint%%{F-} %percentage_used%%

          label-unmounted = %mountpoint% not mounted
          label-unmounted-foreground = ''${colors.disabled}

          [module/pulseaudio]
          type = internal/pulseaudio

          format-volume-prefix = "VOL "
          format-volume-prefix-foreground = ''${colors.primary}
          format-volume = <label-volume>

          label-volume = %percentage%%

          label-muted = muted
          label-muted-foreground = ''${colors.disabled}

          [module/xkeyboard]
          type = internal/xkeyboard
          blacklist-0 = num lock

          label-layout = %layout%
          label-layout-foreground = ''${colors.primary}

          label-indicator-padding = 2
          label-indicator-margin = 1
          label-indicator-foreground = ''${colors.background}
          label-indicator-background = ''${colors.secondary}

          [module/memory]
          type = internal/memory
          interval = 2
          format-prefix = "RAM "
          format-prefix-foreground = ''${colors.primary}
          label = %percentage_used:2%%

          [module/cpu]
          type = internal/cpu
          interval = 2
          format-prefix = "CPU "
          format-prefix-foreground = ''${colors.primary}
          label = %percentage:2%%

          [network-base]
          type = internal/network
          interval = 5
          format-connected = <label-connected>
          format-disconnected = <label-disconnected>
          label-disconnected = %{F#F0C674}%ifname%%{F#707880} disconnected

          [module/wlan]
          inherit = network-base
          interface-type = wireless
          label-connected = %{F#F0C674}%ifname%%{F-} %essid% %local_ip%

          [module/eth]
          inherit = network-base
          interface-type = wired
          label-connected = %{F#F0C674}%ifname%%{F-} %local_ip%

          [module/date]
          type = internal/date
          interval = 1

          date = %H:%M
          date-alt = %Y-%m-%d %H:%M

          label = %date%
          label-foreground = ''${colors.primary}

          [settings]
          screenchange-reload = true
          pseudo-transparency = true
        '';
      };
    };
  };

  programs = {
    # FIXME: this doesn't appear to be active
    atuin = {
      enable = true;
      settings = {
        dialect = "us";
        auto_sync = false;
        update_check = false;
        search_mode = "fuzzy";
      };
    };

    # Note: I had to run "bat cache --build" to get this theme to be picked up.
    # Closed issue: https://github.com/nix-community/home-manager/issues/2482
    bat = {
      enable = true;
      config = {
        theme = "catppuccin-${user.theme}";
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
        cn = "checkout --detach";
        d = "diff";
        di = "diff ':(exclude)./**/package-lock.json' ':(exclude)./**/yarn.lock'";
        dc = "diff --cached";
        dci = "diff --cached ':(exclude)./**/package-lock.json' ':(exclude)./**/yarn.lock'";
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
          excludesfile = "~/.gitignore_global";
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

    helix = {
      enable = true;
      package = inputs.helix.packages.${system}.default;
      languages = [
        {
          name = "markdown";
          language-server.command = "ltex-ls";
        }
      ];
      settings = {
        theme = "catppuccin_${user.theme}";
        keys.normal = {
          C-h = "jump_view_left";
          C-j = "jump_view_down";
          C-k = "jump_view_up";
          C-l = "jump_view_right";
          space.t = ":tree-sitter-subtree";
        };
        editor = {
          idle-timeout = 0;
          cursor-shape = {
            normal = "block";
            insert = "bar";
            select = "underline";
          };
          whitespace.render.tab = "all";
        };
      };
    };

    home-manager.enable = true; # Let Home Manager install and manage itself.

    kitty = {
      enable = true;
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
