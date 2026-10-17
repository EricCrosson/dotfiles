_: {
  programs.rift = {
    enable = true;
    launchd.enable = true;

    # G HUB plays macros by posting keystrokes at the session event point,
    # which rift 0.6.x's HID-level hotkey tap never sees; relay them across.
    hotkeyBridge.enable = true;

    settings = {
      settings = {
        # No layout-change animations; hovering never steals focus and the
        # pointer never follows keyboard focus.
        animate = false;
        focus_follows_mouse = false;
        mouse_follows_focus = false;
        mouse_hides_on_focus = false;

        # Manage every macOS Space from the start rather than waiting for
        # Rift's own per-Space activation toggle (alt-z).
        default_disable = false;
        layout = {
          mode = "traditional";
          traditional = {
            equalize_nodes = true;
          };
          stack = {
            stack_offset = 40.0;
            default_orientation = "horizontal";
          };
        };

        # Apps that should not steal focus across workspaces.
        auto_focus_blacklist = [
          "com.apple.dock"
          "com.apple.systemuiserver"
          "com.apple.Spotlight"
        ];

        # Native macOS menu-bar workspace indicator: a status item on each
        # display's menu bar, labeling every workspace by number so you can
        # see which workspace/window a display is showing at a glance.
        ui.menu_bar = {
          enabled = true;
          # Show every workspace, not just the active one.
          mode = "all";
          # A number/label chip is easier to read at a glance than a miniature
          # layout, and we have 10 workspaces.
          display_style = "label";
          active_label = "index";
        };

        hot_reload = true;
      };

      virtual_workspaces = {
        enabled = true;
        default_workspace_count = 10;
        auto_assign_windows = true;
        preserve_focus_per_workspace = true;

        # Workspace indexes are zero-based. There is no per-workspace monitor
        # pinning; move windows between displays with alt-ctrl-h/j/k/l instead.
        app_rules = [
          {
            app_id = "com.tinyspeck.slackmacgap";
            workspace = 0;
          }
          {
            app_id = "com.goodsnooze.MacWhisper";
            workspace = 0;
            floating = true;
          }
          {
            app_id = "com.chocoford.excalidraw";
            floating = false;
          }
          {
            app_id = "com.apple.finder";
            floating = true;
          }
          {
            app_id = "com.apple.ActivityMonitor";
            floating = true;
          }
          {
            app_id = "com.apple.DiskUtility";
            floating = true;
          }
          {
            app_id = "com.apple.systempreferences";
            floating = true;
          }
          {
            app_id = "com.1password.1password";
            floating = true;
          }
          {
            app_id = "com.apple.keychainaccess";
            floating = true;
          }
          {
            app_id = "com.yubico.yubioath";
            floating = true;
          }
          {
            app_id = "org.pqrs.Karabiner-Elements.Preferences";
            floating = true;
          }
          {
            app_id = "com.apple.Notes";
            floating = true;
          }
          {
            app_id = "com.apple.reminders";
            floating = true;
          }
          {
            app_id = "com.apple.VoiceMemos";
            floating = true;
          }
          {
            app_id = "com.apple.clock";
            floating = true;
          }
          {
            app_id = "com.apple.dt.Xcode";
            floating = true;
          }
          {
            app_id = "com.pais.handy";
            floating = true;
          }
        ];
      };

      # Reload is automatic (hot_reload plus the module's activation hook);
      # there is no service mode to toggle.
      keys = {
        "Alt + Tab" = "switch_to_last_workspace";

        "Alt + H" = {move_focus = "left";};
        "Alt + J" = {move_focus = "down";};
        "Alt + K" = {move_focus = "up";};
        "Alt + L" = {move_focus = "right";};

        "Alt + Shift + H" = {move_node = "left";};
        "Alt + Shift + J" = {move_node = "down";};
        "Alt + Shift + K" = {move_node = "up";};
        "Alt + Shift + L" = {move_node = "right";};

        "Alt + Ctrl + H" = {move_window_to_display = {selector = "left";};};
        "Alt + Ctrl + J" = {move_window_to_display = {selector = "down";};};
        "Alt + Ctrl + K" = {move_window_to_display = {selector = "up";};};
        "Alt + Ctrl + L" = {move_window_to_display = {selector = "right";};};

        "Alt + Minus" = {resize_window_shrink = "smart";};
        "Alt + Equal" = {resize_window_grow = "smart";};

        "Alt + 1" = {switch_to_workspace = 0;};
        "Alt + 2" = {switch_to_workspace = 1;};
        "Alt + 3" = {switch_to_workspace = 2;};
        "Alt + 4" = {switch_to_workspace = 3;};
        "Alt + 5" = {switch_to_workspace = 4;};
        "Alt + 6" = {switch_to_workspace = 5;};
        "Alt + 7" = {switch_to_workspace = 6;};
        "Alt + 8" = {switch_to_workspace = 7;};
        "Alt + 9" = {switch_to_workspace = 8;};
        "Alt + 0" = {switch_to_workspace = 9;};

        "Alt + Shift + 1" = {move_window_to_workspace = 0;};
        "Alt + Shift + 2" = {move_window_to_workspace = 1;};
        "Alt + Shift + 3" = {move_window_to_workspace = 2;};
        "Alt + Shift + 4" = {move_window_to_workspace = 3;};
        "Alt + Shift + 5" = {move_window_to_workspace = 4;};
        "Alt + Shift + 6" = {move_window_to_workspace = 5;};
        "Alt + Shift + 7" = {move_window_to_workspace = 6;};
        "Alt + Shift + 8" = {move_window_to_workspace = 7;};
        "Alt + Shift + 9" = {move_window_to_workspace = 8;};
        "Alt + Shift + 0" = {move_window_to_workspace = 9;};

        "Alt + Comma" = {
          exec = [
            "sh"
            "-c"
            ''
              layout="$(rift-cli query layout 2>/dev/null)" || exit 0
              fs=$(echo "$layout" | jq -r '[.. | objects | select(.node_type == "window" and (.is_fullscreen == true or .is_fullscreen_within_gaps == true))][0] | if . == null then empty else "\(.window_id.pid) \(.window_id.idx) \(if .is_fullscreen == true then "full" else "gaps" end)" end')
              mode=$(echo "$layout" | jq -r .mode)
              count=$(echo "$layout" | jq -r '.tiled_windows | length')

              if [ -n "$fs" ]; then
                # toggle-fullscreen only ever acts on the selected window, so
                # focus the stuck window first and verify it owns the
                # selection; otherwise this press would toggle the wrong
                # window into fullscreen. Each flag needs its matching toggle:
                # toggle-fullscreen on a gaps-fullscreen window converts it to
                # regular fullscreen instead of releasing it.
                set -- $fs
                rift-cli execute window focus --window-id "{\"pid\":$1,\"idx\":$2}"
                layout="$(rift-cli query layout 2>/dev/null)" || exit 0
                sel=$(echo "$layout" | jq -r '[.. | objects | select(.node_type == "window" and .is_selected == true)][0] | if . == null then empty else "\(.window_id.pid) \(.window_id.idx)" end')
                if [ "$sel" = "$1 $2" ]; then
                  if [ "$3" = "full" ]; then
                    rift-cli execute window toggle-fullscreen
                  else
                    rift-cli execute window toggle-fullscreen-within-gaps
                  fi
                fi
              elif [ "$mode" = "stack" ]; then
                rift-cli execute workspace set-layout traditional
              elif [ "$count" -gt 1 ]; then
                rift-cli execute workspace set-layout stack
              fi
            ''
          ];
        };
        "Alt + Shift + Comma" = "toggle_orientation";
        "Alt + Shift + E" = "unjoin_windows";
        "Alt + Shift + Space" = "toggle_window_floating";

        # Join the focused window with its nearest neighbor in that
        # direction into one group. One row above the H/J/K/L move keys:
        # Y=left, U=down, I=up, O=right.
        "Alt + Shift + Y" = {join_window = "left";};
        "Alt + Shift + U" = {join_window = "down";};
        "Alt + Shift + I" = {join_window = "up";};
        "Alt + Shift + O" = {join_window = "right";};

        # Get-me-out-of-jail: rebuild the active workspace as a flat,
        # equalized vertical strip. Round-tripping bsp -> traditional rebuilds
        # the tree with every window as an equal-weight sibling, and
        # toggle-orientation flips the root container from horizontal to
        # vertical. With 5+ windows Rift's smart insertion may nest small
        # sub-containers instead of keeping everything at the root.
        "Alt + Shift + R" = {
          exec = [
            "sh"
            "-c"
            "rift-cli execute workspace set-layout bsp && rift-cli execute workspace set-layout traditional"
          ];
        };
      };
    };
  };
}
