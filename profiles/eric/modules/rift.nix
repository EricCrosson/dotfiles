_: {
  programs.rift = {
    enable = true;
    launchd.enable = true;

    settings = {
      settings = {
        # Rift's bundled defaults animate layout changes and move the mouse
        # with focus; AeroSpace did neither.
        animate = false;
        focus_follows_mouse = false;
        mouse_follows_focus = true;
        mouse_hides_on_focus = false;

        # Manage every macOS Space from the start. Rift's own default is to
        # wait for a per-Space activation toggle (alt-z), but AeroSpace tiled
        # everywhere.
        default_disable = false;
        layout.mode = "traditional";
        layout.traditional = {
          equalize_nodes = true;
        };

        # Apps that should not steal focus across workspaces.
        auto_focus_blacklist = [
          "com.apple.dock"
          "com.apple.systemuiserver"
          "com.apple.Spotlight"
        ];

        # Native macOS menu-bar workspace indicator. AeroSpace had no built-in
        # statusbar; this is Rift's equivalent (a status item on each display's
        # menu bar), labeling every workspace by number so you can see which
        # workspace/window a display is showing at a glance.
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

        # Workspace indexes are zero-based: AeroSpace workspace "1" is 0 here.
        # Rift has no equivalent of AeroSpace's
        # workspace-to-monitor-force-assignment; move windows between displays
        # with alt-ctrl-h/j/k/l instead.
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

      modifier_combinations.comb1 = "Alt + Shift";

      # AeroSpace's service mode (alt-shift-semicolon) has no Rift equivalent;
      # reload is automatic (hot_reload plus the module's activation hook).
      keys = {
        "Alt + Z" = "toggle_space_activated";
        "Alt + Tab" = "switch_to_last_workspace";

        "Alt + H" = {move_focus = "left";};
        "Alt + J" = {move_focus = "down";};
        "Alt + K" = {move_focus = "up";};
        "Alt + L" = {move_focus = "right";};

        "comb1 + H" = {move_node = "left";};
        "comb1 + J" = {move_node = "down";};
        "comb1 + K" = {move_node = "up";};
        "comb1 + L" = {move_node = "right";};

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

        "comb1 + 1" = {move_window_to_workspace = 0;};
        "comb1 + 2" = {move_window_to_workspace = 1;};
        "comb1 + 3" = {move_window_to_workspace = 2;};
        "comb1 + 4" = {move_window_to_workspace = 3;};
        "comb1 + 5" = {move_window_to_workspace = 4;};
        "comb1 + 6" = {move_window_to_workspace = 5;};
        "comb1 + 7" = {move_window_to_workspace = 6;};
        "comb1 + 8" = {move_window_to_workspace = 7;};
        "comb1 + 9" = {move_window_to_workspace = 8;};
        "comb1 + 0" = {move_window_to_workspace = 9;};

        "Alt + Comma" = "toggle_stack";
        "comb1 + Comma" = "toggle_orientation";
        "comb1 + Space" = "toggle_window_floating";

        # Join the focused window with its nearest neighbor in that
        # direction into one group. One row above the H/J/K/L move keys:
        # Y=left, U=down, I=up, O=right.
        "comb1 + Y" = {join_window = "left";};
        "comb1 + U" = {join_window = "down";};
        "comb1 + I" = {join_window = "up";};
        "comb1 + O" = {join_window = "right";};
      };
    };
  };
}
