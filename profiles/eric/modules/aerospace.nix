{pkgs, ...}: {
  programs.aerospace = {
    enable = true;
    launchd.enable = true;
    settings = {
      mode = {
        main.binding = {
          alt-h = "focus left";
          alt-j = "focus down";
          alt-k = "focus up";
          alt-l = "focus right";
          alt-shift-h = "move left";
          alt-shift-j = "move down";
          alt-shift-k = "move up";
          alt-shift-l = "move right";
          alt-ctrl-h = "move-node-to-monitor left";
          alt-ctrl-j = "move-node-to-monitor down";
          alt-ctrl-k = "move-node-to-monitor up";
          alt-ctrl-l = "move-node-to-monitor right";
          alt-minus = "resize smart -50";
          alt-equal = "resize smart +50";
          alt-shift-equal = "balance-sizes";
          alt-1 = "workspace 1";
          alt-2 = "workspace 2";
          alt-3 = "workspace 3";
          alt-4 = "workspace 4";
          alt-5 = "workspace 5";
          alt-6 = "workspace 6";
          alt-7 = "workspace 7";
          alt-8 = "workspace 8";
          alt-9 = "workspace 9";
          alt-0 = "workspace 0";
          alt-shift-1 = "move-node-to-workspace 1";
          alt-shift-2 = "move-node-to-workspace 2";
          alt-shift-3 = "move-node-to-workspace 3";
          alt-shift-4 = "move-node-to-workspace 4";
          alt-shift-5 = "move-node-to-workspace 5";
          alt-shift-6 = "move-node-to-workspace 6";
          alt-shift-7 = "move-node-to-workspace 7";
          alt-shift-8 = "move-node-to-workspace 8";
          alt-shift-9 = "move-node-to-workspace 9";
          alt-shift-0 = "move-node-to-workspace 0";
          alt-shift-semicolon = "mode service";
          alt-comma = "layout tiles horizontal vertical";
          alt-slash = "layout accordion horizontal";
          alt-shift-enter = ''
            exec-and-forget osascript \
              -e 'tell application "kitty" to activate' \
              -e 'tell application "System Events" to keystroke "n" using {command down}'
          '';
          alt-shift-backslash = ''
            exec-and-forget ${pkgs.aerospace}/bin/aerospace list-windows --all --json \
              | ${pkgs.jq}/bin/jq -r '.[] | select(."window-title"=="") | ."window-id"' \
              | xargs -n1 ${pkgs.aerospace}/bin/aerospace close --window-id
          '';
        };
        service.binding = {
          esc = ["reload-config" "mode main"];
          r = ["flatten-workspace-tree" "mode main"];
          f = ["layout floating tiling" "mode main"];
          alt-shift-h = ["join-with left" "mode main"];
          alt-shift-j = ["join-with down" "mode main"];
          alt-shift-k = ["join-with up" "mode main"];
          alt-shift-l = ["join-with right" "mode main"];
        };
      };
      on-window-detected = [
        {
          "if".app-id = "com.apple.finder";
          run = "layout floating";
        }
        {
          "if".app-id = "com.apple.ActivityMonitor";
          run = "layout floating";
        }
      ];
      workspace-to-monitor-force-assignment = {
        "1" = "dell u2718q";
        "2" = "dell u2718q";
        "3" = "dell u2718q";
        "4" = "dell u2718q";
        "5" = "built-in";
        "6" = "built-in";
        "7" = "built-in";
        "8" = "dell u2723qe";
        "9" = "dell u2723qe";
        "0" = "dell u2723qe";
      };
    };
  };
}
