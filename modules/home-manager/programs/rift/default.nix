{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.rift;

  tomlFormat = pkgs.formats.toml {};

  # Rift resolves its config as $HOME/.config/rift/config.toml via dirs::home_dir(),
  # ignoring XDG_CONFIG_HOME, so xdg.configFile would install it to the wrong path.
  configPath = ".config/rift/config.toml";
in {
  options.programs.rift = {
    enable = mkEnableOption "Rift window manager";

    package = mkPackageOption pkgs "rift-wm" {};

    launchd = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Configure the launchd agent that runs the Rift daemon.

          Rift needs macOS Accessibility permission (System Settings →
          Privacy & Security → Accessibility) and exits on first launch until
          it is granted; restart the agent afterwards with
          `launchctl kickstart -k gui/$(id -u)/org.nix-community.home.rift`.

          It also requires "Displays have separate Spaces" (System Settings →
          Desktop & Dock → Mission Control). With nix-darwin set
          `system.defaults.spaces.spans-displays = false;`.

          Verify the agent with `launchctl list | grep rift`; logs are written
          to ~/Library/Logs/rift.log and ~/Library/Logs/rift.error.log.

          If Rift was previously installed with `rift service install`, run
          `rift service uninstall` first to avoid running two instances.
        '';
      };

      keepAlive = mkOption {
        type = types.bool;
        default = true;
        description = "Whether launchd should restart the Rift daemon when it exits.";
      };
    };

    hotkeyBridge = {
      enable = mkEnableOption ''
        a bridge that lets synthetic keystrokes (for example Logitech G HUB
        macro playback) trigger rift hotkeys.

        Rift 0.6.x listens for hotkeys at the macOS HID event point, which
        keystrokes posted at the session event point never reach; affected
        combos fall through to the focused application instead (Option+Comma
        types "≤" on a US layout). The bridge listens at the annotated-session
        event point, consumes matching keydowns, and re-posts them at the HID
        point, where rift dispatches and consumes them normally. Rift and its
        configuration stay untouched. Note that it relays the configured
        combos from any synthetic sender, not just G HUB.

        The bridge needs macOS Accessibility permission for
        rift-hotkey-bridge; until it is granted the agent exits and launchd
        restarts it. Verify with `launchctl list | grep rift-hotkey-bridge`;
        logs are written to ~/Library/Logs/rift-hotkey-bridge.log and
        ~/Library/Logs/rift-hotkey-bridge.error.log.
      '';

      package = mkOption {
        type = types.package;
        default = pkgs.callPackage ./hotkey-bridge.nix {};
        defaultText = literalExpression "pkgs.callPackage ./hotkey-bridge.nix {}";
        description = "The rift-hotkey-bridge package to run.";
      };

      keepAlive = mkOption {
        type = types.bool;
        default = true;
        description = "Whether launchd should restart the bridge when it exits.";
      };

      relays = mkOption {
        type = types.listOf types.str;
        default = ["43:alt"];
        example = ["43:alt" "7:cmd"];
        description = ''
          Combos to relay from the session event point to the HID event point,
          as `KEYCODE:MODIFIERS` with macOS virtual keycodes and
          comma-separated modifiers (alt|shift|ctrl|cmd). Keycode 43 is
          Comma, so the default relays Option+Comma. Keep this list in sync
          with the matching bindings under `settings.keys`; a relayed combo
          that rift no longer binds falls through to the focused application.
        '';
      };
    };

    settings = mkOption {
      inherit (tomlFormat) type;
      default = {};
      example = {
        settings = {
          animate = false;
          layout.mode = "traditional";
        };
        virtual_workspaces = {
          default_workspace_count = 10;
          app_rules = [
            {
              app_id = "com.apple.Safari";
              workspace = 0;
            }
          ];
        };
        modifier_combinations.comb1 = "Alt + Shift";
        keys = {
          "Alt + Z" = "toggle_space_activated";
          "Alt + H" = {move_focus = "left";};
          "comb1 + H" = {move_node = "left";};
        };
      };
      description = ''
        Rift configuration written to ~/.config/rift/config.toml.

        See the configuration reference at
        <https://acsandmann.github.io/rift-docs/reference/configuration/>
        for supported values. The file is only written when this option is
        non-empty; without it Rift uses its bundled configuration.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "programs.rift" pkgs lib.platforms.darwin)
    ];

    home = {
      packages = [cfg.package];

      file.${configPath} = mkIf (cfg.settings != {}) {
        source = tomlFormat.generate "rift-config.toml" cfg.settings;

        onChange = ''
          if ${lib.getExe' cfg.package "rift-cli"} query displays >/dev/null 2>&1; then
            ${lib.getExe' cfg.package "rift-cli"} execute config reload
          else
            echo "Rift is not running yet, skipping config reload."
          fi
        '';
      };
    };

    launchd-with-logs.services = mkMerge [
      (mkIf cfg.launchd.enable {
        rift = {
          enable = true;
          command = lib.getExe cfg.package;
          inherit (cfg.launchd) keepAlive;
          runAtLoad = true;

          environment = {
            # Mirror the environment `rift service install` writes.
            RUST_LOG = "error,warn,info";
            PATH = concatStringsSep ":" [
              "${config.home.profileDirectory}/bin"
              "/run/current-system/sw/bin"
              "/usr/bin"
              "/bin"
              "/usr/sbin"
              "/sbin"
            ];
          };

          logging = {
            stdout = "${config.home.homeDirectory}/Library/Logs/rift.log";
            stderr = "${config.home.homeDirectory}/Library/Logs/rift.error.log";
          };
        };
      })

      (mkIf cfg.hotkeyBridge.enable {
        rift-hotkey-bridge = {
          enable = true;
          command = lib.getExe cfg.hotkeyBridge.package;
          args = map (relay: "--relay=${relay}") cfg.hotkeyBridge.relays;
          inherit (cfg.hotkeyBridge) keepAlive;
          runAtLoad = true;

          environment = {
            RUST_LOG = "error,warn,info";
          };

          logging = {
            stdout = "${config.home.homeDirectory}/Library/Logs/rift-hotkey-bridge.log";
            stderr = "${config.home.homeDirectory}/Library/Logs/rift-hotkey-bridge.error.log";
          };
        };
      })
    ];
  };
}
