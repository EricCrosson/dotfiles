# Home Manager Modules

## launchd-with-logs

A Home Manager module for defining launchd agents with automatic log rotation via newsyslog in a clean, declarative way.

### Features

- Simplified interface for creating launchd agents
- Automatic configuration of log rotation via newsyslog
- Default values for common settings
- Typed options with descriptions
- Integrated with Home Manager

### Usage

1. Import the module in your Home Manager configuration:

```nix
{
  imports = [
    ./path/to/modules/home-manager/launchd-with-logs.nix
  ];
}
```

2. Configure your launchd agents:

```nix
{
  launchd-with-logs = {
    enable = true;
    services = {
      my-service = {
        command = "${pkgs.myPackage}/bin/my-command";
        args = ["--option" "value"];
        environment = {
          MY_ENV_VAR = "value";
        };
        interval = 300; # Run every 5 minutes
        logging = {
          stdout = "${config.home.homeDirectory}/Library/Logs/my-service.log";
          stderr = "${config.home.homeDirectory}/Library/Logs/my-service.error.log";
          rotation = {
            count = 7;      # Keep 7 rotated logs
            size = 786432;  # Rotate at 768MB
            when = "$D0";   # Rotate daily at midnight
            flags = "C";    # Create log file if it doesn't exist
          };
        };
      };
    };
  };
}
```

See the full documentation in the module's source file or in the [module documentation](launchd-with-logs.md).

## programs.rift

A Home Manager module for the [Rift](https://github.com/acsandmann/rift) tiling
window manager (`pkgs.rift-wm`). There is no upstream module, so this one
follows the shape of Home Manager's `programs.aerospace`: free-form `settings`
rendered to `~/.config/rift/config.toml`, plus a launchd agent.

### Features

- Renders `programs.rift.settings` to `~/.config/rift/config.toml` (Rift reads
  that exact path and ignores `XDG_CONFIG_HOME`); the file is skipped while
  `settings` is empty and Rift's bundled defaults apply
- Runs the daemon through `launchd-with-logs`, logging to
  `~/Library/Logs/rift.log` and `~/Library/Logs/rift.error.log` with daily
  rotation
- Reloads a running daemon (`rift-cli execute config reload`) when the
  generated config changes
- Installs `rift` and `rift-cli` from `pkgs.rift-wm`

### Prerequisites

- "Displays have separate Spaces" must be enabled; Rift exits at launch
  otherwise. With nix-darwin: `system.defaults.spaces.spans-displays = false;`
- macOS Accessibility permission must be granted to Rift. Until it is, the
  daemon exits; restart the agent after granting:
  `launchctl kickstart -k gui/$(id -u)/org.nix-community.home.rift`
- If Rift was ever installed with `rift service install`, run
  `rift service uninstall` first so two daemons do not compete

### Usage

```nix
{
  programs.rift = {
    enable = true;
    settings = {
      settings.layout.mode = "traditional";
      keys = {
        "Alt + H" = {move_focus = "left";};
        "Alt + Z" = "toggle_space_activated";
      };
    };
  };
}
```

See `profiles/eric/modules/rift.nix` for the full keyboard-driven
configuration and the
[configuration reference](https://acsandmann.github.io/rift-docs/reference/configuration/)
for every setting. Rift rejects unknown fields, so keep keys exactly as
documented for the installed version.
