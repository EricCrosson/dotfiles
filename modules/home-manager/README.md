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
