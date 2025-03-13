# launchd-with-logs

This Home Manager module provides an idiomatic Nix interface for defining launchd agents with automatic log rotation via newsyslog on macOS.

## Features

- Simplified interface for creating launchd agents
- Automatic configuration of log rotation via newsyslog
- Default values for common settings
- Typed options with descriptions
- Integrated with Home Manager

## Usage

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
            count = 5;    # Keep 5 rotated logs
            size = 512;   # Rotate at 512KB
            when = "*";   # Rotate anytime the size is exceeded
            flags = "C";  # Create log file if it doesn't exist
          };
        };
      };
    };
  };
}
```

## Options

### Top-level Options

| Option     | Type          | Description                          |
| ---------- | ------------- | ------------------------------------ |
| `enable`   | boolean       | Whether to enable the module         |
| `services` | attribute set | Set of launchd services to configure |

### Service Options

| Option                | Type            | Default | Description                            |
| --------------------- | --------------- | ------- | -------------------------------------- |
| `enable`              | boolean         | `true`  | Whether to enable this service         |
| `command`             | string          |         | The command to run                     |
| `args`                | list of strings | `[]`    | Arguments to pass to the command       |
| `environment`         | attribute set   | `{}`    | Environment variables for the service  |
| `interval`            | int or null     | `null`  | Start interval in seconds              |
| `keepAlive`           | boolean         | `false` | Whether to keep the service alive      |
| `runAtLoad`           | boolean         | `true`  | Whether to run the service when loaded |
| `workingDirectory`    | string or null  | `null`  | Working directory for the service      |
| `serviceDependencies` | list of strings | `[]`    | Services this service depends on       |

### Logging Options

| Option     | Type           | Default | Description                                 |
| ---------- | -------------- | ------- | ------------------------------------------- |
| `stdout`   | string or null | `null`  | Path for stdout logs (null means /dev/null) |
| `stderr`   | string or null | `null`  | Path for stderr logs (null means /dev/null) |
| `rotation` | attribute set  | `{}`    | Log rotation settings                       |

### Log Rotation Options

| Option  | Type   | Default | Description                                         |
| ------- | ------ | ------- | --------------------------------------------------- |
| `count` | int    | 7       | Number of rotated logs to keep                      |
| `size`  | int    | 786432  | Size in KB (768 MB) at which to rotate logs         |
| `when`  | string | "$D0"   | When to rotate logs (`$D0` means daily at midnight) |
| `flags` | string | "C"     | Rotation flags (C=create, Z=compress)               |

## Examples

### Basic Service with Log Rotation

```nix
launchd-with-logs = {
  enable = true;
  services = {
    my-background-job = {
      command = "${pkgs.my-package}/bin/background-job";
      interval = 3600;  # Run hourly
      logging = {
        stdout = "${config.home.homeDirectory}/Library/Logs/background-job.log";
        stderr = "${config.home.homeDirectory}/Library/Logs/background-job.error.log";
        rotation = {
          count = 7;   # Keep a week's worth of logs
          size = 1024; # Rotate at 1MB
        };
      };
    };
  };
};
```

### Service with Environment Variables and Dependencies

```nix
launchd-with-logs = {
  enable = true;
  services = {
    api-service = {
      command = "${pkgs.my-api-service}/bin/api-service";
      args = ["--config" "${config.home.homeDirectory}/.config/api-service/config.yaml"];
      environment = {
        API_KEY_PATH = "${config.sops.secrets.api_key.path}";
        DATABASE_URL = "postgresql://user:pass@localhost:5432/db";
      };
      keepAlive = true;
      serviceDependencies = ["sops-nix" "org.postgresql.postgres"];
      logging = {
        stdout = "${config.home.homeDirectory}/Library/Logs/api-service.log";
        stderr = "${config.home.homeDirectory}/Library/Logs/api-service.error.log";
        rotation = {
          count = 10;
          size = 2048; # 2MB
          flags = "CZ"; # Create if not exists and compress
        };
      };
    };
  };
};
```

### Service with No Logging (outputs to /dev/null)

```nix
launchd-with-logs = {
  enable = true;
  services = {
    keychain-ssh = {
      command = "${pkgs.bash}/bin/bash";
      args = [
        "-c"
        "eval \"$(${pkgs.keychain}/bin/keychain --eval --agents ssh --quiet id_rsa)\""
      ];
      # No logging configuration means stdout and stderr go to /dev/null
    };
  };
};
```

## Implementation Notes

This module:

1. Converts the `launchd-with-logs` service definitions into standard Home Manager `launchd.agents` definitions
2. Creates a newsyslog configuration file in `~/.config/newsyslog-launchd-with-logs.conf` for log rotation
3. Ensures the `~/Library/Logs` directory exists

The module is designed to work seamlessly with Home Manager and provide a more convenient interface for defining launchd services with logged outputs.
