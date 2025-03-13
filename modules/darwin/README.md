# launchd-with-logs

This module provides an idiomatic Nix interface for defining launchd agents with automatic log rotation via newsyslog in Home Manager.

## Features

- Simplified interface for creating launchd agents
- Automatic configuration of log rotation via newsyslog
- Default values for common settings
- Typed options with descriptions
- Integrated with Home Manager

## Usage

1. Import the module in your system configuration:

```nix
{
  imports = [
    ./path/to/launchd-with-logs.nix
  ];
}
```

2. Configure your launchd agents in your Home Manager configuration:

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

| Option  | Type   | Default | Description                                               |
| ------- | ------ | ------- | --------------------------------------------------------- |
| `count` | int    | 5       | Number of rotated logs to keep                            |
| `size`  | int    | 512     | Size in KB at which to rotate logs                        |
| `when`  | string | "\*"    | When to rotate logs (`*` means any time size is exceeded) |
| `flags` | string | "C"     | Rotation flags (C=create, Z=compress)                     |

## Example

Here's a complete example showing how to define a launchd agent with logged output in your Home Manager configuration:

```nix
{
  launchd-with-logs = {
    enable = true;
    services = {
      my-background-job = {
        command = "${pkgs.my-package}/bin/background-job";
        environment = {
          API_KEY_PATH = "${config.sops.secrets.api_key.path}";
        };
        interval = 3600;  # Run hourly
        serviceDependencies = ["sops-nix"];
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
}
```

For services that don't need logging, simply omit the logging configuration:

```nix
keychain-ssh = {
  command = "${pkgs.bash}/bin/bash";
  args = [
    "-c"
    "eval \"$(${pkgs.keychain}/bin/keychain --eval --agents ssh --quiet id_rsa)\""
  ];
  # No logging configuration means stdout and stderr go to /dev/null
};
```

## Integration with Home Manager

This module is designed to work within Home Manager's configuration system. The actual implementation:

1. Creates launchd agent configurations for each user's services
2. Consolidates all log rotation configurations into a single newsyslog.d file
3. Ensures proper permissions for log files

When you define services in your Home Manager configuration, they'll automatically be:

- Set up as launchd agents
- Configured with the proper logging paths
- Added to the newsyslog rotation configuration

This eliminates the need to manually manage log rotation configurations separate from your service definitions.
