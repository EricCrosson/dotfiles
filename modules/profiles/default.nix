{lib, ...}:
with lib; let
  # Default values and shared settings
  defaultPreferences = {
    theme = "Mocha";
  };
in {
  ###### interface

  options = {
    # Define the top-level profiles module
    profiles = mkOption {
      type = types.attrsOf (types.submodule ({config, ...}: {
        options = {
          username = mkOption {
            type = types.str;
            description = ''
              The login name of the user. This is used to identify the user's
              profile in the system and is referenced by various services.

              This should correspond to the actual username on the system.
            '';
            example = "alice";
          };

          email = mkOption {
            type = types.str;
            description = ''
              Email address associated with this profile.

              This is used for various services that require an email address,
              such as Git configuration.
            '';
            example = "alice@example.com";
          };

          organization = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              The organization or company associated with this profile.

              This is used to determine which organization-specific configurations
              to apply. If null, assumes a personal profile.
            '';
            example = "acme";
          };

          homeDirectory = mkOption {
            type = types.str;
            description = ''
              Absolute path to the user's home directory.

              This is used by home-manager and other services that need to know
              the location of the user's home directory. The path must be absolute.
            '';
            default = lib.mkDefault (
              if stdenv.isDarwin
              then "/Users/${config.username}"
              else "/home/${config.username}"
            );
            example = "On macOS: /Users/alice, On Linux: /home/alice";
          };

          preferences = mkOption {
            type = types.attrs;
            default = defaultPreferences;
            description = ''
              User preferences that affect configuration options.

              These preferences are used across the configuration to maintain
              consistent styling and behavior across different applications.

              Current preferences:
                - theme: The theme name (e.g., "Mocha", "Latte", etc.)
            '';
            example = literalExpression ''
              {
                theme = "Latte";  # Light theme
              }
            '';
          };
        };
      }));
      description = ''
        User profile definitions.

        A profile represents a collection of settings for a specific user context.
        Profiles can be specific to an organization or personal use and define
        various attributes used across the configuration.

        Each profile defines attributes like username, email, organization, etc.,
        which are then used by various parts of the configuration to customize
        behavior based on the active profile.
      '';
      default = {
        preferences = {
          theme = "Mocha";
        };
      };
      example = literalExpression ''
        {
          work = {
            username = "john";
            email = "john@company.com";
            organization = "company";
            homeDirectory = "/Users/john";
            preferences.theme = "Frappe";
          };

          personal = {
            username = "john";
            email = "john@example.com";
            homeDirectory = "/Users/john";
            # organization defaults to null
            # preferences defaults to the default preferences
          };
        }
      '';
    };
  };
}
