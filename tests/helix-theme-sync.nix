{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertHasAttr;

  # Helper to evaluate the module with a given config
  eval = testConfig:
    (lib.evalModules {
      modules = [
        # The module under test
        ../modules/home-manager/services/helix-theme-sync/default.nix

        # Stub option declarations for outputs the module writes to
        {
          options = {
            home = {
              homeDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser";
              };
              activation = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = {};
              };
            };
            launchd-with-logs.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
          };
        }

        # Test-specific configuration
        testConfig
      ];
      specialArgs = {
        inherit pkgs;
        inputs = {};
      };
    })
    .config;

  # === Test cases ===

  # Test: Service registers a launchd-with-logs service when enabled
  test-service-registered = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
  in
    assert assertHasAttr "service-registered" result.launchd-with-logs.services "helix-theme-sync"; true;

  # Test: Service command references the sync script
  test-service-command = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
    service = result.launchd-with-logs.services.helix-theme-sync;
  in
    assert assertContains "command-ref" service.command "helix-theme-sync"; true;

  # Test: Activation hook is created when enabled
  test-activation-hook = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
  in
    assert assertHasAttr "activation-hook" result.home.activation "syncHelixTheme"; true;

  # Test: No service or activation when disabled
  test-disabled = let
    result = eval {
      services.helix-theme-sync = {
        enable = false;
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
  in
    assert !(result.launchd-with-logs.services ? helix-theme-sync);
    assert !(result.home.activation ? syncHelixTheme); true;
in
  assert test-service-registered;
  assert test-service-command;
  assert test-activation-hook;
  assert test-disabled; "all tests passed"
