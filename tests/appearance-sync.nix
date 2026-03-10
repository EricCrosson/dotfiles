{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertHasAttr assertEq;

  extendedLib =
    lib
    // {
      hm = {
        dag = {
          entryAfter = deps: data: {inherit deps data;};
        };
      };
    };

  darwinPkgs =
    pkgs
    // {
      stdenv =
        pkgs.stdenv
        // {
          isDarwin = true;
        };
    };

  eval = testConfig:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/services/appearance-sync/default.nix

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

        testConfig
      ];
      specialArgs = {
        lib = extendedLib;
        pkgs = darwinPkgs;
      };
    })
    .config;

  # Test: Service produces expected launchd-with-logs entry
  test-launchd-service-registered = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
  in
    assert assertHasAttr "launchd-registered" result.launchd-with-logs.services "test-svc"; true;

  # Test: launchd-with-logs entry has correct properties
  test-launchd-service-properties = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
    svc = result.launchd-with-logs.services.test-svc;
  in
    assert assertContains "command" svc.command "test-svc";
    assert assertEq "runAtLoad" svc.runAtLoad true;
    assert assertEq "watchPaths" svc.watchPaths ["/home/testuser/Library/Preferences/.GlobalPreferences.plist"];
    assert assertContains "stderr-log" svc.logging.stderr "test-svc.error.log"; true;

  # Test: Activation hook is created
  test-activation-hook = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
  in
    assert assertHasAttr "activation-hook" result.home.activation "test-svc"; true;

  # Test: Activation hook deletes state file before running script
  test-activation-deletes-state = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
    hook = result.home.activation.test-svc;
  in
    assert assertContains "rm-state" hook.data "rm -f";
    assert assertContains "state-path" hook.data "appearance-sync/test-svc"; true;

  # Test: Generated script contains appearance detection
  test-script-appearance-detection = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
    svc = result.launchd-with-logs.services.test-svc;
    script = builtins.readFile svc.command;
  in
    assert assertContains "appearance-check" script "AppleInterfaceStyle"; true;

  # Test: Generated script contains idempotency state file check
  test-script-idempotency = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
    svc = result.launchd-with-logs.services.test-svc;
    script = builtins.readFile svc.command;
  in
    assert assertContains "state-file" script "appearance-sync/test-svc"; true;

  # Test: Generated script contains onLight and onDark content
  test-script-fragments = let
    result = eval {
      appearance-sync.services.test-svc = {
        onLight = "echo LIGHT_MARKER";
        onDark = "echo DARK_MARKER";
      };
    };
    svc = result.launchd-with-logs.services.test-svc;
    script = builtins.readFile svc.command;
  in
    assert assertContains "light-fragment" script "LIGHT_MARKER";
    assert assertContains "dark-fragment" script "DARK_MARKER"; true;

  # Test: Empty services produces no launchd-with-logs entries
  test-empty-services = let
    result = eval {};
  in
    assert assertEq "no-services" result.launchd-with-logs.services {}; true;

  # Test: enable = false produces no config
  test-disabled = let
    result = eval {
      appearance-sync.services.test-svc = {
        enable = false;
        onLight = "echo light";
        onDark = "echo dark";
      };
    };
  in
    assert !(result.launchd-with-logs.services ? test-svc);
    assert !(result.home.activation ? test-svc); true;
in
  assert test-launchd-service-registered;
  assert test-launchd-service-properties;
  assert test-activation-hook;
  assert test-activation-deletes-state;
  assert test-script-appearance-detection;
  assert test-script-idempotency;
  assert test-script-fragments;
  assert test-empty-services;
  assert test-disabled; "all tests passed"
