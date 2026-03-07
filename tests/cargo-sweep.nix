{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertEq assertContains;

  # Extend lib with a mock of home-manager's dag functions
  extendedLib =
    lib
    // {
      hm = {
        dag = {
          entryAfter = deps: data: {inherit deps data;};
        };
      };
    };

  eval = testConfig:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/services/cargo-sweep/default.nix

        {
          options = {
            launchd-with-logs.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
            home = {
              homeDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser";
              };
              packages = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                default = [];
              };
            };
          };
        }

        testConfig
      ];
      specialArgs = {
        inherit pkgs;
        lib = extendedLib;
      };
    })
    .config;

  # === Test cases ===

  # Test: Default config produces correct launchd-with-logs service
  test-default-config = let
    result = eval {
      services.cargo-sweep = {
        enable = true;
        package = pkgs.hello;
      };
    };
    svc = result.launchd-with-logs.services.cargo-sweep;
  in
    assert assertContains "command-is-cargo-sweep" svc.command "/bin/cargo-sweep";
    assert assertEq "args-sweep" (builtins.head svc.args) "sweep";
    assert assertEq "args-recursive" (builtins.elemAt svc.args 1) "--recursive";
    assert assertEq "args-time-flag" (builtins.elemAt svc.args 2) "--time";
    assert assertEq "default-interval" svc.interval 86400; true;

  # Test: Custom maxAge is passed through
  test-custom-max-age = let
    result = eval {
      services.cargo-sweep = {
        enable = true;
        package = pkgs.hello;
        maxAge = 3.0;
      };
    };
    svc = result.launchd-with-logs.services.cargo-sweep;
  in
    assert assertEq "custom-age" (builtins.elemAt svc.args 3) (toString 3.0); true;

  # Test: Custom paths are appended to args
  test-custom-paths = let
    result = eval {
      services.cargo-sweep = {
        enable = true;
        package = pkgs.hello;
        paths = ["/foo" "/bar"];
      };
    };
    svc = result.launchd-with-logs.services.cargo-sweep;
    pathArgs = lib.drop 4 svc.args;
  in
    assert assertEq "custom-paths" pathArgs ["/foo" "/bar"]; true;

  # Test: Default path uses homeDirectory
  test-default-path = let
    result = eval {
      services.cargo-sweep = {
        enable = true;
        package = pkgs.hello;
      };
    };
    svc = result.launchd-with-logs.services.cargo-sweep;
    pathArgs = lib.drop 4 svc.args;
  in
    assert assertEq "default-path" pathArgs ["/home/testuser/workspace"]; true;
in
  assert test-default-config;
  assert test-custom-max-age;
  assert test-custom-paths;
  assert test-default-path; "all tests passed"
