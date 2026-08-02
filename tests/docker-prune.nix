{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertEq assertContains;

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
        ../modules/home-manager/services/docker-prune/default.nix

        {
          options = {
            assertions = lib.mkOption {
              type = lib.types.listOf lib.types.anything;
              default = [];
            };
            launchd-with-logs.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
            home.homeDirectory = lib.mkOption {
              type = lib.types.str;
              default = "/home/testuser";
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

  test-default-config = let
    result = eval {services.docker-prune.enable = true;};
    svc = result.launchd-with-logs.services.docker-prune;
  in
    assert assertEq "default-interval" svc.interval 86400;
    assert assertEq "run-at-load" svc.runAtLoad false;
    assert assertContains "command-is-docker-prune" svc.command "/bin/docker-prune";
    assert assertEq "log-path" svc.logging.stderr "/home/testuser/Library/Logs/docker-prune.error.log"; true;

  test-custom-config = let
    result = eval {
      services.docker-prune = {
        enable = true;
        dockerCommand = "/custom/docker";
        maxAge = 7;
        interval = 3600;
      };
    };
    svc = result.launchd-with-logs.services.docker-prune;
  in
    assert assertEq "custom-interval" svc.interval 3600;
    assert assertEq "homebrew-path" svc.environment.PATH "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"; true;
in
  assert test-default-config;
  assert test-custom-config; "all tests passed"
