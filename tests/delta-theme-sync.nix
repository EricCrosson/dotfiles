{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertHasAttr;

  eval = testConfig:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/services/delta-theme-sync/default.nix

        {
          options = {
            home.homeDirectory = lib.mkOption {
              type = lib.types.str;
              default = "/home/testuser";
            };
            appearance-sync.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
          };
        }

        testConfig
      ];
      specialArgs = {
        inherit pkgs;
        inputs = {};
      };
    })
    .config;

  test-appearance-sync-registered = let
    result = eval {
      services.delta-theme-sync = {
        enable = true;
        light-feature = "alabaster";
        dark-feature = "catppuccin-mocha";
      };
    };
  in
    assert assertHasAttr "registered" result.appearance-sync.services "delta-theme-sync"; true;

  test-onLight-contains-light-config = let
    result = eval {
      services.delta-theme-sync = {
        enable = true;
        light-feature = "alabaster";
        dark-feature = "catppuccin-mocha";
      };
    };
    svc = result.appearance-sync.services.delta-theme-sync;
  in
    assert assertContains "light-config-ref" svc.onLight "delta-config-light"; true;

  test-onDark-contains-dark-config = let
    result = eval {
      services.delta-theme-sync = {
        enable = true;
        light-feature = "alabaster";
        dark-feature = "catppuccin-mocha";
      };
    };
    svc = result.appearance-sync.services.delta-theme-sync;
  in
    assert assertContains "dark-config-ref" svc.onDark "delta-config-dark"; true;

  test-onLight-targets-delta-theme-gitconfig = let
    result = eval {
      services.delta-theme-sync = {
        enable = true;
        light-feature = "alabaster";
        dark-feature = "catppuccin-mocha";
      };
    };
    svc = result.appearance-sync.services.delta-theme-sync;
  in
    assert assertContains "target-path" svc.onLight "delta-theme.gitconfig"; true;

  test-disabled = let
    result = eval {
      services.delta-theme-sync = {
        enable = false;
        light-feature = "alabaster";
        dark-feature = "catppuccin-mocha";
      };
    };
  in
    assert !(result.appearance-sync.services ? delta-theme-sync); true;
in
  assert test-appearance-sync-registered;
  assert test-onLight-contains-light-config;
  assert test-onDark-contains-dark-config;
  assert test-onLight-targets-delta-theme-gitconfig;
  assert test-disabled; "all tests passed"
