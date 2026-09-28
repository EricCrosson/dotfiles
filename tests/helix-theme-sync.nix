{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertHasAttr;

  eval = testConfig:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/services/helix-theme-sync/default.nix

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
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
  in
    assert assertHasAttr "registered" result.appearance-sync.services "helix-theme-sync"; true;

  test-onLight-contains-light-config = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
    svc = result.appearance-sync.services.helix-theme-sync;
  in
    assert assertContains "light-config-ref" svc.onLight "helix-config-light"; true;

  test-onDark-contains-dark-config = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
    svc = result.appearance-sync.services.helix-theme-sync;
  in
    assert assertContains "dark-config-ref" svc.onDark "helix-config-dark"; true;

  test-onLight-signals-helix = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        settings = {editor = {};};
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
    svc = result.appearance-sync.services.helix-theme-sync;
  in
    assert assertContains "pkill-signal" svc.onLight "pkill -USR1 hx"; true;

  test-disabled = let
    result = eval {
      services.helix-theme-sync = {
        enable = false;
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
  in
    assert !(result.appearance-sync.services ? helix-theme-sync); true;
  # Invariant: each fragment must create ~/.config/helix before linking
  # into it. On a fresh host (helix never launched) the dir does not
  # exist, and writeShellApplication's set -e would abort the whole
  # theme-sync run, leaving the theme stale until a manual fix.
  test-fragments-create-config-dir = let
    result = eval {
      services.helix-theme-sync = {
        enable = true;
        light-theme = "test-light";
        dark-theme = "test-dark";
      };
    };
    svc = result.appearance-sync.services.helix-theme-sync;
  in
    assert assertContains "onLight-mkdir-before-symlink" svc.onLight "mkdir -p -- \"/home/testuser/.config/helix\"\nln -sf";
    assert assertContains "onDark-mkdir-before-symlink" svc.onDark "mkdir -p -- \"/home/testuser/.config/helix\"\nln -sf"; true;
in
  assert test-appearance-sync-registered;
  assert test-onLight-contains-light-config;
  assert test-onDark-contains-dark-config;
  assert test-onLight-signals-helix;
  assert test-fragments-create-config-dir;
  assert test-disabled; "all tests passed"
