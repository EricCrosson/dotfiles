{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertHasAttr;

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
        ../modules/home-manager/programs/claude/default.nix

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
            appearance-sync.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
          };
        }

        testConfig
      ];
      specialArgs = {
        lib = extendedLib;
        inherit pkgs;
      };
    })
    .config;

  test-appearance-sync-registered = let
    result = eval {programs.claude.theme-sync.enable = true;};
  in
    assert assertHasAttr "registered" result.appearance-sync.services "claude-theme-sync"; true;

  test-onLight-patches-json = let
    result = eval {programs.claude.theme-sync.enable = true;};
    svc = result.appearance-sync.services.claude-theme-sync;
  in
    assert assertContains "jq-usage" svc.onLight "jq";
    assert assertContains "theme-value" svc.onLight "light"; true;

  test-onDark-patches-json = let
    result = eval {programs.claude.theme-sync.enable = true;};
    svc = result.appearance-sync.services.claude-theme-sync;
  in
    assert assertContains "jq-usage" svc.onDark "jq";
    assert assertContains "theme-value" svc.onDark "dark"; true;

  test-targets-claude-json = let
    result = eval {programs.claude.theme-sync.enable = true;};
    svc = result.appearance-sync.services.claude-theme-sync;
  in
    assert assertContains "claude-json" svc.onLight ".claude.json"; true;

  test-runtimeInputs-includes-jq = let
    result = eval {programs.claude.theme-sync.enable = true;};
    svc = result.appearance-sync.services.claude-theme-sync;
  in
    assert lib.any (p: lib.getName p == "jq") svc.runtimeInputs; true;

  test-disabled = let
    result = eval {programs.claude.theme-sync.enable = false;};
  in
    assert !(result.appearance-sync.services ? claude-theme-sync); true;
in
  assert test-appearance-sync-registered;
  assert test-onLight-patches-json;
  assert test-onDark-patches-json;
  assert test-targets-claude-json;
  assert test-runtimeInputs-includes-jq;
  assert test-disabled; "all tests passed"
