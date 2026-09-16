{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertEq assertHasAttr assertNotHasAttr;

  extension = builtins.readFile ../modules/home-manager/programs/omp/extensions/cache-temperature.ts;

  # Formats generators are overridden to emit plain JSON paths so eval can
  # readFile them without building (mirrors tests/omp-module.nix).
  testPkgs =
    pkgs
    // {
      formats =
        pkgs.formats
        // {
          yaml = _:
            (pkgs.formats.yaml {})
            // {
              generate = name: value: builtins.toFile name (builtins.toJSON value);
            };
          json = _:
            (pkgs.formats.json {})
            // {
              generate = name: value: builtins.toFile name (builtins.toJSON value);
            };
        };
    };

  extendedLib =
    lib
    // {
      hm = {
        dag = {
          entryAfter = deps: data: {inherit deps data;};
        };
      };
    };

  evalFor = {
    settings,
    cacheTemperature,
  }:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/programs/omp/default.nix
        {
          options = {
            programs.zsh.initContent = lib.mkOption {
              type = lib.types.lines;
              default = "";
            };
            home = {
              file = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = {};
              };
              activation = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = {};
              };
              packages = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                default = [];
              };
            };
            lib.dag.entryAfter = lib.mkOption {
              type = lib.types.functionTo (lib.types.functionTo lib.types.anything);
              default = deps: data: {inherit deps data;};
            };
          };
        }
        {
          programs.omp = {
            enable = true;
            package = pkgs.hello;
            inherit settings cacheTemperature;
          };
        }
      ];
      specialArgs = {
        lib = extendedLib;
        pkgs = testPkgs;
      };
    })
    .config;

  enabled = evalFor {
    settings = {
      display.cacheMissMarker = true;
      statusLine = {
        preset = "custom";
        separator = "powerline-thin";
        leftSegments = ["pi" "model" "mode" "path" "git" "context_pct"];
        rightSegments = ["session_name" "cache_hit" "cache_read" "cache_write" "cost"];
      };
    };
    cacheTemperature.enable = true;
  };

  disabled = evalFor {
    settings = {};
    cacheTemperature.enable = false;
  };

  baseYamlPath = builtins.head (lib.filter (lib.hasSuffix "omp-base-config.yml") (lib.splitString " " enabled.home.activation.syncOmpConfig.data));
  baseSettings = builtins.fromJSON (builtins.readFile baseYamlPath);

  inherit (baseSettings) statusLine;

  test-extension-deployed = assert assertHasAttr "extension installed" enabled.home.file ".omp/agent/extensions/cache-temperature.ts"; true;
  test-extension-absent-when-disabled = assert assertNotHasAttr "extension not installed when disabled" disabled.home.file ".omp/agent/extensions/cache-temperature.ts"; true;
  test-custom-preset = assert assertEq "statusLine.preset" statusLine.preset "custom"; true;
  test-cache-hit-in-right-segments = assert assertEq "cache_hit in rightSegments" (builtins.elem "cache_hit" statusLine.rightSegments) true; true;
  test-cache-read-write-in-right-segments = assert assertEq "cache_read and cache_write in rightSegments" (builtins.elem "cache_read" statusLine.rightSegments && builtins.elem "cache_write" statusLine.rightSegments) true; true;
  test-miss-marker = assert assertEq "display.cacheMissMarker" baseSettings.display.cacheMissMarker true; true;
  test-extension-contract = assert assertContains "subscribes to provider responses" extension "after_provider_response";
  assert assertContains "renders a widget" extension "setWidget";
  assert assertContains "widget below editor" extension "belowEditor";
  assert assertContains "reads TTL override" extension "OMP_CACHE_TTL_SECONDS";
  assert assertContains "marks compaction" extension "session_compact"; true;
  test-env-override = let
    withTtl = evalFor {
      settings = {};
      cacheTemperature = {
        enable = true;
        ttlSeconds = 30;
      };
    };
    envFile = withTtl.home.file.".omp/agent/.env".text;
  in
    assert assertContains "TTL exported to .env" envFile "OMP_CACHE_TTL_SECONDS=30"; true;
in
  assert test-extension-deployed;
  assert test-extension-absent-when-disabled;
  assert test-custom-preset;
  assert test-cache-hit-in-right-segments;
  assert test-cache-read-write-in-right-segments;
  assert test-miss-marker;
  assert test-extension-contract;
  assert test-env-override; "all tests passed"
