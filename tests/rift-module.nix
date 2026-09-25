{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertEq assertHasAttr assertNotHasAttr;

  extendedLib =
    lib
    // {
      hm = {
        dag = {
          entryAfter = deps: data: {inherit deps data;};
        };
        assertions = {
          assertPlatform = module: pkgs: platform: {
            assertion = pkgs.stdenv.hostPlatform.isDarwin == (platform == lib.platforms.darwin);
            message = "Module ${module} is not available on the current platform";
          };
        };
      };
    };

  # Stub of pkgs.rift-wm so tests never build the real package
  stubRift = pkgs.runCommand "rift-wm-stub" {meta.mainProgram = "rift";} "mkdir -p $out/bin && touch $out/bin/rift $out/bin/rift-cli";

  darwinPkgs =
    pkgs
    // {
      rift-wm = stubRift;
      stdenv =
        pkgs.stdenv
        // {
          hostPlatform =
            pkgs.stdenv.hostPlatform
            // {
              isDarwin = true;
            };
        };
    };

  linuxPkgs =
    pkgs
    // {
      rift-wm = stubRift;
      stdenv =
        pkgs.stdenv
        // {
          hostPlatform =
            pkgs.stdenv.hostPlatform
            // {
              isDarwin = false;
            };
        };
    };

  # Mirrors the settings used by the real profile module
  mainFixture = {
    programs.rift = {
      enable = true;
      package = stubRift;
      launchd = {
        enable = true;
        keepAlive = true;
      };
      settings = {
        settings = {
          layout.mode = "traditional";
          default_disable = false;
          animate = false;
        };
        modifier_combinations.comb1 = "Alt + Shift";
        virtual_workspaces = {
          enabled = true;
          default_workspace_count = 10;
          auto_assign_windows = true;
          preserve_focus_per_workspace = true;
          app_rules = [
            {
              app_id = "com.tinyspeck.slackmacgap";
              workspace = 0;
            }
            {
              app_id = "com.goodsnooze.MacWhisper";
              workspace = 0;
              floating = true;
            }
          ];
        };
        keys = {
          "Alt + Z" = "toggle_space_activated";
          "Alt + H" = {move_focus = "left";};
          "comb1 + H" = {move_node = "left";};
          "Alt + Ctrl + H" = {
            move_window_to_display = {selector = "left";};
          };
          "Alt + Minus" = {resize_window_shrink = "smart";};
          "Alt + 1" = {switch_to_workspace = 0;};
          "comb1 + Comma" = "toggle_orientation";
          "comb1 + Space" = "toggle_window_floating";
          "comb1 + Left" = {join_window = "left";};
        };
      };
    };
  };

  evalWith = pkgsVariant: fixture:
    (lib.evalModules {
      modules = [
        ../modules/home-manager/programs/rift/default.nix
        ../modules/home-manager/services/launchd-with-logs/default.nix

        {
          options = {
            home = {
              homeDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser";
              };
              profileDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser/.nix-profile";
              };
              username = lib.mkOption {
                type = lib.types.str;
                default = "testuser";
              };
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
            launchd.agents = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
            assertions = lib.mkOption {
              type = lib.types.listOf lib.types.anything;
              default = [];
            };
          };
        }

        fixture
      ];
      specialArgs = {
        lib = extendedLib;
        pkgs = pkgsVariant;
      };
    })
    .config;

  eval = evalWith darwinPkgs mainFixture;

  # Round-trip the generated config through toml2json to prove it is valid TOML
  tomlJson = builtins.fromJSON (builtins.readFile (
    pkgs.runCommand "rift-config.json" {nativeBuildInputs = [pkgs.remarshal];}
    "toml2json ${eval.home.file.".config/rift/config.toml".source} > $out"
  ));

  agent = eval.launchd.agents.rift.config;

  disabledEval = evalWith darwinPkgs {
    programs.rift.enable = false;
  };

  emptySettingsEval = evalWith darwinPkgs {
    programs.rift = {
      enable = true;
      package = stubRift;
      launchd = {
        enable = true;
        keepAlive = true;
      };
      settings = {};
    };
  };

  noLaunchdEval = evalWith darwinPkgs {
    programs.rift = {
      enable = true;
      package = stubRift;
      launchd.enable = false;
      inherit (mainFixture.programs.rift) settings;
    };
  };

  linuxEval = evalWith linuxPkgs mainFixture;
  darwinEval = evalWith darwinPkgs mainFixture;

  # The shipped profile settings, evaluated through the module
  profileSettings = (import ../profiles/eric/modules/rift.nix {}).programs.rift.settings;

  # The profile module itself, to guard the migration from silently disabling
  # rift (which would still evaluate and build, just without a window manager)
  profileRift = (import ../profiles/eric/modules/rift.nix {}).programs.rift;
  profileEval = evalWith darwinPkgs {
    programs.rift = {
      enable = true;
      package = stubRift;
      launchd = {
        enable = true;
        keepAlive = true;
      };
      settings = profileSettings;
    };
  };
  profileTomlJson = builtins.fromJSON (builtins.readFile (
    pkgs.runCommand "rift-profile-config.json" {nativeBuildInputs = [pkgs.remarshal];}
    "toml2json ${profileEval.home.file.".config/rift/config.toml".source} > $out"
  ));

  # rift 0.5.5 uses serde deny_unknown_fields: an unknown key makes rift reject
  # the whole config at reload, so guard every table against its real field list
  allowedTopKeys = ["settings" "keys" "virtual_workspaces" "modifier_combinations"];
  allowedSettingsKeys = [
    "animate"
    "animation_duration"
    "animation_fps"
    "animation_easing"
    "default_disable"
    "mouse_follows_focus"
    "mouse_hides_on_focus"
    "focus_follows_mouse"
    "focus_follows_mouse_disable_hotkey"
    "auto_focus_blacklist"
    "layout"
    "ui"
    "gestures"
    "window_snapping"
    "run_on_start"
    "hot_reload"
  ];
  allowedVirtualWorkspaceKeys = [
    "enabled"
    "default_workspace_count"
    "auto_assign_windows"
    "preserve_focus_per_workspace"
    "workspace_auto_back_and_forth"
    "prevent_wrapping"
    "workspace_names"
    "default_workspace"
    "reapply_app_rules_on_title_change"
    "app_rules"
    "workspace_rules"
  ];

  # Test: config file is generated
  test-config-file = assert assertHasAttr "config file" eval.home.file ".config/rift/config.toml"; true;

  # Test: generated config.toml round-trips with the expected structure
  test-toml-roundtrip = assert assertEq "layout mode" tomlJson.settings.layout.mode "traditional";
  assert assertEq "default_disable" tomlJson.settings.default_disable false;
  assert assertEq "animate" tomlJson.settings.animate false;
  assert assertEq "modifier combination" tomlJson.modifier_combinations.comb1 "Alt + Shift";
  assert assertEq "toggle space key" tomlJson.keys."Alt + Z" "toggle_space_activated";
  assert assertEq "move focus key" tomlJson.keys."Alt + H".move_focus "left";
  assert assertEq "move node key" tomlJson.keys."comb1 + H".move_node "left";
  assert assertEq "move to display key" tomlJson.keys."Alt + Ctrl + H".move_window_to_display.selector "left";
  assert assertEq "resize key" tomlJson.keys."Alt + Minus".resize_window_shrink "smart";
  assert assertEq "switch workspace key" tomlJson.keys."Alt + 1".switch_to_workspace 0;
  assert assertEq "toggle orientation key" tomlJson.keys."comb1 + Comma" "toggle_orientation";
  assert assertEq "toggle floating key" tomlJson.keys."comb1 + Space" "toggle_window_floating";
  assert assertEq "join window key" tomlJson.keys."comb1 + Left".join_window "left";
  assert assertEq "workspace count" tomlJson.virtual_workspaces.default_workspace_count 10;
  assert assertEq "app rules" tomlJson.virtual_workspaces.app_rules [
    {
      app_id = "com.tinyspeck.slackmacgap";
      workspace = 0;
    }
    {
      app_id = "com.goodsnooze.MacWhisper";
      workspace = 0;
      floating = true;
    }
  ]; true;

  # Test: launchd agent converted by launchd-with-logs has the expected properties
  test-launchd-agent = assert assertEq "program arguments" agent.ProgramArguments ["${stubRift}/bin/rift"];
  assert assertEq "run at load" agent.RunAtLoad true;
  assert assertEq "keep alive" agent.KeepAlive true;
  assert assertEq "stdout log" agent.StandardOutPath "/home/testuser/Library/Logs/rift.log";
  assert assertEq "stderr log" agent.StandardErrorPath "/home/testuser/Library/Logs/rift.error.log";
  assert assertEq "rust log level" agent.EnvironmentVariables.RUST_LOG "error,warn,info";
  assert assertContains "profile bin in PATH" agent.EnvironmentVariables.PATH "/home/testuser/.nix-profile/bin";
  assert assertContains "system sw in PATH" agent.EnvironmentVariables.PATH "/run/current-system/sw/bin"; true;

  # Test: newsyslog rotation covers both log files
  test-log-rotation = let
    newsyslog = eval.home.file.".config/newsyslog-launchd-with-logs.conf".text;
  in
    assert assertContains "stdout rotation" newsyslog "/home/testuser/Library/Logs/rift.log";
    assert assertContains "stderr rotation" newsyslog "/home/testuser/Library/Logs/rift.error.log"; true;

  # Test: the rift package is installed
  test-package-installed = assert assertEq "rift in packages" (lib.count (p: p == stubRift) eval.home.packages) 1; true;

  # Test: config file change triggers a rift config reload
  test-onchange-reload = let
    inherit (eval.home.file.".config/rift/config.toml") onChange;
  in
    assert assertContains "reload command" onChange "execute config reload";
    assert assertContains "displays query" onChange "query displays";
    assert assertContains "rift-cli" onChange "rift-cli"; true;

  # Test: empty settings suppress the config file but keep the agent
  test-empty-settings = assert assertNotHasAttr "no config file" emptySettingsEval.home.file ".config/rift/config.toml";
  assert assertHasAttr "agent still exists" emptySettingsEval.launchd.agents "rift"; true;

  # Test: launchd disabled suppresses the agent and newsyslog entry but keeps the config
  test-launchd-disabled = assert assertNotHasAttr "no agent" noLaunchdEval.launchd.agents "rift";
  assert assertNotHasAttr "no newsyslog" noLaunchdEval.home.file ".config/newsyslog-launchd-with-logs.conf";
  assert assertHasAttr "config file still exists" noLaunchdEval.home.file ".config/rift/config.toml"; true;

  # Test: disabled module contributes nothing
  test-disabled = assert assertNotHasAttr "no config file" disabledEval.home.file ".config/rift/config.toml";
  assert assertNotHasAttr "no agent" disabledEval.launchd.agents "rift";
  assert assertEq "no packages" disabledEval.home.packages [];
  assert assertEq "no assertions" disabledEval.assertions []; true;

  # Test: platform assertion rejects non-darwin and accepts darwin
  test-platform-assertion = assert assertEq "linux rejected" (lib.any (a: !a.assertion) linuxEval.assertions) true;
  assert assertEq "darwin accepted" (lib.all (a: a.assertion) darwinEval.assertions) true; true;

  # Test: the shipped profile enables rift and its launchd agent
  test-profile-enables-rift = assert assertEq "profile enables rift" profileRift.enable true;
  assert assertEq "profile enables launchd" profileRift.launchd.enable true; true;

  # Test: shipped profile settings only use fields rift 0.5.5 accepts
  test-profile-schema = assert assertEq "unknown top-level keys" (lib.subtractLists allowedTopKeys (builtins.attrNames profileTomlJson)) [];
  assert assertEq "unknown settings keys" (lib.subtractLists allowedSettingsKeys (builtins.attrNames (profileTomlJson.settings or {}))) [];
  assert assertEq "unknown virtual_workspaces keys" (lib.subtractLists allowedVirtualWorkspaceKeys (builtins.attrNames (profileTomlJson.virtual_workspaces or {}))) []; true;
in
  assert test-config-file;
  assert test-toml-roundtrip;
  assert test-launchd-agent;
  assert test-log-rotation;
  assert test-package-installed;
  assert test-onchange-reload;
  assert test-empty-settings;
  assert test-launchd-disabled;
  assert test-disabled;
  assert test-platform-assertion;
  assert test-profile-enables-rift;
  assert test-profile-schema; "all tests passed"
