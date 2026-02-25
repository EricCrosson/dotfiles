{pkgs}: let
  inherit (pkgs) lib;

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

  # Helper to evaluate the module with a given config
  eval = testConfig:
    (lib.evalModules {
      modules = [
        # The module under test
        ../modules/home-manager/services/launchd-with-logs/default.nix

        # Stub option declarations for outputs the module writes to
        {
          options = {
            launchd.agents = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
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
              username = lib.mkOption {
                type = lib.types.str;
                default = "testuser";
              };
            };
          };
        }

        # Test-specific configuration
        testConfig
      ];
      specialArgs = {lib = extendedLib;};
    })
    .config;

  # Test helpers
  assertEq = name: actual: expected:
    if actual == expected
    then true
    else builtins.throw "Test '${name}' failed: expected ${builtins.toJSON expected}, got ${builtins.toJSON actual}";

  assertHasAttr = name: attrset: attr:
    if builtins.hasAttr attr attrset
    then true
    else builtins.throw "Test '${name}' failed: attribute '${attr}' not found in ${builtins.toJSON attrset}";

  assertNotHasAttr = name: attrset: attr:
    if !builtins.hasAttr attr attrset
    then true
    else builtins.throw "Test '${name}' failed: attribute '${attr}' should not exist";

  # === Test cases ===

  # Test: Service with interval produces StartInterval key
  test-interval = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
        interval = 300;
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertEq "interval-value" agentConfig.StartInterval 300; true;

  # Test: Service without interval omits StartInterval key
  test-no-interval = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertNotHasAttr "no-interval" agentConfig "StartInterval"; true;

  # Test: Service args appear in ProgramArguments
  test-args = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/echo";
        args = ["hello" "world"];
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertEq "program-arguments" agentConfig.ProgramArguments ["/usr/bin/echo" "hello" "world"]; true;

  # Test: Command without args uses single-element ProgramArguments
  test-no-args = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertEq "program-arguments-no-args" agentConfig.ProgramArguments ["/usr/bin/true"]; true;

  # Test: Logging paths set correctly
  test-logging = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
        logging = {
          stdout = "/tmp/test.log";
          stderr = "/tmp/test.error.log";
        };
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertEq "stdout-path" agentConfig.StandardOutPath "/tmp/test.log";
    assert assertEq "stderr-path" agentConfig.StandardErrorPath "/tmp/test.error.log"; true;

  # Test: Null logging defaults to /dev/null
  test-logging-null = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
      };
    };
    agentConfig = result.launchd.agents.test-service.config;
  in
    assert assertEq "stdout-null" agentConfig.StandardOutPath "/dev/null";
    assert assertEq "stderr-null" agentConfig.StandardErrorPath "/dev/null"; true;

  # Test: Newsyslog config generated for services with logging
  test-newsyslog = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
        logging = {
          stdout = "/tmp/test.log";
          stderr = "/tmp/test.error.log";
        };
      };
    };
  in
    assert assertHasAttr "newsyslog-config" result.home.file ".config/newsyslog-launchd-with-logs.conf"; true;

  # Test: No newsyslog config when logging is disabled
  test-no-newsyslog = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
      };
    };
  in
    assert assertNotHasAttr "no-newsyslog-config" result.home.file ".config/newsyslog-launchd-with-logs.conf"; true;
in
  # Force evaluation of all tests
  assert test-interval;
  assert test-no-interval;
  assert test-args;
  assert test-no-args;
  assert test-logging;
  assert test-logging-null;
  assert test-newsyslog;
  assert test-no-newsyslog; "all tests passed"
