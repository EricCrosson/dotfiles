{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertEq assertHasAttr assertNotHasAttr;

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
              homeDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser";
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

  # Test: The newsyslog conf is actually consumed — a launchd agent must
  # invoke newsyslog against it. macOS's system newsyslog only reads
  # /etc/newsyslog.conf, so a user conf that nothing executes is dead
  # config and the managed logs grow unbounded.
  test-newsyslog-consumed = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
        logging = {
          stdout = "/tmp/test.log";
          stderr = "/tmp/test.error.log";
        };
      };
    };
    agentConfig = result.launchd.agents.newsyslog-launchd-with-logs.config;
  in
    assert assertEq
    "newsyslog-agent-args"
    agentConfig.ProgramArguments
    ["/usr/sbin/newsyslog" "-r" "-f" "/home/testuser/.config/newsyslog-launchd-with-logs.conf"];
    assert assertEq "newsyslog-agent-interval" agentConfig.StartInterval 3600; true;

  # Test: No newsyslog config when logging is disabled
  test-no-newsyslog = let
    result = eval {
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
      };
    };
  in
    assert assertNotHasAttr "no-newsyslog-config" result.home.file ".config/newsyslog-launchd-with-logs.conf";
    assert assertNotHasAttr "no-newsyslog-agent" result.launchd.agents "newsyslog-launchd-with-logs"; true;
in {
  asserts =
    # Force evaluation of all tests
    assert test-interval;
    assert test-no-interval;
    assert test-args;
    assert test-no-args;
    assert test-logging;
    assert test-logging-null;
    assert test-newsyslog;
    assert test-newsyslog-consumed;
    assert test-no-newsyslog; "all tests passed";

  # The exact invocation the rotator agent must use, and the conf it must
  # consume, exported for behavioral validation: newsyslog refuses to run
  # as non-root without -r, so the flake check executes these args
  # (dry-run) against this conf.
  userInvocation = let
    logged = eval {
      # newsyslog validates the owner field against real users, so the
      # behavioral check must name one that exists on the build host.
      home.username = "root";
      launchd-with-logs.services.test-service = {
        command = "/usr/bin/true";
        logging = {
          stdout = "/tmp/test.log";
          stderr = "/tmp/test.error.log";
        };
      };
    };
    flags = ["-r"];
  in {
    inherit flags;
    programArguments =
      ["/usr/sbin/newsyslog"]
      ++ flags
      ++ ["-f" "/home/testuser/.config/newsyslog-launchd-with-logs.conf"];
    conf = logged.home.file.".config/newsyslog-launchd-with-logs.conf".text;
  };
}
