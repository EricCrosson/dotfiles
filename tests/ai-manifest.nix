{pkgs}: let
  inherit (pkgs) lib;
  module = ../modules/home-manager/programs/ai;

  eval = testConfig:
    lib.evalModules {
      modules = [
        module
        testConfig
      ];
    };

  valid = eval {
    programs.ai.manifest = {
      mcpServers = {
        local = {
          transport = "stdio";
          command = "local-mcp";
          args = ["--verbose" "--port" "9000"];
          env = {TOKEN = "secret";};
        };
        remote = {
          transport = "http";
          url = "https://example.test/mcp";
          headers = {Authorization = "Bearer token";};
          oauth = {
            enabled = true;
            retries = 2;
            issuer = "example";
          };
        };
      };
      rules = [../ai/rules/linear-issue-compliance.md ../ai/rules/linear-issue-compliance.md];
      skills = ../ai/skills;
    };
  };

  hasFailure = config: let
    evaluated = eval config;
    result = builtins.tryEval evaluated.config.programs.ai.manifest.mcpServers;
  in
    !result.success;

  defaults = eval {};
  defaultManifest = defaults.config.programs.ai.manifest;
  validManifest = valid.config.programs.ai.manifest;

  test-defaults = assert defaultManifest.mcpServers == {};
  assert defaultManifest.rules == [];
  assert defaultManifest.skills == null; true;

  test-valid-servers = assert validManifest.mcpServers.local.transport == "stdio";
  assert validManifest.mcpServers.local.command == "local-mcp";
  assert validManifest.mcpServers.local.args == ["--verbose" "--port" "9000"];
  assert validManifest.mcpServers.local.env.TOKEN == "secret";
  assert validManifest.mcpServers.remote.transport == "http";
  assert validManifest.mcpServers.remote.url == "https://example.test/mcp";
  assert validManifest.mcpServers.remote.headers.Authorization == "Bearer token";
  assert validManifest.mcpServers.remote.oauth
  == {
    enabled = true;
    retries = 2;
    issuer = "example";
  }; true;

  test-rules-and-skills = assert validManifest.rules == [../ai/rules/linear-issue-compliance.md ../ai/rules/linear-issue-compliance.md];
  assert validManifest.skills == ../ai/skills; true;

  test-missing-stdio-command = hasFailure {
    programs.ai.manifest.mcpServers.broken = {transport = "stdio";};
  };

  test-missing-http-url = hasFailure {
    programs.ai.manifest.mcpServers.broken = {transport = "http";};
  };

  test-mixed-stdio-http-fields = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "stdio";
      command = "local-mcp";
      url = "https://example.test/mcp";
    };
  };

  test-mixed-http-stdio-fields = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "http";
      url = "https://example.test/mcp";
      args = ["unexpected"];
    };
  };
  test-mixed-stdio-headers = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "stdio";
      command = "local-mcp";
      headers = {Authorization = "unexpected";};
    };
  };

  test-mixed-stdio-oauth = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "stdio";
      command = "local-mcp";
      oauth = {clientId = "unexpected";};
    };
  };

  test-mixed-http-command = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "http";
      url = "https://example.test/mcp";
      command = "unexpected";
    };
  };

  test-mixed-http-env = hasFailure {
    programs.ai.manifest.mcpServers.broken = {
      transport = "http";
      url = "https://example.test/mcp";
      env = {TOKEN = "unexpected";};
    };
  };
in
  assert test-defaults;
  assert test-valid-servers;
  assert test-rules-and-skills;
  assert test-missing-stdio-command;
  assert test-missing-http-url;
  assert test-mixed-stdio-http-fields;
  assert test-mixed-http-stdio-fields;
  assert test-mixed-stdio-headers;
  assert test-mixed-stdio-oauth;
  assert test-mixed-http-command;
  assert test-mixed-http-env; "all tests passed"
