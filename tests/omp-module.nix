{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertEq;
  renderers = import ../modules/home-manager/programs/ai/renderers.nix {inherit lib;};
  inherit (renderers) renderOmp;
  extendedLib =
    lib
    // {
      hm = {
        dag = {
          entryAfter = deps: data: {inherit deps data;};
        };
      };
    };

  testPkgs =
    pkgs
    // {
      formats =
        pkgs.formats
        // {
          json = _:
            (pkgs.formats.json {})
            // {
              generate = name: value: builtins.toFile name (builtins.toJSON value);
            };
        };
    };

  eval =
    (lib.evalModules {
      modules = [
        ../modules/home-manager/programs/omp/default.nix

        {
          options = {
            home.file = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
            home.activation = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
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
            mcpServers = renderOmp {
              slack = {
                transport = "http";
                url = "https://mcp.slack.com/mcp";
                headers = {Authorization = "Bearer fixture-token";};
                oauth = {
                  clientId = "fixture-client";
                  callbackPort = 3118;
                };
              };
            };
          };
        }
      ];
      specialArgs = {
        lib = extendedLib;
        pkgs = testPkgs;
      };
    })
    .config;

  activation = eval.home.activation.syncOmpMcpConfig;
  activationData = activation.data;
  basePath = builtins.head (lib.filter (lib.hasSuffix "-omp-mcp-base.json") (lib.splitString " " activationData));
  baseConfig = builtins.fromJSON (builtins.readFile basePath);
  server = baseConfig.mcpServers.slack;

  test-schema = assert assertEq "schema" baseConfig."$schema"
  "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json"; true;
  test-server = assert assertEq "server type" server.type "http";
  assert assertEq "server URL" server.url "https://mcp.slack.com/mcp";
  assert assertEq "server header" server.headers.Authorization "Bearer fixture-token";
  assert assertEq "oauth client" server.oauth.clientId "fixture-client";
  assert assertEq "oauth callback port" server.oauth.callbackPort 3118; true;
  test-activation = assert assertContains "sync invocation" activationData "omp-mcp-sync";
  assert assertContains "base config path" activationData "omp-mcp-base.json";
  assert assertEq "activation dependency" activation.deps ["linkGeneration"]; true;
in
  assert test-schema;
  assert test-server;
  assert test-activation; "all tests passed"
