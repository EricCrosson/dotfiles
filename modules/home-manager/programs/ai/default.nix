{lib, ...}:
with lib; let
  scalarType = types.oneOf [types.str types.int types.bool];

  serverModule = {
    options = {
      transport = mkOption {
        type = types.enum ["stdio" "http"];
        description = "Transport used to connect to this MCP server.";
      };
      command = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Executable for a stdio MCP server.";
      };
      args = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Arguments passed to a stdio MCP server.";
      };
      env = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Environment variables for a stdio MCP server.";
      };
      url = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "URL for an HTTP MCP server.";
      };
      headers = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "HTTP headers for an HTTP MCP server.";
      };
      oauth = mkOption {
        type = types.nullOr (types.attrsOf scalarType);
        default = null;
        description = "OAuth settings for an HTTP MCP server.";
      };
    };
  };

  validateServer = name: server: let
    path = field: "programs.ai.manifest.mcpServers.${name}.${field}";
    fail = field: message: builtins.throw "${path field} ${message}";
  in
    if server.transport == "stdio"
    then
      if server.command == null
      then fail "command" "is required when transport is stdio"
      else if server.url != null
      then fail "url" "is only valid when transport is http"
      else if server.headers != {}
      then fail "headers" "is only valid when transport is http"
      else if server.oauth != null
      then fail "oauth" "is only valid when transport is http"
      else true
    else if server.url == null
    then fail "url" "is required when transport is http"
    else if server.command != null
    then fail "command" "is only valid when transport is stdio"
    else if server.args != []
    then fail "args" "is only valid when transport is stdio"
    else if server.env != {}
    then fail "env" "is only valid when transport is stdio"
    else true;

  validateServers = servers:
    builtins.foldl' (result: name: builtins.seq (validateServer name servers.${name}) result) true (attrNames servers);
in {
  options.programs.ai.manifest = mkOption {
    type = types.submodule {
      options = {
        mcpServers = mkOption {
          type = types.attrsOf (types.submodule serverModule);
          default = {};
          apply = servers: builtins.seq (validateServers servers) servers;
          description = "Typed MCP server definitions shared by AI harnesses.";
        };
        rules = mkOption {
          type = types.listOf types.path;
          default = [];
          description = "Ordered rule files shared by AI harnesses.";
        };
        skills = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = "Optional skills directory shared by AI harnesses.";
        };
      };
    };
    default = {};
    description = "Shared typed configuration for AI harnesses.";
  };
}
