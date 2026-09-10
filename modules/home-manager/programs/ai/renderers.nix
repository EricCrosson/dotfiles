{lib}: let
  inherit (lib) concatStringsSep filter mapAttrs optionalAttrs;

  transports = ["stdio" "http"];

  # A field carries author intent only when it has a non-empty value; the
  # manifest option defaults (null, [], {}) mean the author wrote nothing.
  isDeclared = value:
    value != null && value != [] && value != {};

  declaredFields = server:
    filter (field: isDeclared (server.${field} or null))
    (builtins.attrNames (builtins.removeAttrs server ["transport" "_module"]));

  # Shared translation driver: reject malformed transports, then reject any
  # declared field the target cannot represent instead of dropping it.
  renderServers = {
    harness,
    allowed,
    render,
  }: servers:
    mapAttrs (name: server: let
      transport = server.transport or null;
      requiredField =
        if transport == "stdio"
        then "command"
        else "url";
    in
      if !(builtins.elem transport transports)
      then
        builtins.throw
        "${harness} renderer: MCP server '${name}' has unsupported value for field 'transport': ${builtins.toJSON transport}"
      else if (server.${requiredField} or null) == null
      then
        builtins.throw
        "${harness} renderer: MCP server '${name}' with transport '${transport}' requires field '${requiredField}'"
      else let
        unsupported =
          filter (field: !(builtins.elem field (allowed.${transport} or []))) (declaredFields server);
      in
        if unsupported != []
        then
          builtins.throw
          "${harness} renderer: cannot represent field '${builtins.head unsupported}' declared by MCP server '${name}'"
        else render name server)
    servers;

  # Codex, Antigravity and OMP all consume command/args/env for stdio servers.
  stdioCommand = server:
    {inherit (server) command;}
    // optionalAttrs ((server.args or []) != []) {inherit (server) args;}
    // optionalAttrs ((server.env or {}) != {}) {inherit (server) env;};

  renderCodex = renderServers {
    harness = "codex";
    allowed = {
      stdio = ["command" "args" "env"];
      http = ["url"];
    };
    render = _name: server:
      if server.transport == "http"
      then {inherit (server) url;}
      else stdioCommand server;
  };

  renderAntigravity = renderServers {
    harness = "antigravity";
    allowed = {
      stdio = ["command" "args" "env"];
      http = ["url"];
    };
    render = _name: server:
      if server.transport == "http"
      then {httpUrl = server.url;}
      else stdioCommand server;
  };

  renderOpenCode = renderServers {
    harness = "opencode";
    allowed = {
      stdio = ["command" "args"];
      http = ["url"];
    };
    render = _name: server:
      if server.transport == "http"
      then {
        type = "remote";
        inherit (server) url;
        enabled = true;
      }
      else {
        type = "local";
        command = [server.command] ++ (server.args or []);
        enabled = true;
      };
  };

  renderOmp = renderServers {
    harness = "omp";
    allowed = {
      stdio = ["command" "args" "env"];
      http = ["url" "headers" "oauth"];
    };
    render = _name: server:
      if server.transport == "http"
      then
        {
          type = "http";
          inherit (server) url;
        }
        // optionalAttrs ((server.headers or {}) != {}) {inherit (server) headers;}
        // optionalAttrs ((server.oauth or null) != null) {inherit (server) oauth;}
      else stdioCommand server;
  };

  renderRulesContext = rules:
    concatStringsSep "\n\n" (map builtins.readFile rules);

  renderSkills = skills: skills;
in {
  inherit
    renderCodex
    renderAntigravity
    renderOpenCode
    renderOmp
    renderRulesContext
    renderSkills
    ;
}
