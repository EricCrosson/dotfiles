{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.conductor;
  mcpConfigFile = (pkgs.formats.json {}).generate "claude-mcp-servers.json" config.programs.claude-code.mcpServers;
in {
  options.programs.conductor = {
    enable = mkEnableOption "Conductor (sync MCP servers into ~/.claude.json)";
  };

  config = mkIf cfg.enable {
    home.activation = {
      # Sync MCP servers into ~/.claude.json (user scope) so Conductor picks
      # them up — its bundled claude binary doesn't use the Nix wrapper that
      # injects --mcp-config.
      syncClaudeMcpServers = config.lib.dag.entryAfter ["writeBoundary"] ''
        if [ -f "$HOME/.claude.json" ]; then
          cp "$HOME/.claude.json" "$HOME/.claude.json.bak"
          if ${pkgs.jq}/bin/jq --slurpfile servers ${mcpConfigFile} '.mcpServers = $servers[0]' \
            "$HOME/.claude.json.bak" > "$HOME/.claude.json.tmp" \
            && ${pkgs.jq}/bin/jq empty "$HOME/.claude.json.tmp" 2>/dev/null; then
            mv "$HOME/.claude.json.tmp" "$HOME/.claude.json"
          else
            echo "WARNING: failed to sync MCP servers into ~/.claude.json, restoring backup" >&2
            mv "$HOME/.claude.json.bak" "$HOME/.claude.json"
            rm -f "$HOME/.claude.json.tmp"
          fi
        fi
      '';
    };
  };
}
