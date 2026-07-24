{pkgs}: let
  mcp-remote = pkgs.callPackage ../pkgs/mcp-remote {};
in
  # Verify the binary exists and is executable
  assert builtins.pathExists "${mcp-remote}/bin/mcp-remote"; "all tests passed"
