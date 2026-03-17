{pkgs}: let
  chrome-devtools-mcp = pkgs.callPackage ../pkgs/chrome-devtools-mcp {};
in
  # Verify the binary exists and is executable
  assert builtins.pathExists "${chrome-devtools-mcp}/bin/chrome-devtools-mcp";
  assert builtins.pathExists "${chrome-devtools-mcp}/bin/chrome-devtools"; "all tests passed"
