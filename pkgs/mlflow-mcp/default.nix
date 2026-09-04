{
  python3,
  writeShellScriptBin,
}:
# Hermetic wrapper around MLflow's built-in MCP server (`mlflow mcp run`,
# bundled in mlflow >= 3.4). The wrapper exists so the shared mcpServers
# attrset can point every harness at a stable binary name instead of a
# python-env interpreter path.
#
# mlflow's [mcp] extra needs fastmcp<4,>=2.7.0, which nixpkgs strips from
# python3Packages.mlflow (optional-dependencies = []), so fastmcp is added
# explicitly. MLFLOW_TRACKING_URI is read from the ambient environment;
# when unset, mlflow falls back to the local ./mlruns file store.
let
  # nixpkgs' mlflow propagates mlflow-skinny, whose /bin/mlflow collides with
  # mlflow's in python3.withPackages, so build the env from mlflow-skinny
  # (the package that carries the mlflow CLI, including `mcp run`) directly.
  pythonEnv = python3.withPackages (ps: [
    ps.mlflow-skinny
    ps.fastmcp
  ]);
in
  writeShellScriptBin "mlflow-mcp" ''
    exec ${pythonEnv}/bin/mlflow mcp run "$@"
  ''
