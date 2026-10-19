{
  lib,
  python3,
  writeShellScriptBin,
  cloudflared ? null,
  env ? {},
  tokenFile ? null,
}:
# Hermetic wrapper around MLflow's built-in MCP server (`mlflow mcp run`,
# bundled in mlflow >= 3.4). The wrapper exists so the shared mcpServers
# attrset can point every harness at a stable binary name instead of a
# python-env interpreter path.
#
# The python env also carries the mlflow-cf-access request-auth provider,
# which attaches the Cloudflare Access JWT whenever MLFLOW_TRACKING_AUTH is
# set to cloudflare-access; without that env var the provider stays dormant.
#
# `env` entries are exported only when the ambient environment does not
# already define them, so per-shell overrides win. `tokenFile` is read at
# invocation time, not at build time: sops-nix materializes secrets on a RAM
# disk via a login-time LaunchAgent, so the file can be missing while the
# wrapper is being built or before the first login.
let
  # nixpkgs' mlflow propagates mlflow-skinny, whose /bin/mlflow collides with
  # mlflow's in python3.withPackages, so build the env from mlflow-skinny
  # (the package that carries the mlflow CLI, including `mcp run`) directly.
  pythonEnv = python3.withPackages (ps: [
    ps.mlflow-skinny
    ps.fastmcp
    (ps.callPackage ../mlflow-cf-access {})
  ]);

  envLines =
    lib.mapAttrsToList (name: value: ''
      if [[ -z "''${${name}:-}" ]]; then
        ${name}=${lib.escapeShellArg value}
      fi
      export ${name}
    '') (env
      // lib.optionalAttrs (cloudflared != null) {
        MLFLOW_CF_ACCESS_CLOUDFLARED = "${cloudflared}/bin/cloudflared";
      });

  tokenLines =
    if tokenFile == null
    then []
    else [
      ''
        if [[ -z "''${MLFLOW_TRACKING_PASSWORD:-}" && -s ${lib.escapeShellArg tokenFile} ]]; then
          MLFLOW_TRACKING_PASSWORD="$(< ${lib.escapeShellArg tokenFile})"
          export MLFLOW_TRACKING_PASSWORD
        fi
      ''
    ];
in
  writeShellScriptBin "mlflow-mcp" ''
    ${lib.concatStringsSep "\n" (envLines ++ tokenLines)}
    exec ${pythonEnv}/bin/mlflow mcp run "$@"
  ''
