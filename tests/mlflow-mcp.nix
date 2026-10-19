{
  pkgs,
  mlflowPython3,
}: let
  inherit (pkgs) callPackage;
  mlflow-mcp = callPackage ../pkgs/mlflow-mcp {python3 = mlflowPython3;};
  configured = callPackage ../pkgs/mlflow-mcp {
    inherit (pkgs) cloudflared;
    env = {
      MLFLOW_TRACKING_URI = "https://mlflow.example.test";
      MLFLOW_TRACKING_AUTH = "cloudflare-access";
      MLFLOW_TRACKING_USERNAME = "someone@example.test";
      MLFLOW_WORKSPACE = "agent-inference-service";
    };
    tokenFile = "/run/secrets/mlflow_access_token";
  };
in
  pkgs.runCommand "mlflow-mcp-test" {
    nativeBuildInputs = [mlflow-mcp];
  } ''
    set -eu
    work=$PWD/work
    mkdir -p "$work"

    test -x "$(command -v mlflow-mcp)"

    mlflow-mcp --help > "$work/usage" 2>&1
    grep -qi 'mcp' "$work/usage"

    # The configured wrapper exports the deployment env as overridable
    # defaults, points the CF provider at cloudflared, and reads the access
    # token from its secret file at invocation time.
    ${configured}/bin/mlflow-mcp --help > "$work/configured-usage" 2>&1
    grep -qi 'mcp' "$work/configured-usage"

    wrapper=${configured}/bin/mlflow-mcp
    grep -q 'MLFLOW_TRACKING_URI' "$wrapper"
    grep -q 'cloudflare-access' "$wrapper"
    grep -q 'agent-inference-service' "$wrapper"
    grep -q 'mlflow_access_token' "$wrapper"
    grep -q '${pkgs.cloudflared}/bin/cloudflared' "$wrapper"

    touch "$out"
  ''
