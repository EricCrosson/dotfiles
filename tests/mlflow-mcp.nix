{
  pkgs,
  mlflowPython3,
}: let
  mlflow-mcp =
    pkgs.callPackage ../pkgs/mlflow-mcp {python3 = mlflowPython3;};
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

    touch "$out"
  ''
