{
  pkgs,
  mlflowPython3,
}: let
  mlflow-cf-access =
    mlflowPython3.pkgs.callPackage ../pkgs/mlflow-cf-access {};
  env = mlflowPython3.withPackages (ps: [
    ps.mlflow-skinny
    mlflow-cf-access
  ]);
in
  pkgs.runCommand "mlflow-cf-access-test" {
    nativeBuildInputs = [env];
  } ''
    set -eu
    work=$PWD/work
    mkdir -p "$work/bin"

    # A fake cloudflared that prints a JWT whose exp is far in the future.
    cat > "$work/bin/cloudflared" <<'EOF'
    #!/bin/sh
    printf '%s' "$FAKE_JWT"
    EOF
    chmod +x "$work/bin/cloudflared"

    FAKE_JWT=$(python3 - <<'PY'
    import base64, json
    def b64(data):
        return base64.urlsafe_b64encode(json.dumps(data).encode()).rstrip(b"=").decode()
    print("header." + b64({"exp": 4102444800}) + ".signature")
    PY
    )
    export FAKE_JWT
    export MLFLOW_CF_ACCESS_CLOUDFLARED=$work/bin/cloudflared

    python3 - <<'PY'
    import mlflow  # noqa: F401  (import registers mlflow.request_auth_provider entry points)
    import requests
    from mlflow.tracking.request_auth.registry import fetch_auth

    auth = fetch_auth("cloudflare-access")
    assert auth is not None, "cloudflare-access provider is not registered"
    request = requests.Request("GET", "https://mlflow.example.test/").prepare()
    auth(request)
    header = request.headers.get("CF-Access-Jwt-Assertion", "")
    assert header.startswith("header."), request.headers
    print("cloudflare-access provider ok")
    PY

    touch "$out"
  ''
