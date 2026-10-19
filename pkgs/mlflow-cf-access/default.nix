{
  lib,
  buildPythonPackage,
  setuptools,
}:
buildPythonPackage rec {
  pname = "mlflow-cf-access";
  version = "0.1.0";
  pyproject = true;

  src = ./.;

  build-system = [setuptools];

  # mlflow (the RequestAuthProvider API) and requests are provided by the
  # consuming environment; nothing to install at build time.
  dependencies = [];

  doCheck = false;

  meta = {
    description = "MLflow request-auth provider that attaches the Cloudflare Access JWT";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
