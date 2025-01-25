# Add dependencies necessary to run the litellm proxy. I'm not sure why these
# aren't in the upstream derivation -- or maybe I'm using it incorrectly?
{pkgs}: let
  python-packages = pkgs.python313Packages;
  rq-no-tests = python-packages.rq.overridePythonAttrs (_: {doCheck = false;});
in
  python-packages.litellm.overridePythonAttrs (old: {
    propagatedBuildInputs =
      (old.propagatedBuildInputs or [])
      ++ (map (
          pkg:
            if pkg == python-packages.rq
            then rq-no-tests
            else pkg
        )
        old.optional-dependencies.proxy)
      ++ (with python-packages; [
        email-validator
        uvloop
        botocore
        boto3
      ]);
  })
