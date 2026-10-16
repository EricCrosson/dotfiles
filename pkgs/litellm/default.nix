# LiteLLM proxy runtime: upstream litellm plus its proxy extras in one python
# env. Composing an env (instead of overriding litellm's
# propagatedBuildInputs) keeps litellm's own store path identical to
# nixpkgs', so it substitutes from cache.nixos.org instead of being rebuilt
# (a 3.6 GB NAR) on every nixpkgs bump.
{pkgs}:
pkgs.python313.withPackages (ps:
    [ps.litellm]
    ++ ps.litellm.optional-dependencies.proxy
    ++ (with ps; [
      email-validator
      uvloop
      botocore
      boto3
      google-auth
    ]))
