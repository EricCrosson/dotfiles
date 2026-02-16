{
  stdenv,
  git-hooks,
}:
git-hooks.lib.${stdenv.hostPlatform.system}.run {
  src = ../.;
  hooks = {
    actionlint.enable = true;
    alejandra = {
      enable = true;
      settings.verbosity = "quiet";
    };
    deadnix.enable = true;
    prettier.enable = true;
    ripsecrets.enable = true;
    statix.enable = true;
    trufflehog.enable = true;
    nix-flake-check = {
      enable = true;
      name = "nix-flake-check";
      entry = "nix flake check ./nix";
      language = "system";
      pass_filenames = false;
      always_run = true;
      stages = ["pre-push"];
    };
  };
}
