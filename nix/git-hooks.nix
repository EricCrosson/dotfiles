{
  pkgs,
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
    zizmor.enable = true;
  };
  package = pkgs.prek;
}
