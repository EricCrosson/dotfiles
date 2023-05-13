{
  system,
  pre-commit-hooks,
}:
pre-commit-hooks.lib.${system}.run {
  src = ../.;
  hooks = {
    actionlint.enable = true;
    alejandra.enable = true;
    deadnix.enable = true;
    prettier.enable = true;
    statix.enable = true;
    stylua.enable = true;
  };
}
