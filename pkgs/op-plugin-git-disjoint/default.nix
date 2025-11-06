{buildGoModule}:
buildGoModule {
  pname = "op-plugin-git-disjoint";
  version = "0.1.0";

  src = ./.;

  vendorHash = "sha256-zojR9VfCtOwjF9ipwsnF+43anXcFXPaOCHO1Py26o24=";

  meta = {
    description = "1Password shell plugin for git-disjoint";
    homepage = "https://github.com/ericcrosson/git-disjoint";
    mainProgram = "op-plugin-git-disjoint";
  };
}
