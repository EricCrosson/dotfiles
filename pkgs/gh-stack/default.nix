{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule {
  pname = "gh-stack";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "github";
    repo = "gh-stack";
    tag = "v0.0.1";
    hash = "sha256-om7ekHez08X1YjP0W+3p0PxmjU/za6+/gHX5GPakKAw=";
  };

  vendorHash = "sha256-s85Lz6yfY1TiIFPolU1qESDyw8XoBORyuOMdiHj6Grc=";

  doCheck = false;

  meta = {
    description = "GitHub Stacked PRs";
    homepage = "https://github.com/github/gh-stack";
    license = lib.licenses.mit;
    mainProgram = "gh-stack";
  };
}
