{buildGoModule}:
buildGoModule {
  pname = "op-plugin-git-disjoint";
  version = "0.1.0";

  src = ./.;

  vendorHash = "sha256-C+HpeOzrqk1aGKAKz38Yl2zaKIiv4kIZQL9Ld6AnUKM=";

  meta = {
    description = "1Password shell plugin for git-disjoint";
    homepage = "https://github.com/ericcrosson/git-disjoint";
    mainProgram = "op-plugin-git-disjoint";
  };
}
