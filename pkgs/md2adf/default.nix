{buildNpmPackage}:
buildNpmPackage {
  pname = "md2adf";
  version = "1.0.0";
  src = ./.;
  npmDepsHash = "sha256-aKFrMdleM9+1srI0Xky0v5SITUJzEuyXMXAygJ6M/pc=";
  dontNpmBuild = true;
  meta.mainProgram = "md2adf";
}
