{buildNpmPackage}:
buildNpmPackage {
  pname = "md2adf";
  version = "1.0.0";
  src = ./.;
  npmDepsHash = "sha256-X8Y24wW8DJi0fawUQtvYPONymvXJDIiMsrORUKTTsRw=";
  dontNpmBuild = true;
  meta.mainProgram = "md2adf";
}
