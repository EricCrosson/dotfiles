{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule {
  pname = "showboat";
  version = "0.6.1";

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "showboat";
    tag = "v0.6.1";
    hash = "sha256-yYK6j6j7OgLABHLOSKlzNnm2AWzM2Ig76RJypBsBnkI=";
  };

  vendorHash = "sha256-mGKxBRU5TPgdmiSx0DHEd0Ys8gsVD/YdBfbDdSVpC3U=";

  # Tests require python3 and other external tools not available in the sandbox
  doCheck = false;

  meta = {
    description = "Executable demo documents for agents";
    homepage = "https://github.com/simonw/showboat";
    license = lib.licenses.asl20;
    mainProgram = "showboat";
  };
}
