{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    # deadnix: skip
    self,
    nixpkgs,
  }: {
    packages = nixpkgs.lib.genAttrs ["aarch64-darwin"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      # Stub package for private inputs in CI.
      # Provides bin/atlas because atlas runs at build time (atlas init zsh).
      # Uses mkDerivation to set pname (required by home-manager gh module).
      # All other private inputs only need a valid package, not a real binary.
      stub = pkgs.stdenv.mkDerivation {
        pname = "private-input-stub";
        version = "0.0.0";
        dontUnpack = true;
        installPhase = ''
          mkdir -p $out/bin
          echo '#!/bin/sh' > $out/bin/atlas
          chmod +x $out/bin/atlas
        '';
      };
    in {
      default = stub;
      gh-agent = stub;
    });
  };
}
