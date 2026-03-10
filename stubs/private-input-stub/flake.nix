{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    # deadnix: skip
    self,
    nixpkgs,
  }: let
    inherit (nixpkgs) lib;
  in {
    packages = lib.genAttrs ["aarch64-darwin"] (system: let
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

    # Stub for cortex and atlas home-manager modules
    homeManagerModules.default = {
      lib,
      pkgs,
      ...
    }: let
      atlasStub = pkgs.stdenv.mkDerivation {
        pname = "atlas-stub";
        version = "0.0.0";
        dontUnpack = true;
        installPhase = ''
          mkdir -p $out/bin
          echo '#!/bin/sh' > $out/bin/atlas
          chmod +x $out/bin/atlas
        '';
      };
    in {
      options.programs.cortex = {
        enable = lib.mkEnableOption "cortex";
        url = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "Cortex server URL";
        };
      };
      options.programs.atlas = {
        enable = lib.mkEnableOption "atlas";
        package = lib.mkOption {
          type = lib.types.package;
          default = atlasStub;
        };
        settings = lib.mkOption {
          type = lib.types.anything;
          default = {};
        };
        zshIntegration = lib.mkOption {
          type = lib.types.str;
          default = "none";
        };
        initScript = lib.mkOption {
          type = lib.types.path;
          readOnly = true;
          description = "Pre-rendered zsh init script.";
        };
      };
      config.programs.atlas.initScript = pkgs.writeText "atlas-init-zsh-stub" "";
    };

    # Stub for cortex library
    lib.cortex-instructions = "";
  };
}
