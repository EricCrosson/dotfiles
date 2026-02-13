# Rust server for CodeRLM — tree-sitter-backed codebase indexer.
# Uses edition = "2024" which requires Rust 1.85+, so we build with fenix's
# stable toolchain instead of the nixpkgs default.
{
  lib,
  fenix,
  makeRustPlatform,
  fetchFromGitHub,
  libiconv,
  stdenv,
}: let
  inherit (fenix.stable) toolchain;
  rustPlatform = makeRustPlatform {
    cargo = toolchain;
    rustc = toolchain;
  };
in
  rustPlatform.buildRustPackage {
    pname = "coderlm-server";
    version = "0.1.1-unstable-2026-02-09";

    src = fetchFromGitHub {
      owner = "JaredStewart";
      repo = "coderlm";
      rev = "94c587567b56ed398db35a0d444e628c08b6f2f5";
      hash = "sha256-AO8almhy4uXML7m/FcFXcm6ut0d87GHJY6b3U2w0xGM=";
    };

    sourceRoot = "source/server";
    cargoLock.lockFile = ./Cargo.lock;
    postPatch = ''
      cp -f ${./Cargo.lock} Cargo.lock
    '';

    buildInputs = lib.optionals stdenv.isDarwin [libiconv];

    meta = {
      description = "Tree-sitter-backed code indexer for LLM agents";
      homepage = "https://github.com/JaredStewart/coderlm";
      license = lib.licenses.mit;
      mainProgram = "coderlm-server";
    };
  }
