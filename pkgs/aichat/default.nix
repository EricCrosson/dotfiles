{
  lib,
  stdenv,
  darwin,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  installShellFiles,
}:
rustPlatform.buildRustPackage rec {
  pname = "aichat";
  version = "0.28.0";

  src = fetchFromGitHub {
    owner = "sigoden";
    repo = "aichat";
    rev = "v${version}";
    hash = "sha256-gs2nkZhz26tmFbAShLsFOgYt/RlPiqKTmdaPSG96m3E="; # From build error
  };

  # Use recommended cargoVendor approach
  useFetchCargoVendor = true;
  cargoHash = "sha256-cDYxT8WvryTLzBeMtp/iObdSfF84W1XT8ZN/nmoZfFY=";

  nativeBuildInputs = [
    pkg-config
    installShellFiles
  ];

  buildInputs = lib.optionals stdenv.hostPlatform.isDarwin [
    darwin.apple_sdk.frameworks.AppKit
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.Security
  ];

  postInstall = ''
    installShellCompletion ./scripts/completions/aichat.{bash,fish,zsh}
  '';

  meta = with lib; {
    description = "Use GPT-4(V), Claude, Gemini, LocalAI, Ollama and other LLMs in the terminal";
    homepage = "https://github.com/sigoden/aichat";
    license = licenses.mit;
    mainProgram = "aichat";
  };
}
