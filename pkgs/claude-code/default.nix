{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
}: let
  version = "0.2.27";
in
  stdenv.mkDerivation {
    pname = "claude-code";
    inherit version;

    src = fetchFromGitHub {
      owner = "anthropics";
      repo = "claude-code";
      rev = "v${version}";
      sha256 = lib.fakeHash; # Replace with actual hash after first build attempt
    };

    nativeBuildInputs = [makeWrapper];

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin
      cp claude $out/bin/claude
      wrapProgram $out/bin/claude \
        --set ANTHROPIC_MODEL "arn:aws:bedrock:us-west-2::foundation-model/anthropic.claude-3-7-sonnet-20250219-v1:0" \
        --set AWS_PROFILE "dev" \
        --set AWS_REGION "us-west-2" \
        --set CLAUDE_CODE_USE_BEDROCK "1" \
        --set DISABLE_PROMPT_CACHING "1"

      runHook postInstall
    '';

    meta = with lib; {
      description = "Claude AI in your terminal";
      homepage = "https://github.com/anthropics/claude-code";
      license = licenses.unfree;
      platforms = platforms.all;
      maintainers = with maintainers; [ericcrosson];
    };
  }
