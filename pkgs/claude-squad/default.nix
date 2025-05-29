{
  lib,
  buildGoModule,
  fetchFromGitHub,
  makeWrapper,
  tmux,
  gh,
}:
buildGoModule rec {
  pname = "claude-squad";
  version = "1.0.3";

  src = fetchFromGitHub {
    owner = "smtg-ai";
    repo = "claude-squad";
    rev = "v${version}";
    hash = "sha256-rAWx3c2LNzBwSvnq5I9HKrpXWX2/6IXL4YfZtVAxEss=";
  };

  vendorHash = "sha256-MoKzShPn/JY4V7WnP6KdqdmUxxpJLInca8GdauIe66Q=";

  nativeBuildInputs = [makeWrapper];

  postInstall = ''
    wrapProgram $out/bin/claude-squad \
      --prefix PATH : ${lib.makeBinPath [tmux gh]}
  '';

  meta = with lib; {
    description = "Terminal application for managing multiple AI coding assistants in isolated workspaces";
    homepage = "https://github.com/smtg-ai/claude-squad";
    license = licenses.mit;
    maintainers = [];
  };
}
