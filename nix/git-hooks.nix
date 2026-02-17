{
  stdenv,
  git-hooks,
  go,
  writeShellScript,
}:
git-hooks.lib.${stdenv.hostPlatform.system}.run {
  src = ../.;
  hooks = {
    actionlint.enable = true;
    alejandra = {
      enable = true;
      settings.verbosity = "quiet";
    };
    deadnix.enable = true;
    prettier.enable = true;
    ripsecrets.enable = true;
    statix.enable = true;
    trufflehog.enable = true;
    claude-wrapper-test = {
      enable = true;
      name = "claude-wrapper-test";
      entry = toString (writeShellScript "claude-wrapper-test" ''
        export PATH="${go}/bin:$PATH"
        export GOTOOLCHAIN=local
        cd pkgs/claude-wrapper
        go test
      '');
      language = "system";
      pass_filenames = false;
      files = "^pkgs/claude-wrapper/.*\\.(go|nix)$";
      stages = ["pre-commit"];
    };
  };
}
