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
    op-plugin-claude-test = {
      enable = true;
      name = "op-plugin-claude-test";
      entry = toString (writeShellScript "op-plugin-claude-test" ''
        export PATH="${go}/bin:$PATH"
        export GOTOOLCHAIN=local
        cd pkgs/op-plugins/claude
        go test
      '');
      language = "system";
      pass_filenames = false;
      files = "^pkgs/op-plugins/claude/.*\\.(go|nix)$";
      stages = ["pre-commit"];
    };
    op-plugin-jira-test = {
      enable = true;
      name = "op-plugin-jira-test";
      entry = toString (writeShellScript "op-plugin-jira-test" ''
        export PATH="${go}/bin:$PATH"
        export GOTOOLCHAIN=local
        cd pkgs/op-plugins/jira
        go test
      '');
      language = "system";
      pass_filenames = false;
      files = "^pkgs/op-plugins/jira/.*\\.(go|nix)$";
      stages = ["pre-commit"];
    };
    op-plugin-git-disjoint-test = {
      enable = true;
      name = "op-plugin-git-disjoint-test";
      entry = toString (writeShellScript "op-plugin-git-disjoint-test" ''
        export PATH="${go}/bin:$PATH"
        export GOTOOLCHAIN=local
        cd pkgs/op-plugins/git-disjoint
        go test
      '');
      language = "system";
      pass_filenames = false;
      files = "^pkgs/op-plugins/git-disjoint/.*\\.(go|nix)$";
      stages = ["pre-commit"];
    };
  };
}
