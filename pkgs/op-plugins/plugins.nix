{
  pkgs,
  inputs,
  lib ? pkgs.lib,
}: let
  opPluginLib = pkgs.callPackage ./lib.nix {};

  # Claude: plugin + unwrapped (wrapper via HM module due to MCP config)
  claudePackages = opPluginLib.buildOpPluginPackages {
    name = "claude";
    src = ./claude;
    vendorHash = "sha256-T5wY6DshhHITCEnHG7229UsTYYE5NPMUWz6xqJ4G6yc=";
    homepage = "https://claude.ai/claude-code";
    description = "1Password shell plugin for Claude Code";
    wrappedPackage = pkgs.claude-code;
    wrappedBinaryName = "claude";
  };

  # Jira/git-disjoint: full packages (no HM modules)
  jiraPackages = opPluginLib.buildOpPluginPackages {
    name = "jira";
    src = ./jira;
    vendorHash = "sha256-T5wY6DshhHITCEnHG7229UsTYYE5NPMUWz6xqJ4G6yc=";
    homepage = "https://github.com/ankitpokhrel/jira-cli";
    description = "1Password shell plugin for Jira CLI";
    wrappedPackage = pkgs.jira-cli-go;
    wrappedBinaryName = "jira";
  };

  gitDisjointPackages = opPluginLib.buildOpPluginPackages {
    name = "git-disjoint";
    src = ./git-disjoint;
    vendorHash = "sha256-C+HpeOzrqk1aGKAKz38Yl2zaKIiv4kIZQL9Ld6AnUKM=";
    homepage = "https://github.com/ericcrosson/git-disjoint";
    description = "1Password shell plugin for git-disjoint";
    wrappedPackage = inputs.git-disjoint.packages.${pkgs.system}.default;
    wrappedBinaryName = "git-disjoint";
  };
in {
  # Claude: plugin + unwrapped (no wrapper - HM module handles that)
  claude = {
    inherit (claudePackages) plugin unwrapped;
  };

  # Jira/git-disjoint: full packages
  jira = jiraPackages;
  git-disjoint = gitDisjointPackages;

  # All plugins (for activation script)
  allPlugins = {
    plugins = [claudePackages.plugin jiraPackages.plugin gitDisjointPackages.plugin];
    # Only for non-HM packages
    unwrapped = [claudePackages.unwrapped jiraPackages.unwrapped gitDisjointPackages.unwrapped];
    wrappers = [jiraPackages.wrapper gitDisjointPackages.wrapper];
  };

  # Generate activation script for installing plugins
  mkActivationScript = {pluginList}: let
    installCmd = plugin: "run cp -f ${plugin}/bin/${plugin.meta.mainProgram} ~/.op/plugins/local/";
  in ''
    run mkdir -p ~/.op/plugins/local
    run chmod 700 ~/.op ~/.op/plugins ~/.op/plugins/local 2>/dev/null || true
    ${lib.concatMapStringsSep "\n" installCmd pluginList}
  '';
}
