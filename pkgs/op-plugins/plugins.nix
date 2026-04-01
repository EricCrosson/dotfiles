{
  pkgs,
  inputs,
  lib ? pkgs.lib,
}: let
  opPluginLib = pkgs.callPackage ./lib.nix {};

  # git-dl: full packages (no HM modules)
  gitDlPackages = opPluginLib.buildOpPluginPackages {
    name = "git-dl";
    src = ./git-dl;
    vendorHash = "sha256-wT//dZxfhstx+BrpN0P/VrRUknUruxTjgJEgfarQzoM=";
    homepage = "https://github.com/ericcrosson/git-dl";
    description = "1Password shell plugin for git-dl";
    wrappedPackage = inputs.git-dl.packages.${pkgs.system}.default;
    wrappedBinaryName = "git-dl";
  };
in {
  # git-dl: full packages
  git-dl = gitDlPackages;

  # All plugins (for activation script)
  allPlugins = {
    plugins = [gitDlPackages.plugin];
    # Only for non-HM packages
    unwrapped = [gitDlPackages.unwrapped];
    wrappers = [gitDlPackages.wrapper];
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
