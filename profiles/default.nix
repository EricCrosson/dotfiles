{
  inputs,
  withSystem,
  ...
}: let
  sharedModules = [
    ./eric
    ../home/editor/helix
  ];

  homeImports = {
    "eric@MBP-0954" = sharedModules;

    "eric@belisaere" =
      [
        ../home/window-manager/bspwm
      ]
      ++ sharedModules;
  };

  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in {
  imports = [
    {_module.args = {inherit homeImports;};}
  ];

  flake = {
    homeConfigurations =
      withSystem "aarch64-darwin" ({pkgs, ...}: {
        "eric@MBP-0954" = homeManagerConfiguration {
          modules = homeImports."eric@MBP-0954";
          inherit pkgs;
        };
      })
      // withSystem "x86_64-linux" ({pkgs, ...}: {
        "eric@belisaere" = homeManagerConfiguration {
          modules = homeImports."eric@belisaere";
          inherit pkgs;
        };
      });
  };
}
