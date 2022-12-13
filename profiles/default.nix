{
  inputs,
  withSystem,
  ...
}: let
  sharedModules = [
    ./eric
    ../home/editor/helix
  ];

  # DISCUSS: is coupling to the host the right direction for a profile here?
  homeImports = {
    "eric@MBP-0954" = sharedModules;

    "eric@belisaere" =
      sharedModules
      ++ [
        ../home/window-manager/awesome
      ];

    "ericcrosson@corvere" =
      sharedModules
      ++ [
        ./bitgo
        ../home/window-manager/awesome
      ];
  };

  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in {
  imports = [
    {_module.args = {inherit homeImports;};}
  ];

  flake = {
    # REFACTOR: can we avoid referencing the exact user@host twice?
    # (Once in profiles/default.nix, once in hosts/default.nix)
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
      })
      // withSystem "x86_64-linux" ({pkgs, ...}: {
        "ericcrosson@corvere" = homeManagerConfiguration {
          modules = homeImports."ericcrosson@corvere";
          inherit pkgs;
        };
      });
  };
}
