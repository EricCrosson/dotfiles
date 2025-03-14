{
  inputs,
  withSystem,
  ...
}: let
  sharedModules = [
    ./eric
    # DISCUSS: a lightweight helix -- can we download from a precompiled release?
    #          https://github.com/helix-editor/helix/releases/tag/23.05
    # ../home/editor/helix
  ];

  # DISCUSS: is coupling to the host the right direction for a profile here?
  # DISCUSS: a lightweight helix -- can we download from a precompiled release?
  #          https://github.com/helix-editor/helix/releases/tag/23.05
  homeImports = {
    "ericcrosson@MBP-0954" =
      sharedModules
      ++ [
        ./bitgo
        ./development
        ../home/editor/helix
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
      // withSystem "aarch64-linux" ({pkgs, ...}: {
        "eric@bain" = homeManagerConfiguration {
          modules = homeImports."eric@bain";
          inherit pkgs;
        };
      });
  };
}
