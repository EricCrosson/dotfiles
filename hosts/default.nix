{
  homeImports,
  inputs,
  withSystem,
  ...
}: let
  preferences = {
    theme = "Mocha";
  };
  # REFACTOR: rename as `profiles`
  profile = {
    # REFACTOR: can we type this? Using mkOption or something
    eric = rec {
      inherit preferences;
      username = "eric";
      organization = "personal";
      email = "eric.s.crosson@utexas.edu";
      homeDirectory = "/home/${username}";
    };
    bitgo = stdenv: rec {
      inherit preferences;
      username = "ericcrosson";
      organization = "bitgo";
      email = "${username}@bitgo.com";
      homeDirectory =
        if stdenv.isDarwin
        then "/Users/${username}"
        else "/home/${username}";
    };
  };
in {
  flake.darwinConfigurations = withSystem "aarch64-darwin" ({system, ...}: let
    pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.fenix.overlays.default
        inputs.firefox-darwin.overlay
        inputs.nur.overlays.default
      ];
    };
    inherit (pkgs) stdenv;
  in {
    MBP-0954 = let
      user = profile.bitgo stdenv;
    in
      inputs.darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./MBP-0954/configuration.nix
          inputs.home-manager.darwinModules.home-manager
          {
            users.users.${user.username}.home = user.homeDirectory;
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {inherit inputs pkgs user;};
              users.${user.username}.imports = homeImports."ericcrosson@MBP-0954";
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
            };
          }
        ];
      };
  });
}
