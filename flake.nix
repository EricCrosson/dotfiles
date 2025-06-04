{
  description = "Eric's nix-darwin and Home Manager flake";

  outputs = {
    # deadnix: skip
    self,
    home-manager,
    nix-darwin,
    nixpkgs,
    ...
  } @ inputs: let
    preferences = {
      theme = "Mocha";
    };
    # REFACTOR: rename as `profiles`
    profile = {
      # REFACTOR: can we type this? Using mkOption or something
      bitgo = rec {
        inherit preferences;
        username = "ericcrosson";
        organization = "bitgo";
        email = "${username}@bitgo.com";
        homeDirectory = "/Users/${username}";
      };
    };
  in {
    darwinConfigurations.MBP-0954 = let
      user = profile.bitgo;
      pkgs = import inputs.nixpkgs {
        system = "aarch64-darwin";
        config = {
          allowUnfree = true;
          allowBroken = true; # Needed for open-webui
        };
        overlays = [
          inputs.fenix.overlays.default
          (import ./overlays).combined
        ];
      };
    in
      nix-darwin.lib.darwinSystem {
        modules = [
          hosts/MBP-0954/configuration.nix
          home-manager.darwinModules.home-manager
          modules/darwin
          {
            users.users.${user.username}.home = user.homeDirectory;
            home-manager = {
              extraSpecialArgs = {inherit pkgs inputs user;};
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${user.username}.imports = [
                profiles/eric
                profiles/bitgo
                profiles/development
                home/editor/helix
              ];
            };
          }
        ];
      };
  };

  nixConfig.substituters = [
    "https://cache.nixos.org"
    "https://ericcrosson.cachix.org"
    "https://helix.cachix.org"
    "https://nix-community.cachix.org"
    "https://pre-commit-hooks.cachix.org"
  ];
  nixConfig.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "helix.cachix.org-1:ejp9KQpR1FBI2onstMQ34yogDm4OgU2ru6lIwPvuCVs="
    "ericcrosson.cachix.org-1:M0b4GgWNxAXJSxBhwj7O8wBV4LerI6xc7W83DZp47ww="
  ];

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    atuin = {
      url = "github:ellie/atuin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bash-barrier = {
      url = "github:ericcrosson/bash-barrier";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    batch-edit-prs = {
      url = "github:ericcrosson/batch-edit-prs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bell = {
      url = "github:ericcrosson/bell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-code = {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code";
      flake = false;
    };
    crane = {
      url = "github:ipetkov/crane";
    };
    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-diff-regex = {
      url = "github:ericcrosson/git-diff-regex";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-disjoint = {
      url = "github:ericcrosson/git-disjoint";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-dl = {
      url = "github:ericcrosson/git-dl";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-review = {
      url = "github:ericcrosson/git-review";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:helix-editor/helix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npm-dep-version = {
      url = "github:ericcrosson/npm-dep-version";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    percentage-changed-calculator = {
      url = "github:ericcrosson/percentage-changed-calculator";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    retry = {
      url = "github:ericcrosson/retry";
      inputs.crane.follows = "crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
