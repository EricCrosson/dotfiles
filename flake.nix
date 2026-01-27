{
  description = "Eric's nix-darwin and Home Manager flake";

  outputs = {
    # deadnix: skip
    self,
    ...
  } @ inputs: let
    hostBuilder = import ./hosts {inherit inputs;};
  in {
    darwinConfigurations =
      builtins.mapAttrs
      (hostName: hostConfig:
        hostBuilder.mkDarwinHost {
          inherit hostName hostConfig;
        })
      hostBuilder.hosts;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    _1password-shell-plugins = {
      url = "github:1Password/shell-plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aws-console-bitgo = {
      url = "git+ssh://git@github.com-bitgo/bitgo/aws-console";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aws-saml-bitgo = {
      url = "git+ssh://git@github.com-bitgo/bitgo/aws-saml";
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
    crane = {
      url = "github:ipetkov/crane";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gh-agent = {
      url = "git+ssh://git@github.com-bitgo/bitgo/gh-agent";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gh-arm = {
      url = "github:ericcrosson/gh-arm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gh-automerge = {
      url = "github:ericcrosson/gh-automerge";
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
    nix-darwin = {
      url = "github:lnl7/nix-darwin";
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
}
