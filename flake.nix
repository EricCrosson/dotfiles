{
  description = "Eric's NixOS and Home-Manager flake";

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit (inputs) self;} {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      imports = [
        ./profiles
        ./hosts
      ];
    };

  nixConfig.substituters = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
    "https://helix.cachix.org"
    "https://ericcrosson.cachix.org"
  ];
  nixConfig.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "helix.cachix.org-1:ejp9KQpR1FBI2onstMQ34yogDm4OgU2ru6lIwPvuCVs="
    "ericcrosson.cachix.org-1:M0b4GgWNxAXJSxBhwj7O8wBV4LerI6xc7W83DZp47ww="
  ];

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    atuin = {
      url = "github:ellie/atuin";
      inputs.flake-utils.follows = "flake-utils";
      # don't follow nixpkgs until I'm using nixpkgs with Rust 1.67 or higher
      # https://github.com/ellie/atuin/issues/737
    };
    ast-grep = {
      url = "github:ericcrosson/escpkgs?dir=ast-grep";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bash-barrier = {
      url = "github:ericcrosson/bash-barrier";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bell = {
      url = "github:ericcrosson/bell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox = {
      url = "github:colemickens/flake-firefox-nightly";
    };
    firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
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
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kmonad = {
      url = "github:kmonad/kmonad?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
    nurl = {
      url = "github:nix-community/nurl";
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
