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
    atlas = {
      url = "git+ssh://git@github.com-bitgo/BitGo/atlas";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        crane.follows = "crane";
        fenix.follows = "fenix";
      };
    };
    cortex = {
      url = "git+ssh://git@github.com-bitgo/BitGo/cortex";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        crane.follows = "crane";
        fenix.follows = "fenix";
      };
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
    };
    gh-automerge = {
      url = "github:ericcrosson/gh-automerge";
    };
    git-diff-regex = {
      url = "github:ericcrosson/git-diff-regex";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-disjoint = {
      url = "github:ericcrosson/git-disjoint";
    };
    git-dl = {
      url = "github:ericcrosson/git-dl";
    };
    git-review = {
      url = "github:ericcrosson/git-review";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:helix-editor/helix";
      # QUESTION: does this break caching?
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # QUESTION: Is this providing any value?
    mcp-servers-nix = {
      url = "github:natsukium/mcp-servers-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
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
    retry = {
      url = "github:ericcrosson/retry";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig.extra-substituters = [
    "https://ericcrosson.cachix.org"
    "https://ericcrosson-gh-arm.cachix.org"
    "https://ericcrosson-gh-automerge.cachix.org"
    "https://ericcrosson-git-dl.cachix.org"
    "https://ericcrosson-retry.cachix.org"
    "https://git-disjoint.cachix.org"
    "https://helix.cachix.org"
    "https://nix-community.cachix.org"
  ];

  nixConfig.extra-trusted-public-keys = [
    "ericcrosson.cachix.org-1:M0b4GgWNxAXJSxBhwj7O8wBV4LerI6xc7W83DZp47ww="
    "ericcrosson-gh-arm.cachix.org-1:F/7TLvBGov4xjEE3MSRgNNmYRxPDxW8BUeAyy0rbVu="
    "ericcrosson-gh-automerge.cachix.org-1:GbweXYwKQnDBSkYtrGQQ9qYfsjh2Ar9CYUoa78pvqRM="
    "ericcrosson-git-dl.cachix.org-1:qhFI0OIKhtlyEQeKRnyfXryIiDkk/p8R77xfjiOfntM="
    "ericcrosson-retry.cachix.org-1:3l9PEQ/c6PDI+P+avNRPS40irPntuh9hldgjXMttWIs="
    "git-disjoint.cachix.org-1:KldtYCsH4nhDDfgWwx79lZF75P1smpKtjzoENpwDynw="
    "helix.cachix.org-1:ejp9KQpR1FBI2onstMQ34yogDm4OgU2ru6lIwPvuCVs="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
}
