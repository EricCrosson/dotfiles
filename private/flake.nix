{
  description = "Eric's private nix-darwin configuration";

  inputs = {
    public.url = "path:..";

    nixpkgs.follows = "public/nixpkgs";
    _1password-shell-plugins = {
      url = "github:1Password/shell-plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bash-barrier.follows = "public/bash-barrier";
    bell.follows = "public/bell";
    crane.follows = "public/crane";
    disko.follows = "public/disko";
    fenix.follows = "public/fenix";
    gh-arm.follows = "public/gh-arm";
    gh-automerge.follows = "public/gh-automerge";
    git-diff-regex.follows = "public/git-diff-regex";
    git-disjoint.follows = "public/git-disjoint";
    git-dl.follows = "public/git-dl";
    git-review.follows = "public/git-review";
    helix.follows = "public/helix";
    home-manager.follows = "public/home-manager";
    mcp-servers-nix.follows = "public/mcp-servers-nix";
    nix-index-database.follows = "public/nix-index-database";
    nix-darwin.follows = "public/nix-darwin";
    npm-dep-version.follows = "public/npm-dep-version";
    retry.follows = "public/retry";
    sops-nix.follows = "public/sops-nix";

    aws-console-bitgo = {
      url = "git+ssh://git@github.com-bitgo/bitgo/aws-console";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aws-saml-bitgo = {
      url = "git+ssh://git@github.com-bitgo/bitgo/aws-saml";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gh-gantt = {
      url = "git+ssh://git@github.com-bitgo/bitgo/gh-gantt";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gh-endorse = {
      url = "git+ssh://git@github.com-bitgo/bitgo/gh-endorse";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: let
    hostBuilder = import ../hosts {inherit inputs;};
  in {
    darwinConfigurations =
      builtins.mapAttrs
      (hostName: hostConfig:
        hostBuilder.mkDarwinHost {
          inherit hostName hostConfig;
        })
      hostBuilder.hosts;
  };
}
