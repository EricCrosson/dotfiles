{
  pkgs,
  user,
  lib,
  config,
  ...
}: let
  aws-console = pkgs.callPackage ../../pkgs/aws-console {};
  aws-saml = pkgs.callPackage ../../pkgs/aws-saml {};
  auto-merge-previously-reviewed-api-docs-prs = pkgs.callPackage ../../pkgs/auto-merge-previously-reviewed-api-docs-prs {};
  litellm = pkgs.callPackage ../../pkgs/litellm {};
in {
  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      auto-merge-previously-reviewed-api-docs-prs
      aws-console
      aws-saml
      dive
      element-desktop
      fabric-ai
      go-jira
      k9s
      kubectl
      kubectx
      kustomize
      litellm
      openai-whisper
      yq-go
    ];

    file = {
      ".aider.conf.yml" = {
        source = ../../.aider.conf.yml;
      };
      ".config/litellm/config.yaml" = {
        source = ../../.config/litellm/config.yaml;
      };
      ".config/fabric/.env" = {
        source = ../../.config/fabric/.env;
      };
      ".config/fabric/config.yaml" = {
        source = ../../.config/fabric/config.yaml;
      };
      ".jira.d" = {
        # I would prefer this to be true but that doesn't appear to be working right now
        recursive = false;
        source = ../../.jira.d;
      };
      "Library/Application Support/io.datasette.llm/default_model.txt" = {
        source = ../.. + "/Library/Application Support/io.datasette.llm/default_model.txt";
      };
      "Library/Application Support/io.datasette.llm/extra-openai-models.yaml" = {
        source = ../.. + "/Library/Application Support/io.datasette.llm/extra-openai-models.yaml";
      };
    };
  };

  programs = {
    git = {
      signing = {
        key = "5BD755D7FD4AFCB6";
        signByDefault = true;
      };
    };

    zsh = {
      envExtra = builtins.readFile ../../zsh/bitgo_zshenv.zsh;
      initExtra = builtins.readFile ../../zsh/bitgo_zshrc.zsh;
    };
  };

  launchd = {
    agents = {
      auto-merge-previously-reviewed-api-docs-prs = {
        enable = true;
        config = {
          ProgramArguments = [
            "${auto-merge-previously-reviewed-api-docs-prs}/bin/auto-merge-previously-reviewed-api-docs-prs"
          ];
          EnvironmentVariables = {
            GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
          };
          StartInterval = 300; # every 5 minutes
          RunAtLoad = true;
          StandardOutPath = "/dev/null";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/auto-merge-previously-reviewed-api-docs-prs.error.log";
          ServiceDependencies = ["sops-nix"];
        };
      };
      litellm-proxy = {
        enable = true;
        config = {
          ProgramArguments = [
            "${litellm}/bin/litellm"
            "--config"
            "/Users/ericcrosson/.config/litellm/config.yaml"
          ];
          EnvironmentVariables = {
            PATH = lib.makeBinPath [aws-saml];
          };
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "/dev/null";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/litellm-proxy.error.log";
        };
      };
    };
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = user.homeDirectory + "/.gnupg";
    secrets = {
      github_token_bitgo = {};
      jira_token_bitgo = {};
      youtube_api_key = {};
    };
  };
}
