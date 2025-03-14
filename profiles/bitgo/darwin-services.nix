{
  pkgs,
  config,
  lib,
  user,
  ...
}: let
  auto-merge-previously-reviewed-api-docs-prs = pkgs.callPackage ../../pkgs/auto-merge-previously-reviewed-api-docs-prs {};
  auto-merge-prs-that-only-bump-openapi-spec-version-numbers = pkgs.callPackage ../../pkgs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers {};
  litellm = pkgs.callPackage ../../pkgs/litellm {};
  aws-saml = pkgs.callPackage ../../pkgs/aws-saml {};

  # Nix store reference for litellm config
  litellm-config = pkgs.writeText "litellm-config.yaml" (builtins.readFile ../../.config/litellm/config.yaml);
in {
  launchd-with-logs.services = {
    # Define auto-merge-previously-reviewed-api-docs-prs service
    auto-merge-previously-reviewed-api-docs-prs = {
      command = "${auto-merge-previously-reviewed-api-docs-prs}/bin/auto-merge-previously-reviewed-api-docs-prs";
      environment = {
        GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
      };
      interval = 300; # every 5 minutes
      serviceDependencies = ["sops-nix"];
      logging = {
        stdout = "${user.homeDirectory}/Library/Logs/auto-merge-previously-reviewed-api-docs-prs.log";
        stderr = "${user.homeDirectory}/Library/Logs/auto-merge-previously-reviewed-api-docs-prs.error.log";
      };
    };

    # Define auto-merge-prs-that-only-bump-openapi-spec-version-numbers service
    auto-merge-prs-that-only-bump-openapi-spec-version-numbers = {
      command = "${auto-merge-prs-that-only-bump-openapi-spec-version-numbers}/bin/auto-merge-prs-that-only-bump-openapi-spec-version-numbers";
      environment = {
        GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
      };
      interval = 300; # every 5 minutes
      serviceDependencies = ["sops-nix"];
      logging = {
        stdout = "${user.homeDirectory}/Library/Logs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers.log";
        stderr = "${user.homeDirectory}/Library/Logs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers.error.log";
      };
    };

    # Define litellm-proxy service
    litellm-proxy = {
      command = "${litellm}/bin/litellm";
      args = [
        "--config"
        "${litellm-config}"
      ];
      environment = {
        PATH = lib.makeBinPath [aws-saml];
      };
      keepAlive = true;
      logging = {
        stdout = "${user.homeDirectory}/Library/Logs/litellm-proxy.log";
        stderr = "${user.homeDirectory}/Library/Logs/litellm-proxy.error.log";
      };
    };

    # Define colima service
    colima = {
      command = "/opt/homebrew/bin/colima";
      args = [
        "start"
        "--cpu"
        "8"
        "--memory"
        "8"
        "--arch"
        "aarch64"
        "--vm-type=vz"
        "--vz-rosetta"
      ];
      environment = {
        PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
      };
      keepAlive = false;
      logging = {
        stdout = "${user.homeDirectory}/Library/Logs/colima.log";
        stderr = "${user.homeDirectory}/Library/Logs/colima.error.log";
      };
    };

    # Add our keys using keychain once at boot time
    ssh-add-keys = {
      command = "${pkgs.keychain}/bin/keychain";
      args = [
        "--agents"
        "ssh"
        "--inherit"
        "any"
        "--systemd"
        "${user.homeDirectory}/.ssh/id_rsa"
        "${user.homeDirectory}/.ssh/id_rsa_personal"
      ];
      runAtLoad = true;
      logging = {
        stdout = "${user.homeDirectory}/Library/Logs/ssh-add-keys.log";
        stderr = "${user.homeDirectory}/Library/Logs/ssh-add-keys.error.log";
      };
    };
  };
}
