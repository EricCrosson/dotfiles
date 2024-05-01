{
  pkgs,
  user,
  ...
}: {
  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      # awscli2 # temporarily broken upstream
      dive
      element-desktop
      go-jira
      k9s
      kubectl
      kubectx
      gpodder
      yq-go
    ];

    file = {
      ".jira.d" = {
        # I would prefer this to be true but that doesn't appear to be working right now
        recursive = false;
        source = ../../.jira.d;
      };
      # Shell
      # REFACTOR: use shellAliases
      ".zshenv_bitgo".source = ../../.zshenv_bitgo;
      ".zshrc_bitgo".source = ../../.zshrc_bitgo;
    };
  };

  programs = {
    git = {
      signing = {
        key = "5BD755D7FD4AFCB6";
        signByDefault = true;
      };
    };
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = user.homeDirectory + "/.gnupg";
    secrets = {
      github_token_bitgo = {};
      jira_token_bitgo = {};
    };
  };
}
