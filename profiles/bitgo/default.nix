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
      github_token_bitgo = {
        # TODO: really delete if this is not necessary.
        # What does that mean, that the value needs to be an empty object?
        # Strange.
        # path = "%r/github-token.txt";
      };
      jira_token_bitgo = {
        # path = "%r/jira-token.txt";
      };
    };
  };
}
