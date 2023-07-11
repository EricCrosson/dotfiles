{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      awscli2
      dive
      go-jira
      k9s
      kubectl
      kubectx
      yq-go
    ];

    file = {
      ".config/git/personal-config".source = ../../.config/git/personal-config;

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
        key = "0AB49222C769F13EDE3EBFB2352FBA3B4180A44A";
        signByDefault = true;
      };
      extraConfig = {
        includeIf = {
          "gitdir:~/workspace/EricCrosson/" = {
            path = "~/.config/git/personal-config";
          };
          "gitdir:~/workspace/semantic-release-action/" = {
            path = "~/.config/git/personal-config";
          };
          "gitdir:~/workspace/typescript-tools/" = {
            path = "~/.config/git/personal-config";
          };
        };
      };
    };
  };
}
