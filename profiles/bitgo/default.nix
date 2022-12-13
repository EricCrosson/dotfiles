{
  pkgs,
  user,
  ...
}: {
  home = {
    username = "${user.username}";
    homeDirectory = "${user.homeDirectory}";
    stateVersion = "22.05";

    packages = with pkgs; [
      amazon-ecr-credential-helper
      dive
      go-jira
      kubectl
      yq-go

      # Still missing
      # kubectx
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
