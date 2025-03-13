{user, ...}: {
  sops.defaultSopsFile = ../secrets/main.yaml;
  sops.secrets =
    {
      github_token_personal = {
        mode = "0400";
        owner = user.username;
        group = user.username;
      };
    }
    // (
      if user.organization == "bitgo"
      then {
        github_ssh_private_key_personal = {
          mode = "0400";
          owner = user.username;
          group = user.username;
        };
        github_token_bitgo = {
          mode = "0400";
          owner = user.username;
          group = user.username;
        };
        jira_token_bitgo = {
          mode = "0400";
          owner = user.username;
          group = user.username;
        };
        youtube_api_key = {
          mode = "0400";
          owner = user.username;
          group = user.username;
        };
      }
      # This means user.organization == "personal", but this
      # would ideally be type-checked and exhaustive
      else {
      }
    );
}
