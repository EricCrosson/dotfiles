{profile, ...}: {
  sops.defaultSopsFile = ../secrets/main.yaml;
  sops.secrets =
    {
      github_token_personal = {
        mode = "0400";
        owner = profile.username;
        group = profile.username;
      };
    }
    // (
      if profile.organization == "bitgo"
      then {
        github_ssh_private_key_personal = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
        github_token_bitgo = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
        jira_token_bitgo = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
        youtube_api_key = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
      }
      # This means user.organization == "personal", but this
      # would ideally be type-checked and exhaustive
      else {
      }
    );
}
