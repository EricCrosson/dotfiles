{user, ...}: {
  sops.defaultSopsFile = ../secrets/main.yaml;
  sops.secrets =
    {
      github_token_personal = {
        mode = "0400";
        owner = user.username;
        # TODO: use more restrictive group (in this entire file)
        group = "users";
      };
    }
    // (
      if user.organization == "bitgo"
      then {
        github_token_bitgo = {
          mode = "0400";
          owner = user.username;
          group = "users";
        };
        jira_token_bitgo = {
          mode = "0400";
          owner = user.username;
          group = "users";
        };
        youtube_api_key = {
          mode = "0400";
          owner = user.username;
          group = "users";
        };
      }
      # This means user.organization == "personal", but this
      # would ideally be type-checked and exhaustive
      else {
      }
    );
}
