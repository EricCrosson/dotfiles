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
        claude_code_github_token = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
        claude_code_atlassian_api_token = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
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
        github_token_bitgo_nix = {
          mode = "0400";
          owner = profile.username;
          group = profile.username;
        };
        google_service_account_private_key = {
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
