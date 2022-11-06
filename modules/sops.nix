{ config, ...}:

{
  sops.defaultSopsFile = ../secrets/main.yaml;
  sops.secrets.github_token_personal = {
    mode = "0400";
    # TODO: use variable for username
    owner = config.users.users.eric.name;
    # TODO: use more restrictive group
    group = "users";
  };
}
