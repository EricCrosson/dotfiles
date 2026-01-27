{buildGoModule}:
buildGoModule {
  pname = "op-plugin-jira";
  version = "0.1.0";

  src = ./.;

  vendorHash = "sha256-T5wY6DshhHITCEnHG7229UsTYYE5NPMUWz6xqJ4G6yc=";

  meta = {
    description = "1Password shell plugin for Jira CLI";
    homepage = "https://github.com/ankitpokhrel/jira-cli";
    mainProgram = "op-plugin-jira";
  };
}
