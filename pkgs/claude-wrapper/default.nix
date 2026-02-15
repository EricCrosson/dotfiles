{
  lib,
  writeShellApplication,
  jq,
  curl,
  _1password-cli,
  bash,
}: {
  bedrockProfile,
  bedrockRegion,
  bedrockThreshold ? 80,
  envTemplate,
  extraPathPackages ? [],
}:
writeShellApplication {
  name = "claude";
  excludeShellChecks = ["SC2016"];
  runtimeInputs = [jq curl _1password-cli bash];
  text =
    ''
      # Nix-injected configuration
      export AWS_PROFILE=${lib.escapeShellArg bedrockProfile}
      export AWS_REGION=${lib.escapeShellArg bedrockRegion}
      _BEDROCK_THRESHOLD=${toString bedrockThreshold}
      _ENV_TEMPLATE=${lib.escapeShellArg envTemplate}
      ${lib.concatMapStringsSep "\n    " (pkg: "export PATH=\"${pkg}/bin:$PATH\"") extraPathPackages}
    ''
    + builtins.readFile ./wrapper.sh;
}
