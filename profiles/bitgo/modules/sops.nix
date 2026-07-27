{
  lib,
  config,
  profile,
  ...
}:
with lib; let
  cfg = config.bitgo.sops;
in {
  options.bitgo.sops = {
    enable = mkEnableOption "BitGo sops secret decryption";
    secretPaths = {
      bedrock_opus_arn = mkOption {
        type = types.str;
        readOnly = true;
      };
      bedrock_sonnet_arn = mkOption {
        type = types.str;
        readOnly = true;
      };
      bedrock_haiku_arn = mkOption {
        type = types.str;
        readOnly = true;
      };
      openrouter_api_key = mkOption {
        type = types.str;
        readOnly = true;
      };
      linear_api_key = mkOption {
        type = types.str;
        readOnly = true;
      };
    };
  };

  config = {
    sops = mkIf cfg.enable {
      defaultSopsFile = ../../../secrets/main.yaml;
      gnupg.home = profile.homeDirectory + "/.gnupg";
      secrets = {
        aws_config = {
          path = "${config.home.homeDirectory}/.aws/config";
          mode = "0600";
        };
        bedrock_opus_arn = {};
        bedrock_sonnet_arn = {};
        bedrock_haiku_arn = {};
        openrouter_api_key = {};
        linear_api_key = {};
      };
    };

    bitgo.sops.secretPaths =
      if cfg.enable
      then {
        bedrock_opus_arn = config.sops.secrets.bedrock_opus_arn.path;
        bedrock_sonnet_arn = config.sops.secrets.bedrock_sonnet_arn.path;
        bedrock_haiku_arn = config.sops.secrets.bedrock_haiku_arn.path;
        openrouter_api_key = config.sops.secrets.openrouter_api_key.path;
        linear_api_key = config.sops.secrets.linear_api_key.path;
      }
      else {
        bedrock_opus_arn = "/dev/null";
        bedrock_sonnet_arn = "/dev/null";
        bedrock_haiku_arn = "/dev/null";
        openrouter_api_key = "/dev/null";
        linear_api_key = "/dev/null";
      };
  };
}
