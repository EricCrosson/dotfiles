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
      atlas_bedrock_model_id = mkOption {
        type = types.str;
        readOnly = true;
      };
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
        atlas_bedrock_model_id = {};
        bedrock_opus_arn = {};
        bedrock_sonnet_arn = {};
        bedrock_haiku_arn = {};
      };
    };

    bitgo.sops.secretPaths =
      if cfg.enable
      then {
        atlas_bedrock_model_id = config.sops.secrets.atlas_bedrock_model_id.path;
        bedrock_opus_arn = config.sops.secrets.bedrock_opus_arn.path;
        bedrock_sonnet_arn = config.sops.secrets.bedrock_sonnet_arn.path;
        bedrock_haiku_arn = config.sops.secrets.bedrock_haiku_arn.path;
      }
      else {
        atlas_bedrock_model_id = "/dev/null";
        bedrock_opus_arn = "/dev/null";
        bedrock_sonnet_arn = "/dev/null";
        bedrock_haiku_arn = "/dev/null";
      };
  };
}
