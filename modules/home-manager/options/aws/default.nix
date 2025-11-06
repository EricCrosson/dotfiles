{lib, ...}:
with lib; {
  options.aws-options = {
    profile = {
      default = mkOption {
        type = types.str;
        default = "dev";
        description = "Default AWS profile to use for authentication";
      };
    };

    region = {
      default = mkOption {
        type = types.str;
        default = "us-west-2";
        description = "Default AWS region to use for services";
      };
    };
  };
}
