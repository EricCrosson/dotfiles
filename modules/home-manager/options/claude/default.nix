{lib, ...}:
with lib; {
  options.claude-options = {
    bedrock = {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to use AWS Bedrock for Claude";
      };

      profile = mkOption {
        type = types.str;
        default = "dev";
        description = "AWS profile to use for Bedrock authentication";
      };

      region = mkOption {
        type = types.str;
        default = "us-west-2";
        description = "AWS region to use for Bedrock";
      };
    };

    models = {
      default = mkOption {
        type = types.str;
        default = "bedrock-claude-sonnet";
        description = "Default Claude model to use across tools";
      };

      sonnet = {
        id = mkOption {
          type = types.str;
          default = "bedrock-claude-sonnet";
          description = "Model ID for Claude Sonnet";
        };

        name = mkOption {
          type = types.str;
          default = "bedrock/us.anthropic.claude-sonnet-4-5-20250929-v1:0";
          description = "Full model name/identifier";
        };

        contextLength = mkOption {
          type = types.int;
          default = 200000;
          description = "Context length in tokens for Claude Sonnet";
        };
      };

      haiku = {
        id = mkOption {
          type = types.str;
          default = "bedrock-claude-haiku";
          description = "Model ID for Claude Haiku";
        };

        name = mkOption {
          type = types.str;
          default = "bedrock/us.anthropic.claude-3-haiku-20240307-v1:0";
          description = "Full model name/identifier";
        };

        contextLength = mkOption {
          type = types.int;
          default = 200000;
          description = "Context length in tokens for Claude Haiku";
        };
      };
    };

    # Common settings for all Claude tools
    tools = {
      disableTelemetry = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to disable telemetry for Claude tools";
      };

      disablePromptCaching = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to disable prompt caching in Claude tools";
      };

      smartCd = {
        gitStatus = mkOption {
          type = types.bool;
          default = false;
          description = "Whether to show git status in smart cd";
        };

        ls = mkOption {
          type = types.bool;
          default = false;
          description = "Whether to run ls after cd in smart cd";
        };
      };
    };
  };
}
