{
  config,
  lib,
  ...
}:
with lib; {
  options.services-options = {
    litellm-proxy = {
      enabled = mkOption {
        type = types.bool;
        default = config.services.litellm-proxy.enable or false;
        description = "Whether LiteLLM proxy service is enabled";
        readOnly = true;
      };

      host = mkOption {
        type = types.str;
        default = "localhost";
        description = "Hostname for the LiteLLM proxy service";
      };

      port = mkOption {
        type = types.port;
        default = 4000;
        description = "Port for the LiteLLM proxy service";
      };

      # Break circular reference by using a fixed string template that the module applies its values to
      baseUrl = mkOption {
        type = types.str;
        default = "http://${config.services-options.litellm-proxy.host}:${toString config.services-options.litellm-proxy.port}";
        description = "Base URL for the LiteLLM proxy service";
        readOnly = true;
      };

      apiUrl = mkOption {
        type = types.str;
        default = "${config.services-options.litellm-proxy.baseUrl}/v1";
        description = "API URL for the LiteLLM proxy service";
        readOnly = true;
      };
    };
  };
}
