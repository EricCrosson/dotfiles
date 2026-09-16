{
  config,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/home-manager
    ../bitgo/modules/sops.nix
  ];

  bitgo.sops.enable = true;

  programs.omp = {
    enable = true;
    package = inputs.omp.packages.${pkgs.system}.default;
    settings = {
      advisor.enabled = true;
      modelProviderOrder = [
        "openrouter"
      ];
      display.cacheMissMarker = true;
      statusLine = {
        preset = "custom";
        separator = "powerline-thin";
        leftSegments = [
          "pi"
          "vim"
          "model"
          "mode"
          "path"
          "git"
          "pr"
          "subagents"
          "context_pct"
        ];
        rightSegments = [
          "session_name"
          "cache_hit"
          "cache_read"
          "cache_write"
          "token_out"
          "cost"
        ];
      };
    };
    cacheTemperature.enable = true;
    models.providers.openrouter.apiKey = "!cat ${config.bitgo.sops.secretPaths.openrouter_api_key}";
    models.providers.litellm = {
      baseUrl = "http://127.0.0.1:4000/v1";
      apiKey = "!cat ${config.bitgo.sops.secretPaths.litellm_master_key}";
      api = "openai-completions";
      # litellm's /v1/models route requires a database connection, so list
      # models explicitly instead of using discovery.type = "litellm".
      models = [
        {
          id = "gemini-3.8-flash";
          name = "Gemini 3.8 Flash (via litellm)";
          contextWindow = 1048576;
          maxTokens = 65536;
        }
      ];
    };
    env = {
      GOOGLE_CLOUD_PROJECT = "ai-enablement-500217";
      GOOGLE_CLOUD_LOCATION = "global";
      SMART_CD_LS = "false";
      SMART_CD_GIT_STATUS = "false";
    };
  };
}
