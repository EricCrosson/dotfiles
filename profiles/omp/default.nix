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

  home.packages = [inputs.omp.packages.${pkgs.system}.default];

  programs.omp = {
    enable = true;
    settings = {
      advisor.enabled = true;
      modelProviderOrder = [
        "openrouter"
      ];
    };
    models.providers.openrouter.apiKey = "!cat ${config.bitgo.sops.secretPaths.openrouter_api_key}";
    env = {
      GOOGLE_CLOUD_PROJECT = "ai-enablement-500217";
      GOOGLE_CLOUD_LOCATION = "global";
      SMART_CD_LS = "false";
      SMART_CD_GIT_STATUS = "false";
    };
  };
}
