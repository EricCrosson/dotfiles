{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertEq assertHasAttr assertNotHasAttr;

  # Helper to evaluate the module with a given config
  eval = testConfig:
    (lib.evalModules {
      modules = [
        # The module under test
        ../modules/home-manager/services/litellm-proxy/default.nix

        # Stub option declarations for outputs the module writes to
        {
          options = {
            home = {
              file = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = {};
              };
              homeDirectory = lib.mkOption {
                type = lib.types.str;
                default = "/home/testuser";
              };
            };
            launchd-with-logs.services = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
          };
        }

        # Test-specific configuration
        testConfig
      ];
      specialArgs = {
        inherit pkgs;
        inputs = {};
      };
    })
    .config;

  # === Test cases ===

  # Test: Model with aws_profile_name includes it in litellm_params
  test-aws-profile = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            model = "bedrock/test";
            aws_profile_name = "dev";
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
    modelParams = (builtins.head configJson.model_list).litellm_params;
  in
    assert assertEq "aws-profile" modelParams.aws_profile_name "dev"; true;

  # Test: Model without aws_profile_name omits it from litellm_params
  test-no-aws-profile = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            model = "bedrock/test";
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
    modelParams = (builtins.head configJson.model_list).litellm_params;
  in
    assert assertNotHasAttr "no-aws-profile" modelParams "aws_profile_name"; true;

  # Test: extraConfig JSON merged into litellm_params
  test-extra-config = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            model = "bedrock/test";
            extraConfig = ''{"api_key": "test-key"}'';
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
    modelParams = (builtins.head configJson.model_list).litellm_params;
  in
    assert assertEq "extra-config" modelParams.api_key "test-key"; true;

  # Test: Output has correct model_list structure
  test-json-structure = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            model = "bedrock/test";
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
  in
    assert assertHasAttr "model-list" configJson "model_list";
    assert assertEq "model-count" (builtins.length configJson.model_list) 1;
    assert assertEq "model-name" (builtins.head configJson.model_list).model_name "test-model";
    assert assertEq "model-id" (builtins.head configJson.model_list).litellm_params.model "bedrock/test"; true;

  # Test: Model with modelFile uses placeholder in config
  test-model-file = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            modelFile = "/run/secrets/bedrock_arn";
            aws_profile_name = "bitgo-ai";
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
    modelParams = (builtins.head configJson.model_list).litellm_params;
  in
    assert assertEq "model-file-placeholder" modelParams.model "@MODEL_FILE_${builtins.hashString "sha256" "/run/secrets/bedrock_arn"}@";
    assert assertEq "model-file-aws-profile" modelParams.aws_profile_name "bitgo-ai"; true;

  # Test: modelFile causes wrapper to be used instead of direct litellm
  test-model-file-uses-wrapper = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "test-model";
            modelFile = "/run/secrets/bedrock_arn";
          }
        ];
      };
    };
    service = result.launchd-with-logs.services.litellm-proxy;
  in
    assert assertEq "wrapper-no-args" (service.args or []) [];
    assert assertEq "wrapper-command-contains-wrapper" (lib.hasInfix "litellm-proxy-wrapper" service.command) true; true;

  # Test: Multiple models
  test-multiple-models = let
    result = eval {
      services.litellm-proxy = {
        enable = true;
        package = pkgs.hello;
        aws-saml = pkgs.hello;
        models = [
          {
            name = "model-a";
            model = "bedrock/a";
          }
          {
            name = "model-b";
            model = "bedrock/b";
            aws_profile_name = "prod";
          }
        ];
      };
    };
    configJson = builtins.fromJSON result.home.file.".config/litellm/config.yaml".text;
  in
    assert assertEq "multiple-model-count" (builtins.length configJson.model_list) 2;
    assert assertEq "first-model-name" (builtins.elemAt configJson.model_list 0).model_name "model-a";
    assert assertEq "second-model-name" (builtins.elemAt configJson.model_list 1).model_name "model-b";
    assert assertHasAttr "second-model-aws" (builtins.elemAt configJson.model_list 1).litellm_params "aws_profile_name"; true;
in
  # Force evaluation of all tests
  assert test-aws-profile;
  assert test-no-aws-profile;
  assert test-extra-config;
  assert test-json-structure;
  assert test-model-file;
  assert test-model-file-uses-wrapper;
  assert test-multiple-models; "all tests passed"
