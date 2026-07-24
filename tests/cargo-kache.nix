{pkgs}: let
  developmentProfile = import ../profiles/development {
    inherit pkgs;
    inputs = {};
    profile = {
      username = "testuser";
      homeDirectory = "/home/testuser";
    };
  };
in
  assert developmentProfile.home.file.".cargo/config.toml".text
  == ''
    [build]
    rustc-wrapper = "kache"
  ''; "all tests passed"
