{
  buildGoModule,
  writeShellScriptBin,
  lib,
  _1password-cli,
}: rec {
  # Build op-plugin binary from Go source
  buildOpPlugin = {
    name,
    src,
    vendorHash,
    homepage,
    description,
    version ? "0.1.0",
  }:
    buildGoModule {
      pname = "op-plugin-${name}";
      inherit version src vendorHash;
      meta = {
        inherit description homepage;
        mainProgram = "op-plugin-${name}";
      };
    };

  # Simple unwrapped package - direct reference to a specific package
  # Used for packages without home-manager modules (jira, git-disjoint)
  mkUnwrappedPackage = {
    name,
    package,
    binaryName ? name,
  }:
    writeShellScriptBin "${name}-unwrapped" ''
      exec ${package}/bin/${binaryName} "$@"
    '';

  # Create wrapper that calls through 'op plugin run'
  mkPluginWrapper = {
    name,
    hiPrio ? false,
  }: let
    wrapper = writeShellScriptBin name ''
      exec ${_1password-cli}/bin/op plugin run -- ${name}-unwrapped "$@"
    '';
  in
    if hiPrio
    then lib.hiPrio wrapper
    else wrapper;

  # Build everything for packages WITHOUT home-manager modules
  buildOpPluginPackages = {
    name,
    src,
    vendorHash,
    homepage,
    description,
    wrappedPackage,
    wrappedBinaryName ? name,
    wrapperHiPrio ? false,
    version ? "0.1.0",
  }: {
    plugin = buildOpPlugin {
      inherit name src vendorHash homepage description version;
    };
    unwrapped = mkUnwrappedPackage {
      inherit name;
      package = wrappedPackage;
      binaryName = wrappedBinaryName;
    };
    wrapper = mkPluginWrapper {
      inherit name;
      hiPrio = wrapperHiPrio;
    };
  };
}
