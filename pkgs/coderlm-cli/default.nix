# Wrapper around upstream coderlm_cli.py — gives a stable Nix store path
# that hooks and skill references can point to.
{
  writeShellScriptBin,
  python3,
  fetchurl,
}:
writeShellScriptBin "coderlm-cli" ''
  exec ${python3}/bin/python3 ${fetchurl {
    url = "https://raw.githubusercontent.com/JaredStewart/coderlm/94c587567b56ed398db35a0d444e628c08b6f2f5/plugin/skills/coderlm/scripts/coderlm_cli.py";
    hash = "sha256-4yaADFsLysQzWDdJrsJWY/yWq1mPxWuEqFky9a3Izoc=";
  }} "$@"
''
