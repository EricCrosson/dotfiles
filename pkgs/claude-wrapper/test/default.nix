{
  runCommand,
  bash,
  jq,
  bats,
  wrapperSrc,
}: let
  batsWithLibs = bats.withLibraries (p: [p.bats-support p.bats-assert]);
in
  runCommand "claude-wrapper-test" {
    nativeBuildInputs = [batsWithLibs bash jq];
  } ''
    cp ${wrapperSrc}/wrapper.sh .
    cp ${wrapperSrc}/test/wrapper.bats .
    cp -r ${wrapperSrc}/test/snapshots .
    chmod -R u+w .

    # Fix shebangs for Nix sandbox (Linux sandbox lacks /usr/bin/env)
    sed -i 's|#!/usr/bin/env bash|#!${bash}/bin/bash|g' wrapper.bats

    export HOME=$TMPDIR
    bats wrapper.bats
    touch $out
  ''
