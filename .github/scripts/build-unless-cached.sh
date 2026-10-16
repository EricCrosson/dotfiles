#!/usr/bin/env bash
# Build a flake installable unless its output is already in
# ericcrosson.cachix.org. Evaluates once; the build realises the
# instantiated .drv instead of re-evaluating the flake.
#
# Usage: build-unless-cached.sh <installable> [extra `nix eval` args...]
set -euo pipefail

installable=$1
shift

# Eval fetches flake inputs: retry network blips.
for attempt in 1 2 3; do
  if paths=$(nix eval --json "$installable" "$@" \
    --apply 'd: { drv = d.drvPath; out = d.outPath; }'); then
    break
  fi
  if [ "$attempt" -eq 3 ]; then
    exit 1
  fi
done
drv=$(jq -r .drv <<<"$paths")
out=$(jq -r .out <<<"$paths")

if nix path-info --store https://ericcrosson.cachix.org "$out" >/dev/null 2>&1; then
  echo "::notice title=Cache hit::$out is already in ericcrosson.cachix.org; skipping build"
  exit 0
fi

retry --up-to 2x -- nix build "$drv^*" --no-link --keep-going --print-build-logs
