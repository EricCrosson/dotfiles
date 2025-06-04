# This file provides a centralized way to access all overlays
# defined in this directory, either individually or as a combined overlay.
{
  # Individual overlays for selective importing
  pypandoc-disable-tests = import ./pypandoc-disable-tests.nix;

  # Combined overlay that applies all overlays in this directory
  combined = final: prev:
    builtins.foldl'
    (acc: overlay: acc // (overlay final prev))
    {}
    [
      (import ./pypandoc-disable-tests.nix)
    ];
}
