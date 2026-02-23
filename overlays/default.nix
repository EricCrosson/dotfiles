# This file provides a centralized way to access all overlays
{
  # Individual overlays for selective importing
  git-extras-remove-pr = import ./git-extras-remove-pr.nix;
  pypandoc-disable-tests = import ./pypandoc-disable-tests.nix;
  wakatime-disable-tests = import ./wakatime-disable-tests.nix;

  # Combined overlay that applies all overlays in this directory
  combined = final: prev: let
    overlays = [
      (import ./git-extras-remove-pr.nix)
      (import ./pypandoc-disable-tests.nix)
      (import ./wakatime-disable-tests.nix)
    ];
  in
    builtins.foldl'
    (acc: overlay: acc // (overlay final prev))
    {}
    overlays;
}
