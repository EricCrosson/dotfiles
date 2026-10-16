# Binary caches every host substitutes from. CI pushes everything it builds
# to ericcrosson.cachix.org, so `darwin-rebuild`/`nixos-rebuild` download
# instead of rebuilding. Keep in sync with flake.nix nixConfig and
# NIX_EXTRA_CONF in .github/workflows/nix-flake-check.yaml.
{
  nix.settings = {
    extra-substituters = [
      "https://ericcrosson.cachix.org"
      "https://ericcrosson-gh-arm.cachix.org"
      "https://ericcrosson-gh-automerge.cachix.org"
      "https://ericcrosson-git-dl.cachix.org"
      "https://ericcrosson-retry.cachix.org"
      "https://git-disjoint.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ericcrosson.cachix.org-1:M0b4GgWNxAXJSxBhwj7O8wBV4LerI6xc7W83DZp47ww="
      "ericcrosson-gh-arm.cachix.org-1:F/7TLvBGov4xjEE3MSRgNNmYRxPDxW8BUeAyy0rbVu4="
      "ericcrosson-gh-automerge.cachix.org-1:GbweXYwKQnDBSkYtrGQQ9qYfsjh2Ar9CYUoa78pvqRM="
      "ericcrosson-git-dl.cachix.org-1:qhFI0OIKhtlyEQeKRnyfXryIiDkk/p8R77xfjiOfntM="
      "ericcrosson-retry.cachix.org-1:3l9PEQ/c6PDI+P+avNRPS40irPntuh9hldgjXMttWIs="
      "git-disjoint.cachix.org-1:KldtYCsH4nhDDfgWwx79lZF75P1smpKtjzoENpwDynw="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
