{
  libiconv,
  libopus,
  omp,
}:
omp.overrideAttrs (old: {
  postInstall =
    (old.postInstall or "")
    + ''
      mkdir -p "$out/nix-support"
      printf '%s\n' ${libiconv} ${libopus} > "$out/nix-support/embedded-addon-runtime-libraries"
    '';
})
