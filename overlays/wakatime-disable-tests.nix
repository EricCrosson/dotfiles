_final: prev: {
  wakatime = prev.wakatime.overrideAttrs (_: {
    doCheck = false;
  });
}
