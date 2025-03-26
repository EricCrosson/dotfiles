{
  lib,
  pkgs,
}:
pkgs.writeShellApplication {
  name = "yt-summarize";

  runtimeInputs = with pkgs; [
    curl
    fabric-ai
    jq
  ];

  text = builtins.readFile ./yt-summarize.sh;

  meta = with lib; {
    description = "Extract and summarize wisdom from YouTube videos using Fabric and cache the results";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
