#!/usr/bin/env bash

# FIXME: this is GNU/Linux only

# https://github.com/artempyanykh/marksman/releases/
# 2022-09-13
download_link="https://github.com/artempyanykh/marksman/releases/download/2022-09-13/marksman-linux"

destination="$HOME/.local/bin/marksman"

if [[ ! -f "${destination}" ]]
then

  curl \
    --silent \
    --location \
    --output "${destination}" \
    "${download_link}"
  chmod +x "${destination}"

fi
