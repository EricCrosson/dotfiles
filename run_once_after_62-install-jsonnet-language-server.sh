#!/usr/bin/env bash

# FIXME: this is GNU/Linux only

# From https://github.com/grafana/jsonnet-language-server/releases
# v0.9.0
download_link="https://github.com/grafana/jsonnet-language-server/releases/download/v0.9.0/jsonnet-language-server_0.9.0_linux_amd64"

destination="$HOME/.local/bin/jsonnet-language-server"

if [[ ! -f "${destination}" ]]
then

  curl \
    --silent \
    --location \
    --output "${destination}" \
    "${download_link}"
  chmod +x "${destination}"

fi
