#!/usr/bin/env bash

install_gh_extension() {
  local extension="${1:?Must supply gh extension to install}"
  if ! gh extension list | grep -q "${extension}"; then
    gh extension install "${extension}"
  fi
}

install_gh_extension mislav/gh-branch
# install_gh_extension heaths/gh-label
