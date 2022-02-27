#!/usr/bin/env bash

install_stack() {
  if ! command -v stack &>/dev/null; then
    curl -sSL https://get.haskellstack.org/ | sh
  fi
}

install_stack
