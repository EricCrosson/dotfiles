#!/usr/bin/env bash
# vim: filetype=sh

install_rust() {
  if !command -v rust &>/dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
  fi
}

install_rust
