#!/usr/bin/env bash

readonly prefix="${HOME}/.local/share/dotfiles"
readonly rust_analyzer_repository="https://github.com/rust-analyzer/rust-analyzer.git"
readonly rust_analyzer_directory="${prefix}/rust-analyzer"

install_rust_analyzer() {
  mkdir -p "${prefix}"

  git clone "${rust_analyzer_repository}" "${rust_analyzer_directory}"

  cd "${rust_analyzer_directory}" || exit
  cargo xtask install --server
}

if !command -v rust-analyzer &>/dev/null; then
  install_rust_analyzer
fi
