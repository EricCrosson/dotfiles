#!/usr/bin/env bash
# vim: filetype=sh

readonly prefix="${HOME}/.local/share/dotfiles"
readonly helix_repository="https://github.com/helix-editor/helix"
readonly helix_directory="${prefix}/helix"
readonly helix_runtime_directory="${HOME}/.config/helix/runtime"

install_helix() {
  mkdir -p "${prefix}"

  git clone \
    --recurse-submodules \
    --shallow-submodules \
    -j "$(nproc)" \
    "${helix_repository}" \
    "${helix_directory}"

  # compile helix
  cd "${helix_directory}" || exit
  cargo install --path helix-term

  # copy the helix runtme as instructed in the readme
  rm -rf "${helix_runtime_directory}"
  cp -r "${helix_directory}/runtime" "${helix_runtime_directory}"
}

if [ ! -d "${helix_directory}" ]; then
  install_helix
fi
