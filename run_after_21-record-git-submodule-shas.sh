#!/usr/bin/env bash

readonly chezmoi_dir="${HOME}/.local/share/chezmoi"
readonly git_submodule_sha_dir="${chezmoi_dir}/.git-submodule-shas"

cd "${chezmoi_dir}" || exit
mkdir -p "${git_submodule_sha_dir}"

IFS=$'\n'

for submodule in $(git submodule status)
do
  repo="$(echo "$submodule" | cut -d' ' -f3)"
  hash="$(echo "$submodule" | cut -d' ' -f2)"

  echo "$hash" > "${git_submodule_sha_dir}/${repo}"
done
