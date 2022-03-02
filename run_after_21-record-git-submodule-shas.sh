#!/usr/bin/env bash

readonly chezmoi_dir="${HOME}/.local/share/chezmoi"
readonly git_submodule_sha_dir="${chezmoi_dir}/.git-submodule-shas"

cd "${chezmoi_dir}" || exit
mkdir -p "${git_submodule_sha_dir}"

IFS=$'\n'

for submodule in $(git submodule status)
do
  # drop the dirty bit
  description="${submodule:1}"
  repo="$(echo "$description" | awk '{print $2}')"
  hash="$(echo "$description" | awk '{print $1}')"

  echo "$hash" > "${git_submodule_sha_dir}/${repo}"
done
