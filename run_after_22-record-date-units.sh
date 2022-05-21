#!/usr/bin/env bash

readonly chezmoi_dir="${HOME}/.local/share/chezmoi"
readonly date_units_dir="${chezmoi_dir}/.date-units"

cd "${chezmoi_dir}" || exit
mkdir -p "${date_units_dir}"

# Changes once per week
readonly week=$(date +%Y-%V)

echo "${week}" > "${date_units_dir}/week"
