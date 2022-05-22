#!/usr/bin/env bash

readonly prefix="${HOME}/.local/share/luakit/adblock"
readonly easylist="https://easylist-downloads.adblockplus.org/easylist.txt"

mkdir -p "${prefix}"
cd "${prefix}" || exit 1

# If the file does not exist, or is older than seven days
# https://stackoverflow.com/a/32019461
if [ ! -f easylist.txt ] || [[ $(find easylist.txt -mtime +7 -print) ]]
then
  wget --output-document "${prefix}/easylist.txt" "${easylist}"
fi
