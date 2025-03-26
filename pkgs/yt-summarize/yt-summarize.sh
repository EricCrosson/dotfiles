#!/usr/bin/env bash
#
# yt-summarize - Extract and summarize content from YouTube videos
#
# DESCRIPTION:
#   This script uses the Fabric AI tool to extract key insights and wisdom
#   from YouTube videos. It processes a YouTube URL, extracts the video ID and title,
#   saves the summarized content to a cache file, and displays it in the terminal.
#
# USAGE:
#   yt-summarize <youtube-url>
#
# ARGUMENTS:
#   youtube-url    URL to a YouTube video (e.g., https://www.youtube.com/watch?v=tsSDvflzJbc)
#
# OPTIONS:
#   None currently supported
#
# OUTPUT:
#   - Displays the extracted wisdom in the terminal
#   - Saves the content to a cache file in ${XDG_CACHE_HOME:-$HOME/.cache}/yt-summarize/
#
# EXAMPLES:
#   yt-summarize https://www.youtube.com/watch?v=tsSDvflzJbc
#
# AUTHOR:
#   Eric Crosson
#

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $(basename "$0") <youtube-url>"
  exit 1
fi

url="$1"
video_id=$(echo "$url" | grep --only-matching --extended-regexp 'v=[A-Za-z0-9_-]+' | cut --delimiter='=' --fields=2)

if [ -z "$video_id" ]; then
  echo "Error: Could not extract video ID from URL"
  exit 1
fi

# Get video title to use for filename
title=$(curl --silent "https://www.youtube.com/oembed?url=$url&format=json" | jq --raw-output '.title')
filename=$(echo "$title" | tr '[:upper:]' '[:lower:]' | sed --regexp-extended 's/[^a-z0-9]+/_/g' | sed --regexp-extended 's/^_+|_+$//g')

# Create XDG cache directory if it doesn't exist
cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/yt-summarize"
mkdir --parents "$cache_dir"
cache_file="$cache_dir/${filename:-$video_id}.txt"

# Run fabric command and cache the output
echo "Extracting wisdom from YouTube video: $title"
echo "Cache file: $cache_file"

# Always use tee to display output and save to file
fabric --stream --pattern extract_wisdom --youtube "$url" | tee "$cache_file"

echo "Summary saved to: $cache_file"
