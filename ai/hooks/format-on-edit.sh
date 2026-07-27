#!/usr/bin/env bash
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE_PATH" ] || [ ! -f "$FILE_PATH" ]; then
  exit 0
fi

case "$FILE_PATH" in
  *.nix)
    alejandra --quiet "$FILE_PATH" 2>/dev/null
    echo "Formatted ${FILE_PATH##*/} with alejandra" >&2
    ;;
  *.js | *.jsx | *.ts | *.tsx | *.json | *.yaml | *.yml | *.md | *.mdx | *.css | *.scss | *.html | *.graphql | *.vue | *.svelte | *.less)
    prettier --write "$FILE_PATH" 2>/dev/null
    echo "Formatted ${FILE_PATH##*/} with prettier" >&2
    ;;
esac

exit 0
