#!/usr/bin/env bash
INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd')
MESSAGE=$(echo "$INPUT" | jq -r '.message')
TITLE=$(echo "$INPUT" | jq -r '.title // empty')

REPO=$(echo "$CWD" | rev | cut -d/ -f1-2 | rev)

NOTIF_TITLE="$REPO"
if [ -n "$TITLE" ]; then
  NOTIF_TITLE="$REPO: $TITLE"
fi

osascript - "$NOTIF_TITLE" "$MESSAGE" <<'APPLESCRIPT'
on run argv
  display notification (item 2 of argv) with title (item 1 of argv)
end run
APPLESCRIPT
