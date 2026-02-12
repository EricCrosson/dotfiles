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

ARGS=(
  -title "$NOTIF_TITLE"
  -message "$MESSAGE"
  -activate net.kovidgoyal.kitty
  -group "claude-code-${CWD}"
)

if [ -n "${CLAUDE_NOTIFICATION_ICON:-}" ] && [ -f "$CLAUDE_NOTIFICATION_ICON" ]; then
  ARGS+=(-appIcon "$CLAUDE_NOTIFICATION_ICON")
fi

terminal-notifier "${ARGS[@]}"
