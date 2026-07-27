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

# Pass title/message as argv to avoid AppleScript injection; beep plays the
# system alert sound configured in System Settings > Sound > Alert Sound.
osascript - "$NOTIF_TITLE" "$MESSAGE" <<'APPLESCRIPT'
on run argv
  set notifTitle to item 1 of argv
  set notifMessage to item 2 of argv
  beep
  display notification notifMessage with title notifTitle
end run
APPLESCRIPT
