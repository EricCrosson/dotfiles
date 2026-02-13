#!/usr/bin/env bash
# Check if coderlm-server is running and auto-create session.
# Called by the SessionStart hook. Always exits 0 to never block session start.

STATE_FILE=".claude/coderlm_state/session.json"

# Check server health
if ! curl -s --max-time 2 http://127.0.0.1:3000/api/v1/health > /dev/null 2>&1; then
    echo "[coderlm] Server not running — skipping session init" >&2
    exit 0
fi

# Auto-init if no active session
if [ ! -f "$STATE_FILE" ]; then
    if ! coderlm-cli init 2>&1; then
        echo "[coderlm] Failed to initialize session" >&2
    fi
fi
