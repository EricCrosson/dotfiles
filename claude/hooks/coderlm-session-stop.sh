#!/usr/bin/env bash
# Save annotations and clean up coderlm session on Stop.
# Must never block or fail loudly.

STATE_FILE=".claude/coderlm_state/session.json"

if [ -f "$STATE_FILE" ] && curl -s --max-time 2 http://127.0.0.1:3000/api/v1/health > /dev/null 2>&1; then
    coderlm-cli save-annotations 2>/dev/null || true
    coderlm-cli cleanup 2>/dev/null || true
fi
