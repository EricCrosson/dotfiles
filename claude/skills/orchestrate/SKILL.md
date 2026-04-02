---
name: orchestrate
description: Use when a task should be parallelized across multiple agents —
  decomposes work into Linear issues, then drains the queue by spawning
  worktree agents for unblocked tickets using drive-to-completion
---

<required>
Phase 1 — Decompose
1. Break the task into subtasks with dependency edges
2. Invoke /create-linear-issue for each subtask
3. Set blocking relations via `blockedBy`/`blocks` fields on save_issue

Phase 2 — Drain the queue 4. Spawn a background Agent for each unblocked ticket (run_in_background: true, isolation: "worktree") 5. As agents complete, spawn newly-unblocked tickets 6. When the queue is empty, report results (branches, PRs, issues)
</required>

## Agent prompt

Each spawned agent's prompt must include:

- The Linear issue identifier and description
- Instructions to re-read the issue before starting — others may have updated it
- Implement the issue directly using /drive-to-completion (do NOT enter plan mode or use /review-plan)
- Completion means, in this order: PR merged, change demoed in target
  environment, issue transitioned to Done after it's been proven to work as
  expected

<good-example>
"Work on CTX-105: Add rate limiting middleware.
Re-read the issue first — context may have changed.
Implement it now using /drive-to-completion. Do not use /plan or /review-plan.
Done means: PR merged, change demoed in target environment, CTX-105 set to Done."
</good-example>
