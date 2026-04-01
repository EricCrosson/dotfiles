# Linear Issue Compliance

<required>
Run `git config user.email`. Skip this rule unless the result is `ericcrosson@bitgo.com`.

Before your first commit, ensure a Linear issue is associated with this task:

1. If I gave you an issue, use it
2. Otherwise, use `/create-linear-issue` — include assignee (Eric Crosson) and
   priority in the creation call (do not create-then-update separately)
3. Transition the issue: **In Progress** when starting work, **In Review**
   when pushing or creating a PR (use `mcp__claude_ai_Linear__save_issue`
   with the issue `id` and desired `state`)
4. Include a `Ticket: XX-1234` trailer in every commit message (where XX-1234
   is the Linear issue identifier)
   </required>

<system-reminder>
If a commit-msg or pre-push hook rejects a commit for a missing ticket
reference, create an issue and retry.
</system-reminder>

<good-example>
feat(api): add rate limiting

Ticket: CTX-1234
</good-example>

<bad-example>
feat(api): add rate limiting
</bad-example>
