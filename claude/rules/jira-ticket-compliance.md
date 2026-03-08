# Jira Ticket Compliance

<required>
Run `git config user.email`. Skip this rule unless the result is `ericcrosson@bitgo.com`.

Before your first commit, ensure a Jira ticket is associated with this task:

1. If I gave you a ticket, use it
2. Otherwise, use `/create-jira-ticket` — include assignee (`@me`) and
   priority in the creation payload (do not create-then-set separately)
3. Transition the ticket: **In Progress** when starting work, **In Review**
   when pushing or creating a PR
4. Include a `Ticket: XX-1234` trailer in every commit message
</required>

<system-reminder>
If a commit-msg or pre-push hook rejects a commit for a missing ticket
reference, create a ticket and retry.
</system-reminder>

<good-example>
feat(api): add rate limiting

Ticket: DX-1234
</good-example>

<bad-example>
feat(api): add rate limiting
</bad-example>
