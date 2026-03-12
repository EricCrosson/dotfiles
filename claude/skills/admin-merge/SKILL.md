---
name: admin-merge
description: Use when merging the current branch into master via admin merge commit and closing the Jira ticket
---

<required>
Add these steps to your task list:

1. Push current branch and create PR targeting master
2. Admin-merge PR with merge commit: `gh pr merge --admin --merge`
3. Spawn acli agent to transition Jira ticket to Done (ticket key from chat context)
   </required>

<system-reminder>
If `gh pr merge --admin --merge` fails due to branch protection rules, do NOT attempt workarounds. Tell the user to adjust the repository's branch protection settings to allow admin merges, then re-run this skill.
</system-reminder>
