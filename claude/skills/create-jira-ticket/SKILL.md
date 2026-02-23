---
name: create-jira-ticket
description: Use when creating a Jira ticket from a feature, bug, or task description — gathers required fields then delegates to the acli agent
disable-model-invocation: true
---

<required>
Add these steps to your task list:

1. Extract any fields provided inline; ask me only for what's still missing
2. Spawn the acli agent to execute
3. Return the ticket key and URL

</required>

## Fields fields

- **Project key** — default: DX
- **Type** — default: Task
- **Summary** — one line, imperative, specific
- **Description** — context, acceptance criteria, and any implementation notes. Can be left blank, if I asked you to create the ticket but did not provide enough context to populate this.

## What makes a good summary

<bad-example>
Fix the thing
Update dashboard
</bad-example>

<good-example>
Add rate limiting to the /auth endpoint
Surface error state when webhook delivery fails
</good-example>
