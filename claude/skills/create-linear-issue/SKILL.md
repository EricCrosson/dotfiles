---
name: create-linear-issue
description: Use when creating a Linear issue from a feature, bug, or task description — gathers required fields then calls the Linear MCP tool directly
---

<required>
Add these steps to your task list:

1. Extract any fields provided inline; ask me only for what's still missing
2. Call `mcp__claude_ai_Linear__save_issue` with all fields in a single call
3. Return the issue identifier and URL
   </required>

## Fields

- **Team** — default: CTX
- **Title** — one line, imperative, specific
- **Description** — context, acceptance criteria, and implementation notes in Markdown. Can be left blank if I asked you to create the issue but did not provide enough context to populate this.
- **Assignee** — default: Eric Crosson
- **Priority** — default: 3 (Normal). Scale: 0=None, 1=Urgent, 2=High, 3=Normal, 4=Low

<system-reminder>
Include assignee and priority in the creation call. Do not create the issue
first and then set these fields separately.
</system-reminder>
