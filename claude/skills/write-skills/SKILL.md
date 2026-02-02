---
name: Write Skills
description: Use when creating or improving agent skills - ensures efficient context usage, clear activation conditions, and proper structure for different skill types
---

<required>
Add these steps to your task list:

1. Identify skill type (process-based or integration)
2. Draft description explaining WHEN to use (not what it does)
3. Structure content appropriately for skill type
4. Challenge each line - is it truly needed?
5. Verify under 150 lines (target <100)
   </required>

# Core Principles

## Treat Context as a Public Good

Context window is shared with system prompts, conversation history, and other skills. Keep skills under 150 lines,
ideally under 100. Challenge each piece of information: do I truly need this explanation?

## Descriptions Explain When, Not What

The description field clarifies activation conditions, not summaries.

<bad-example>
description: This skill helps write git commit messages following best practices
</bad-example>

<good-example>
description: Use when creating git commits - ensures messages follow Conventional Commits, explain motivation over implementation, and help future maintainers
</good-example>

# Skill Types

## Process-Based Skills

Multi-step workflows need structure:

- Use `<required>` blocks with explicit step-by-step instructions
- Integrate with TaskCreate/TaskUpdate for complex workflows
- Add `<system-reminder>` tags for critical decision points

<good-example>
```markdown
<required>
Add these steps to your task list:

1. Write failing test (RED phase)
2. Verify test fails correctly
   <system-reminder>If multiple tests needed, write all before GREEN phase.</system-reminder>
3. Write minimal code to pass (GREEN phase)
   </required>

```
</good-example>

## Integration Skills

CLI tools and scripts should do the work:

- Keep skill thin - delegate to external tools
- Rely on tool error messages to guide behavior
- Don't over-explain what the tool already handles

<bad-example>
Lengthy explanations of all CLI flags, error cases, and edge conditions
</bad-example>

<good-example>
Brief instruction to run CLI tool, let its help/errors guide the agent
</good-example>

# Formatting Conventions

- Use fake XML tags: `<good-example>`, `<bad-example>`, `<required>`, `<system-reminder>`
- Write first person: "Ask me to do ABC" (not "Ask the user")
```
