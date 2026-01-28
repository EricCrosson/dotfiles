---
name: Write Commit Message
description: Use when creating git commits - ensures messages follow Conventional Commits, explain motivation over implementation, and help future maintainers
---

<required>
Add these steps to your task list:

1. Draft commit message with proper structure
2. Verify message follows format and length constraints
3. Review for clarity and motivation

 </required>

# Commit Message Format

```
<type>[optional scope]: <description under 72 chars, prefer <50>

[optional body with 72-char line limit]

[optional footer(s)]
```

## Types

- `feat:` - New feature (MINOR version bump)
- `fix:` - Bug fix (PATCH version bump)
- `perf:` - Performance improvement (PATCH version bump)
- `chore:` - Maintenance (deps, config, no code change)
- `docs:` - Documentation only
- `refactor:` - Code restructure without behavior change
- `test:` - Add or fix tests
- `build:`, `ci:`, `style:` - As appropriate

## Breaking Changes

Add `!` after type or `BREAKING CHANGE:` footer:

```
feat!: remove legacy API endpoints

BREAKING CHANGE: Clients must migrate to v2 API by 2026-03-01.
See migration guide at docs/v2-migration.md
```

# What To Include

## Title

Describe the **effect** (what changed), not implementation (how).

<bad-example>
```
fix: add mutex to guard database handle
```
Implementation detail, doesn't explain impact
</bad-example>

<good-example>
```
fix: prevent database corruption during simultaneous sign-ups
```
Clear effect on users
</good-example>

## Body

### Motivation (Most Important)

Explain **why** this change exists.

<bad-example>
```
style: change background from blue to pink

Updates CSS for pink background instead of blue.

```
States the obvious
</bad-example>

<good-example>
```

style: change background from blue to pink

Blue background made links illegible. Pink improves contrast
while matching app personality.

```
Explains reasoning and constraints
</good-example>

### Context

- What problem does this solve?
- What alternatives did you consider?
- What did you learn?
- External references that guided decisions

### Cross-References

```

Closes Ticket: DX-1234

```
Auto-closes Jira ticket when merged.

```

Ticket: DX-1234

```
References without closing.

Summarize relevant bug details - don't force readers to dig through tickets.

# What To Exclude

- File lists (visible in diff)
- API names you called (visible in code)
- Obvious implementation details
- Critical code maintenance info (belongs in code comments with automated checks)
- Adjectives, adverbs, flowery language
- Time-limited preview URLs

# Length Constraints

- Title: 72 chars max, prefer <50
- Body: 72 chars per line
- Use headings (`**Background**`, `**Motivation**`) to structure long messages

# Style

- Present tense ("fix" not "fixed")
- Active voice ("prevents corruption" not "corruption is prevented")
- Inverted pyramid - most important info first
- Succinct - remove unnecessary words
- Liberally use footnotes to permalinks from GitHub and external sources (APIs, documentation, etc)
```
