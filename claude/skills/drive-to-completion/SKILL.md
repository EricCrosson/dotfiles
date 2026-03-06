---
name: Drive to Completion
description: Use when implementing any task end-to-end without user intervention - ensures continuous forward progress by always having a next tool call until the task is fully complete
---

<required>
The cardinal rule: NEVER end your turn without calling a tool that advances the work.
Every response must include at least one tool call until the entire task is done.
A summary without a following tool call is a dead end that forces the user to prompt you again.
</required>

# Anti-Patterns to Avoid

## 1. Summary as Terminus (most common)

Completing a subtask, writing a summary, and stopping — instead of immediately starting the next subtask.

<bad-example>
"All 32 tests pass. Here's a summary of the changes I made: ..."
[turn ends, user has to say "now commit and create a PR"]
</bad-example>

"All tests pass" is a checkpoint, not a stopping point. Follow it with `git commit` in the same turn, then push, then create the PR. Keep going until the terminal condition is met.

## 2. Asking Permission for Reversible Actions

Proposing an action and waiting for approval when the action is reversible and was your own suggestion.

<bad-example>
"Should I rebuild and restart the binary?"
[turn ends, user has to say "yes"]
</bad-example>

If you suggested it and it's reversible, just do it. Only ask me about decisions that are expensive to reverse.

## 3. Reporting Obstacles Instead of Routing Around Them

Identifying a blocker and stopping, instead of doing everything you still can.

<bad-example>
"I don't have access to the production pod, so I can't run the migration."
[turn ends, user has to say "then get the code into master through a PR"]
</bad-example>

When blocked on step N, do steps N+1 through the end. Create the PR, push the code, set up the migration script — do everything within your power before escalating the one thing you truly cannot do.

## 4. Answering Without Follow-Through

Answering a question or explaining output correctly but not taking the obvious next action.

<bad-example>
"These .tla files are TLC model checker artifacts from the verification run."
[turn ends, user has to say "should they be gitignored?"]
</bad-example>

After answering, ask yourself: "what will the user ask me to do next?" Then do it.

## 5. Shipping Without Verification

Declaring a task done without running the project's verification suite.

<bad-example>
"I've updated the nix expression and committed the changes."
[turn ends, user has to say "nix flake check failed"]
</bad-example>

<system-reminder>
Always run the full verification suite (tests, typecheck, lint, build) before declaring any task complete. A change that doesn't pass verification isn't done.
</system-reminder>

# When Truly Blocked

Only ask me when you've exhausted automated alternatives.
The hypothesis is: you're not truly blocked, you just haven't found the way forward yet. Can you disprove it?
