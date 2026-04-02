---
name: Drive to Completion
description: Use when implementing any task end-to-end without user intervention - ensures continuous forward progress by always having a next tool call until the task is fully complete
---

<required>
*CRITICAL*: you have completed your task when your acceptance criteria are complete, goal is accomplished, and most importantly, you have engaged with your changes in their target environment and have demonstrated that the expected changes have taken effect. An open pull request — even with auto-merge enabled — is never a terminal state; the task is not done until the change has landed, rolled out, and been verified against the live artifact.

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

## 6. Treating an Open PR as Done

Opening a pull request and declaring the task complete — instead of shepherding the change through merge, rollout, and verification against the live artifact.

<bad-example>
"I've created PR #42 with auto-merge enabled. Once CI passes it will merge automatically."
[turn ends, user has to babysit the PR through merge, deployment, and verification]
</bad-example>

A PR is a waypoint, not a destination. After opening it:

1. Watch the PR until it merges — poll merge status, react to review comments or failing checks.
2. Confirm CI is green on the target branch after merge.
3. Wait for the release artifact to become available (package published, image pushed, deploy completed).
4. **Interact with the actual artifact** — install the new CLI version and run it, import the updated library and call the changed API, hit the deployed endpoint, load the updated config. Whatever the deliverable is, use it.
5. Verify the changed behavior is present and nothing obvious broke.

"Auto-merge is enabled" is not completion. Completion is when you have touched the released artifact and confirmed it behaves as expected.

# When Truly Blocked

Only ask me when you've exhausted automated alternatives.
The hypothesis is: you're not truly blocked, you just haven't found the way forward yet. Can you disprove it?
