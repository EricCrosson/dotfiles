# Worktree Isolation

At the start of every interactive session, before doing any other work, call
the `EnterWorktree` tool to isolate your work in a git worktree.

After entering the worktree, run `direnv allow` to activate the nix dev shell.

Skip this step if:

- The user explicitly asks to work in the current directory
- You are already in a worktree
- You are not in a git repository
