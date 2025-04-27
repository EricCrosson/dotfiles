---
title: Write Useful Git Commit Message
description: Craft a meaningful git commit message based on Michael Lynch's guidelines
author: Eric Crosson
version: 1.0.0
date: 2025-04-27
---

# Write Useful Git Commit Message

This custom pattern helps you create effective git commit messages following Michael Lynch's guidelines from [refactoringenglish.com](https://refactoringenglish.com/chapters/commit-messages/).

## Usage

```bash
# Run on staged changes
git diff --staged | fabric --pattern write_useful_git_commit_message

# Run on file or directory
git diff -- path/to/file | fabric --pattern write_useful_git_commit_message
```

## Features

- Creates commit messages in present tense, imperative mood
- Focuses on explaining the WHY behind changes
- Follows best practices for commit message structure
- Provides context about the change's purpose
