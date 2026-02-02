---
name: Reverse TDD PR Review
description: Use when reviewing PRs to verify tests actually validate implementation - removes implementation to confirm test failure, then restores it to confirm test success; proves TDD was followed correctly
---

<required>
Add these steps to your task list:

1. Identify implementation changes in PR (exclude test files)
2. Stash or temporarily remove implementation changes
3. Run tests and verify they FAIL
   <system-reminder>If tests pass without implementation, they don't test the new behavior</system-reminder>
4. Restore implementation changes
5. Run tests and verify they PASS
6. Report verification results
   </required>

# The Verification Process

Reverse TDD validates that:

- Tests fail without implementation (they test something real)
- Tests pass with implementation (implementation fixes what tests check)
- Tests aren't just passing by accident or testing wrong behavior

## Identify Implementation vs Test Changes

Split PR diff into:

- **Implementation**: Production code (src/, lib/, app/, etc.)
- **Tests**: Test files (_test_, _spec_, **tests**, etc.)

## Temporarily Remove Implementation

```bash
# Stash implementation files
git stash push -m "reverse-tdd-check" src/feature.ts

# Or checkout from base branch
git checkout origin/main -- src/feature.ts
```

## Verify Tests Fail (RED)

Run the test suite:

```bash
npm test              # or pytest, cargo test, etc.
npm test -- path/to/new/tests  # run only new tests
```

**Expected**: New tests FAIL

**Why this matters**:

- Proves tests actually check the new behavior
- Confirms tests would catch regressions
- Shows tests aren't tautologies or no-ops

**If tests pass**: Tests don't validate implementation. They might:

- Test existing behavior (not new functionality)
- Have mocked behavior that makes them pass regardless
- Not actually run the code path they claim to test

## Restore Implementation

Put implementation back:

```bash
# If you stashed
git stash pop

# If you checked out from base
git checkout - -- src/feature.ts src/utils.ts

# If manual changes
# Reapply from PR diff
```

## Verify Tests Pass (GREEN)

Run tests again.

**Expected**: Tests PASS

**Why this matters**:

- Proves implementation makes tests green
- Confirms implementation solves the right problem
- Shows TDD cycle was completed correctly

## Report Results

<good-example>
✅ **Reverse TDD Verification PASSED**
- Without implementation: 3 tests failed (testing real behavior)
- With implementation: All tests pass
- Conclusion: Tests validate implementation correctly
</good-example>

<bad-example>
❌ **Reverse TDD Verification FAILED**
- Without implementation: Tests still pass
- Issue: Tests don't verify new functionality
</bad-example>

# Common Failure Patterns

**Tests pass without implementation**:

- Tests mock what they should test
- Tests check existing behavior, not new behavior
- Wrong assertions
- **Action**: Rewrite tests

**Tests fail for wrong reasons**:

- Test setup depends on implementation
- Tests check implementation details
- **Action**: Improve test isolation

**Can't run tests without implementation**:

- Acceptable for integration tests
- Unit tests should be isolatable

# When to Use

Works best for:

- New features with new tests
- Bug fixes with regression tests
- Behavior changes with updated tests

Skip for:

- Integration tests requiring full system
- Type-only changes
- Refactors without behavior changes
- Documentation-only PRs
