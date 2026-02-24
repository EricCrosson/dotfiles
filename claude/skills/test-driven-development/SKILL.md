---
name: Test-Driven Development (TDD)
description: Use when implementing any feature or bugfix, before writing implementation code - write the test first, watch it fail, write minimal code to pass; ensures tests actually verify behavior by requiring failure first
---

<required>
*CRITICAL* Add the following steps to your task list:

1. Write a failing test (RED phase)
2. Verify the test fails due to the behavior of the application, and NOT due to the test.
   <system-reminder>If you have more than one test that you need to write, you should write all of them before moving to the GREEN phase.</system-reminder>
3. Write the minimal amount of code necessary to make the test pass (GREEN phase)
4. Verify the test now passes due to the behavior of the application.
5. Refactor the code to clean it up.
6. Verify tests still pass.
   </required>

# Test Writing Guidelines

## What to Test: The Beyonce Rule

"If you liked it then you shoulda put a test on it." If a behavior is important
enough that you rely on it, it must have a test. No exceptions.

## How to Test: Behaviors, Not Methods

Structure tests around **behaviors** (given X, when Y, then Z) -- not around methods.
One method may exhibit multiple behaviors. One behavior may span multiple methods.
Name each test after the behavior it verifies.

- Test through the system's **public API**. Internal refactors must not break tests.
- Assert on **state and output** (return values, side effects), not on interactions
  (which methods were called, in what order). This is why mock-heavy tests are bad.
- Do not test types, data structure shapes, or implementation details.
- Only unit test utilities. Production code must be integration or end-to-end tested.

## RED - Write Failing Test

Write one minimal test showing what should happen.

<good-example>

```typescript
test("retries failed operations 3 times", async () => {
  let attempts = 0;
  const operation = () => {
    attempts++;
    if (attempts < 3) throw new Error("fail");
    return "success";
  };

  const result = await foobar.retryOperation(operation);

  expect(result).toBe("success");
  expect(attempts).toBe(3);
});
```

Test name describes a behavior. Asserts on output (result) and state (attempts),
not on interactions. Exercises the real module through its public API.

</good-example>

<bad-example>

```typescript
test("retry works", async () => {
  const mock = jest
    .fn()
    .mockRejectedValueOnce(new Error())
    .mockRejectedValueOnce(new Error())
    .mockResolvedValueOnce("success");
  await retryOperation(mock);
  expect(mock).toHaveBeenCalledTimes(3);
});
```

Name describes nothing. Asserts on interactions (call count), not on what the
system produces. Entirely mock-driven -- tests the test, not the code.
</bad-example>

## Verify RED - Watch It Fail

**MANDATORY. Never skip.** Run the test. Confirm:

- Test **fails** (not errors)
- Failure message matches expected missing behavior
- Fails because feature is missing, not because of typos or test bugs

**Test passes?** You are testing existing behavior. Fix the test.
**Test errors?** Fix the error. Re-run until it fails correctly.

## GREEN - Minimal Code

Write simplest code to pass the test.

<good-example>
```typescript
async function retryOperation<T>(fn: () => Promise<T>): Promise<T> {
  for (let i = 0; i < 3; i++) {
    try {
      return await fn();
    } catch (e) {
      if (i === 2) throw e;
    }
  }
  throw new Error('unreachable');
}
```
Just enough to pass
</good-example>

<bad-example>
```typescript
async function retryOperation<T>(
  fn: () => Promise<T>,
  options?: {
    maxRetries?: number;
    backoff?: 'linear' | 'exponential';
    onRetry?: (attempt: number) => void;
  }
): Promise<T> {
  // YAGNI
}
```
Over-engineered
</bad-example>

## Verify GREEN - Watch It Pass

**MANDATORY.** Run the test suite. Confirm:

- New test passes
- All other tests still pass
- No errors or warnings in output

**Test fails?** Fix the code, not the test.
**Other tests break?** Fix them now.

## REFACTOR - Clean Up

After green only:

- Remove duplication
- Improve names
- Extract helpers

Keep tests green. Do not add behavior.
