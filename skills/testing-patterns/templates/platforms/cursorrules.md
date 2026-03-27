# Testing Patterns — Cursor Rules

# Write tests that catch bugs, not tests that pass.

## Testing Pyramid
- ~70% unit tests (fast, isolated, single function/class)
- ~20% integration tests (module boundaries, APIs, DB)
- ~10% E2E tests (full user workflows)
- If E2E tests outnumber unit tests, invert the pyramid

## Unit Tests
- Always use Arrange-Act-Assert (AAA) structure
- Use parameterized tests for same logic with multiple inputs
- Use test doubles correctly: stubs for data, mocks for interactions, spies for observation
- Only mock external boundaries (APIs, DB, filesystem) — never mock internal logic
- Use dependency injection to make code testable

## Integration Tests
- Test real module interactions across boundaries
- Use transaction rollback or testcontainers for database tests
- Use supertest/httptest for API endpoint testing
- Create test data via factories, not shared fixtures

## E2E Tests
- Use Page Object Model to abstract page interactions
- Always use `data-testid` attributes — never select by CSS class
- Use explicit waits — never use `sleep()`
- Design tests for parallel execution — no shared state
- Capture screenshots on failure for CI debugging

## Test Quality
- Tests must be deterministic: same input, same result, every time
- Tests must be isolated: no shared mutable state between tests
- Tests must be fast: unit <10ms, integration <1s, E2E <30s
- Tests must be readable: name describes scenario and expectation
- One logical assertion per test — failures should pinpoint the problem

## Anti-Patterns — Never Do These
- Never test implementation details — test behavior and outputs
- Never use `sleep()` in tests — use explicit waits
- Never share mutable state between tests — reset in beforeEach
- Never skip tests without a plan to fix or delete them
- Never write assert-free tests — every test must verify something
- Never mock everything — only mock at system boundaries
- Every bug fix must include a regression test

## Coverage
- Aim for 80%+ line coverage on business logic and core domain
- Aim for 90%+ branch coverage on security-critical paths
- Never chase 100% — diminishing returns on getters/setters
- Don't test generated code, third-party internals, or config files
