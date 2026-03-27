# Testing Patterns — Copilot Instructions

## Instructions

Write tests that catch bugs, not tests that pass. Follow the testing pyramid, use proper test structure, and mock only at system boundaries.

## Testing Pyramid

Maintain the ratio: ~70% unit, ~20% integration, ~10% E2E. If E2E tests outnumber unit tests, invert the pyramid.

## Unit Test Structure — Arrange-Act-Assert

```typescript
test('calculates order total with tax', () => {
  // Arrange
  const items = [{ price: 10, qty: 2 }, { price: 5, qty: 1 }];
  const taxRate = 0.08;

  // Act
  const total = calculateTotal(items, taxRate);

  // Assert
  expect(total).toBe(27.0);
});
```

Use parameterized tests for multiple inputs:

```typescript
test.each([
  ['hello', 'HELLO'],
  ['world', 'WORLD'],
  ['', ''],
])('toUpperCase(%s) → %s', (input, expected) => {
  expect(input.toUpperCase()).toBe(expected);
});
```

## Mocking — Only at Boundaries

```typescript
// ❌ BAD — mocking internal implementation
jest.mock('./utils/formatDate');

// ✅ GOOD — mocking external boundary
jest.mock('./services/paymentGateway');
```

Mock HTTP APIs, databases, filesystems, and time. Never mock pure functions, domain logic, or data transformations.

## E2E — Page Object Model

```typescript
class LoginPage {
  constructor(private page: Page) {}
  async login(email: string, password: string) {
    await this.page.fill('[data-testid="email"]', email);
    await this.page.fill('[data-testid="password"]', password);
    await this.page.click('[data-testid="submit"]');
  }
}
```

Use `data-testid` for selectors, explicit waits instead of `sleep()`, and design for parallel execution.

## Anti-Patterns to Avoid

- Don't test implementation — test behavior and outputs
- Don't use `sleep()` — use explicit waits or polling
- Don't share mutable state — reset in `beforeEach`
- Don't skip tests indefinitely — fix or delete them
- Don't mock everything — only mock external boundaries
- Every bug fix must include a regression test
