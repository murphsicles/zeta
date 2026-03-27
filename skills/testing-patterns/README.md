# Testing Patterns

Unit, integration, and E2E testing patterns with framework-specific guidance. Write tests that catch bugs, not tests that pass — confidence through coverage, speed through isolation.

## What's Inside

- Testing pyramid with ratio, speed, and confidence breakdown
- Unit testing patterns (Arrange-Act-Assert, Given-When-Then, parameterized, snapshot, property-based)
- Test doubles guide (stubs, mocks, spies, fakes) with when-to-use guidance
- Parameterized tests across TypeScript, Python, and Go
- Integration testing patterns (transaction rollback, fixtures, factory functions, testcontainers)
- API testing with Supertest
- Mocking best practices (mock boundaries not implementations, dependency injection)
- Framework quick reference (Jest, Vitest, Playwright, Cypress, pytest, Go testing, Rust, JUnit, RSpec, PHPUnit, xUnit)
- Test quality checklist (deterministic, isolated, fast, readable, maintainable, focused)
- Coverage strategy with targets by project type
- Test organization conventions
- Anti-patterns catalog

## When to Use

- Writing tests for new or existing code
- Adding test coverage to a project
- Defining a testing strategy
- Fixing flaky tests or improving test quality
- Choosing the right test doubles and patterns

## Installation

```bash
npx add https://github.com/wpank/ai/tree/main/skills/testing/testing-patterns
```

### Manual Installation

#### Cursor (per-project)

From your project root:

```bash
mkdir -p .cursor/skills
cp -r ~/.ai-skills/skills/testing/testing-patterns .cursor/skills/testing-patterns
```

#### Cursor (global)

```bash
mkdir -p ~/.cursor/skills
cp -r ~/.ai-skills/skills/testing/testing-patterns ~/.cursor/skills/testing-patterns
```

#### Claude Code (per-project)

From your project root:

```bash
mkdir -p .claude/skills
cp -r ~/.ai-skills/skills/testing/testing-patterns .claude/skills/testing-patterns
```

#### Claude Code (global)

```bash
mkdir -p ~/.claude/skills
cp -r ~/.ai-skills/skills/testing/testing-patterns ~/.claude/skills/testing-patterns
```

## Related Skills

- [e2e-testing-patterns](../e2e-testing-patterns/) — Dedicated E2E patterns with Playwright and Cypress
- [testing-workflow](../testing-workflow/) — Meta-skill that orchestrates testing across a project
- [quality-gates](../quality-gates/) — Coverage thresholds and CI/CD enforcement
- [clean-code](../clean-code/) — Code quality standards that tests verify

---

Part of the [Testing](..) skill category.
