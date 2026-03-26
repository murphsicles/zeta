# Self-Improving Memory - HOT Rules

## Zeta Compiler Development Patterns

### Tokenization & Parsing (2026-03-26)
- **Float literal storage**: When `f64` can't implement `Eq`/`Hash` traits, store as string in AST
- **Escape sequence parsing**: Manual character iteration works better than complex nom combinators for escape sequences
- **Backward compatibility**: Always test that existing integer literals still work when adding float support
- **Integration testing**: Create comprehensive test files that use all new features together

### Code Quality Patterns
- **Code review checklist**: Use security, performance, correctness, maintainability, testing dimensions
- **Severity levels**: Critical (blocks merge), Major (blocks merge), Minor (improvement), Nitpick (style)
- **Three-pass review**: 1) High-level structure, 2) Line-by-line detail, 3) Edge cases & hardening

### Proactive Development
- **Anticipate next steps**: After fixing tokenization, immediately create integration tests
- **Document design decisions**: Explain why floats are stored as strings (trait limitations)
- **Check for regressions**: Run existing tests to ensure no breaking changes

## User Preferences
- **Quality focus**: "Quality is non-negotiable" - apply rigorous code review standards
- **GitHub as reality**: Treat all work as if it will be committed to GitHub
- **Skill integration**: Use available skills (code-review, self-improving, proactivity, git-essentials)

## Workflow Patterns
- **Test-driven fixes**: Create failing tests first, then implement fixes
- **Comprehensive validation**: Test edge cases (`.0`, `0.5`, mixed escapes, Windows paths)
- **Clear reporting**: Show what was fixed and provide evidence of success