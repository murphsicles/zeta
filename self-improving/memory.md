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

### EOP Research & Implementation
- **Regular types in Rust**: Eq implies PartialEq, Drop not needed in trait bounds
- **Transformation error handling**: Always use Result types for fallible operations
- **Mathematical testing**: Combine property-based tests with formal proofs
- **Three-phase implementation**: Research → Review → Refine with skill application
- **Git integration**: Commit meaningful changes with skill application documentation

## User Preferences
- **Quality focus**: "Quality is non-negotiable" - apply rigorous code review standards
- **GitHub as reality**: Treat all work as if it will be committed to GitHub
- **Skill integration**: Use available skills (code-review, self-improving, proactivity, git-essentials)
- **Father Zak's way**: "This is the way." - Apply skills systematically for quality improvement

## Workflow Patterns
- **Test-driven fixes**: Create failing tests first, then implement fixes
- **Comprehensive validation**: Test edge cases (`.0`, `0.5`, mixed escapes, Windows paths)
- **Clear reporting**: Show what was fixed and provide evidence of success
- **Skill-based quality**: Apply code-review, self-improving, proactivity, git-essentials systematically
- **Three-pass review**: High-level → line-by-line → edge cases for all significant work
- **Mathematical foundation**: Build on EOP principles with proofs and property testing