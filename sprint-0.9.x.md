# Sprint: Zeta Self-Hosting — v0.9.x Series

**Start:** 2026-04-30 05:03 BST
**Check-in:** ~06:30 BST (Roy home from work)

## Phases (in order, each → git tag + release)

### Phase 1: Fix 3 Critical Bugs → v0.9.6
- [ ] If-else as tail expression returns 0 instead of evaluated value
- [ ] Match as sole body expression → "No main function"
- [ ] `println` FFI panics with "CRITICAL: Missing function 'println'"

### Phase 2: Plug the panic hole → v0.9.7
- [ ] Fix `monomorphize.rs:96` — real todo!() that panics at runtime

### Phase 3: Complete AST→MIR coverage → v0.9.8
- [ ] `AstNode::Block` handling (may just be fold into existing patterns)
- [ ] `AstNode::FloatLit` — float literal support
- [ ] `AstNode::MacroCall` — macro invocation lowering
- [ ] `AstNode::MacroDef` — macro definition lowering

### Phase 4: Resolver & Type System → v0.9.9
- [ ] New resolver TODOs (Future trait, conversion validation, struct field lookup)
- [ ] Type checker gaps (element type inference, identity inference)
- [ ] Module resolver — real stdlib resolution instead of stubs

### Phase 5: Get zeta_src/ compiling → v0.9.10
- [ ] Start with simplest files, compile & fix iteratively
- [ ] Each file that fails reveals a fix
- [ ] Target: zeta_src/main.z compiles

## Release Process
Each successful phase → `cargo fmt && cargo clippy && cargo build --release` 
→ `git add -A && git commit -m "v0.9.x: ..."` 
→ `git tag -f v0.9.x` 
→ `git push --force origin dev --tags`
