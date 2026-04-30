# Sprint: Zeta Self-Hosting — v0.9.x Series

**Started:** 2026-04-30 05:03 BST
**Sprint 2:** 08:10 BST — Resolve all zeta_src/ compilation blockers

## Phase 6: Compile zeta_src/ files iteratively

- [ ] Compile zeta_src/main.z, fix each blocker as it appears
- [ ] Each blocker resolved → commit as v0.9.x+1
- [ ] Target: zeta_src/main.z compiles and produces a working Zeta compiler

## Release Process
Each fix → `cargo fmt && cargo clippy && cargo build --release`
→ `cargo test --lib` → `git add -A && git commit -m "v0.9.x: ..."`
→ `git tag -f v0.9.x` → `git push --force origin dev --tags`
→ Create GitHub release with same body
