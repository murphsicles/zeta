// src/plan.rs
//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete.
//! Updated Dec 18, 2025: Actor runtime stubs addressed [x]; improved generics and error prop handling.
pub mod checklist {
    /// Core Language Features
    pub const PARSER: &str = r#"
[x] Enums (enum Name { Variant, Variant(params) } - full variants with params)
    "#;
    /// Semantic Analysis
    pub const RESOLVER: &str = r#"
[x] Full trait resolution beyond Addable/StrOps
[x] Parametric handling for Result and Map generics
[ ] Complete type inference for advanced nodes
[ ] Expanded ABI checks for more cases
[ ] Implicit borrowing for types beyond Str/StrRef
    "#;
    /// MIR (Mid-level IR)
    pub const MIR: &str = r#"
[x] Generic Result handling in error propagation
[x] Type-based determination for affine moves in calls
[x] Generic support for DictLit/Subscript keys/values
    "#;
    /// Codegen (LLVM)
    pub const CODEGEN: &str = r#"
[x] Extract actual generics for monomorphization type args
[ ] Implement mul in SemiringOp vectorized/scalar folds
[ ] Define actual "str_concat" intrinsic or logic
[ ] Execute LLVM passes from MLGO recommendations
[ ] Real MIR analysis for stats (beyond print length)
    "#;
    /// Optimizations
    pub const OPTS: &str = r#"
[ ] Full const eval beyond simple add/mul in CTFE
[ ] Handle mul chains in semiring folding
    "#;
    /// Concurrency/Std
    pub const ACTORS_STD: &str = r#"
[x] Global channel map in host send/recv by chan_id
[x] Real reqwest integration for HTTP host functions
[x] TLS library (e.g., rustls) for handshake host
[x] Map spawn to actual actor entries by func_id
[ ] Error handling if scheduler not initialized
    "#;
    /// Bootstrap/Self-host
    pub const BOOTSTRAP: &str = r#"
[ ] True self-hosting bootstrap beyond parsing Rust sources
    "#;
    /// Ergonomics (Ease of Use for Adoption)
    pub const ERGONOMICS: &str = r#"
[ ] Go-like simplicity (no-brace single-line fns, manual errors with ? prop)
[ ] Python expressiveness (dict literals map[key]=val, auto-imports)
[ ] Interactive REPL (JIT eval for quick prototyping)
[ ] Docs in concepts (/// comments, auto-gen)
[ ] Barriers Solved: Debugging pain (visual MIR dumps, auto-step JIT)
[ ] Barriers Solved: Ecosystem lock-in (modular crates, easy FFI)
[ ] Wins: Visual profiler (MLGO-integrated graphs)
[ ] Wins: Modular crates (one-file packages, auto-link)
    "#;
    /// Adoption (Viral Strategies for 30-Year Dominance)
    pub const ADOPTION: &str = r#"
[ ] One-Click Playground (WASM REPL at zeta-lang.com/play, shareable links)
[ ] AI Code Gen Templates (VS Code ext for scaffolds: zeta new api)
[ ] Cross-Platform Kit (zeta run/deploy to WASM/native/Docker in 1 cmd)
[ ] Zeta Challenges (weekly bounties, leaderboards for puzzles)
[ ] Migration Tools (zeta migrate-rs → auto-transpile safe code)
[ ] Community Flywheels (GitHub stars >100k target, HN/Reddit auto-posts)
[ ] Ecosystem Hooks (one-file crates, JS/Python FFI zero-glue)
    "#;
}
