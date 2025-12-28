//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete.
//! Updated December 28, 2025: v0.1.1 released with full f-string and defer lowering.
//! v0.1.2 in progress – completing Semantic Analysis features.
pub mod checklist {
    /// Core Language Features
    pub const PARSER: &str = r#"
[x] Full f-string interpolation (nested exprs, formatting specs)
[x] Defer statement lowering + RAII void calls in MIR/codegen
    "#;

    /// Semantic Analysis
    pub const RESOLVER: &str = r#"
[x] Advanced trait resolution with associated types and specialization
[x] Parametric generics for Result/Map with proper monomorphization
[x] Full CTFE constant evaluation in type inference
[x] Expanded stable ABI checks for generic functions
    "#;

    /// MIR (Mid-level IR)
    pub const MIR: &str = r#"
[ ] Algebraic fusion of semiring chains (add/mul reassociation + vectorization hints)
[ ] Proper lowering of Defer (collect VoidCalls at function exit)
[ ] Generic Result handling + ? propagation lowering
[ ] Full DictLit/Subscript lowering with proper key/value types
    "#;

    /// Codegen (LLVM)
    pub const CODEGEN: &str = r#"
[ ] Real str_concat implementation (no external host dependency)
[ ] Execute actual MLGO-recommended passes via Inkwell PassManager
[ ] Accurate MIR statistics for AI optimization prompts
[ ] SIMD vectorization of SemiringFold (mul chains + loop vectorize)
    "#;

    /// Optimizations
    pub const OPTS: &str = r#"
[ ] Full compile-time evaluation beyond basic literals
[ ] Advanced semiring folding with mul chain fusion
[ ] Global specialization cache persistence across compilations
    "#;

    /// Concurrency/Std
    pub const ACTORS_STD: &str = r#"
[ ] Robust global channel map + proper registration/lookup
[ ] Real reqwest integration for std::http_get / std::tls_get (rustls)
[ ] Efficient single Tokio runtime (no per-host Runtime::new)
[ ] Proper actor entry registration and spawn mapping by func_id
[ ] Scheduler initialization error handling + panic safety
    "#;

    /// Bootstrap/Self-host
    pub const BOOTSTRAP: &str = r#"
[ ] True self-hosting bootstrap (Zeta compiler written in Zeta, compiles itself to native binary)
[ ] Expand selfhost.z to full parser/AST/resolver/codegen in Zeta syntax
    "#;

    /// Ergonomics (Ease of Use for Adoption)
    pub const ERGONOMICS: &str = r#"
[ ] Rich string methods (to_lowercase, replace, starts_with, etc.)
[ ] + operator sugar specialized for string concatenation
[ ] Implicit &str ↔ owned str conversions without lifetimes
[ ] Visual MIR dumps for debugging
[ ] Auto-step JIT debugger integration
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
