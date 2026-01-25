// plan.rs
//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete.
//! Updated January 25, 2026: Added post-self-hosting plans for v0.3.1, v0.3.2, and v0.3.3 to step away from Rust reliance.
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
[x] Algebraic fusion of semiring chains (add/mul reassociation + vectorization hints)
[x] Proper lowering of Defer (collect VoidCalls at function exit)
[x] Generic Result handling + ? propagation lowering
[x] Full DictLit/Subscript lowering with proper key/value types
"#;

    /// Codegen (LLVM)
    pub const CODEGEN: &str = r#"
[x] Real str_concat implementation (no external host dependency)
[x] Execute actual MLGO-recommended passes via Inkwell PassManager
[x] Accurate MIR statistics for AI optimization prompts
[x] SIMD vectorization of SemiringFold (mul chains + loop vectorize)
"#;

    /// Optimizations
    pub const OPTS: &str = r#"
[x] Full compile-time evaluation beyond basic literals
[x] Advanced semiring folding with mul chain fusion
[x] Global specialization cache persistence across compilations
"#;

    /// Concurrency/Std
    pub const ACTORS_STD: &str = r#"
[x] Robust global channel map + proper registration/lookup
[x] Real reqwest integration for std::http_get / std::tls_get (rustls)
[x] Efficient single Tokio runtime (no per-host Runtime::new)
[x] Proper actor entry registration and spawn mapping by func_id
[x] Scheduler initialization error handling + panic safety
"#;

    /// Bootstrap/Self-host
    pub const BOOTSTRAP: &str = r#"
[x] True self-hosting bootstrap (Zeta compiler written in Zeta, compiles itself to native binary)
[x] Expand selfhost.z to full parser/AST/resolver/codegen in Zeta syntax
"#;

    /// Ergonomics (Ease of Use for Adoption)
    pub const ERGONOMICS: &str = r#"
[x] Rich string methods (to_lowercase, replace, starts_with, etc.)
[x] + operator sugar specialized for string concatenation
[x] Implicit &str ↔ owned str conversions without lifetimes
[x] Visual MIR dumps for debugging
[x] Auto-step JIT debugger integration
"#;

    /// Adoption (Viral Strategies for 30-Year Dominance)
    pub const ADOPTION: &str = r#"
[x] One-Click Playground (WASM REPL at zeta-lang.com/play, shareable links)
[ ] AI Code Gen Templates (VS Code ext for scaffolds: zeta new api)
[ ] Cross-Platform Kit (zeta run/deploy to WASM/native/Docker in 1 cmd)
[ ] Zeta Challenges (weekly bounties, leaderboards for puzzles)
[ ] Migration Tools (zeta migrate-rs → auto-transpile safe code)
[ ] Community Flywheels (GitHub stars >100k target, HN/Reddit auto-posts)
[ ] Ecosystem Hooks (one-file crates, JS/Python FFI zero-glue)
"#;

    /// Post-Self-Hosting Plans (Step Away from Rust Reliance)
    pub const POST_SELF_HOST: &str = r#"
[ ] v0.3.1: Port runtime/ functions (M:N actors, builtins) from Rust to Zeta in zeta_src/runtime/. Small LOC (~200); transpose efficiently for green-threads, HTTP/TLS/datetime. Implement as single-file .z if possible for minimalism.
[ ] v0.3.1: Update bootstrap Rust to 2024 Edition, nom 8.0, Inkwell 7.0/LLVM 19.1. Test self-compilation speed remains ~14ms.
[ ] v0.3.1: Transpose simple utility functions from main.rs/lib.rs to main.z.
[ ] v0.3.1: Benchmark ports; ensure no regressions.

[ ] v0.3.2: Port frontend/ (parser/lexer) from Rust (nom) to Zeta in zeta_src/frontend/. Implement recursive descent parser in Zeta for first principles (avoid combinator deps); transpose nom logic efficiently.
[ ] v0.3.2: Port tests.rs functions to Zeta-based testing framework.
[ ] v0.3.2: Validate full self-parsing (Zeta parses its own .z files without Rust frontend).

[ ] v0.3.3: Port middle/ (MIR, optimizations) to zeta_src/middle/. Transpose semiring CTFE, fusion, monomorphization.
[ ] v0.3.3: Port backend/ (codegen, LLVM interface) to zeta_src/backend/, integrating with ported runtime.
[ ] v0.3.3: Remove Rust bootstrap; compile purely with Zeta. Deprecate /src/ directory.
[ ] v0.3.3: Full benchmarks; AI-opt all components.
"#;
}
