// src/plan.rs
//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete.
//! Updated Dec 16, 2025: Ergonomics barriers addressed [x] — adoption phase planning.
pub mod checklist {
    /// Core Language Features
    pub const PARSER: &str = r#"
[x] Basic literals/vars/assigns (i64, ident = expr;)
[x] Binary ops (e.g., + as method call)
[x] Function defs (fn name(params: Type) -> Ret { stmts })
[x] Concepts (concept Name { method_sigs })
[x] Impls (impl Concept for Type { method_sigs })
[x] Enums (enum Name { Variant, Variant(params) } - basic variants)
[x] Structs (struct Name { field: Type, ... })
[x] Generics (fn foo<T>(x: T) -> T)
[x] Traits hybrid (nominal + structural dispatch)
[x] Partial specialization (mangle on safe types)
    "#;
    /// Semantic Analysis
    pub const RESOLVER: &str = r#"
[x] Type inference (env, builtins i64 Addable)
[x] Func sig registry (params/ret)
[x] Call resolution (func/trait lookup)
[x] Borrow checker (Owned/Borrowed/Consumed affine)
[x] Typecheck (infer + borrow per fn)
[x] Stable ABI checks (no UB, const-time TimingOwned)
[x] CTFE (const eval semirings)
    "#;
    /// MIR (Mid-level IR)
    pub const MIR: &str = r#"
[x] Lower AST to MIR (stmts/exprs/locals)
[x] Params (alloc locals, no init)
[x] Calls (args materialize, dest)
[x] Defer (collect VoidCall at end, RAII)
[x] Spawn (Call to actor_spawn_)
[x] SemiringFold opt (add chains)
[x] ParamInit (from caller args)
[x] Affine moves (consume after call)
    "#;
    /// Codegen (LLVM)
    pub const CODEGEN: &str = r#"
[x] Multi-fn (gen_mirs from Mirs, entry main calls user main)
[x] Locals/param inits (alloca/store from args)
[x] Calls/VoidCalls (declare extern if missing)
[x] Intrinsics (datetime/free/channel/spawn/http/tls)
[x] TBAA (TimingOwned const-time metadata)
[x] JIT (verify/pass_mgr/ee/map hosts)
[x] SIMD (vec ops via MLGO passes)
[x] Stable ABI (no UB, thin mono via specialization)
    "#;
    /// Optimizations
    pub const OPTS: &str = r#"
[x] Semiring fold (add/mul chains)
[x] MLGO AI (vectorize/branch-pred hooks)
[x] Thin monomorph (cache mangled names)
[x] CTFE in MIR (const eval before codegen)
    "#;
    /// Concurrency/Std
    pub const ACTORS_STD: &str = r#"
[x] Actor runtime (channels/scheduler/spawn host)
[x] Spawn lowering (intrinsic call)
[x] Channel send/recv (intrinsics)
[x] Std embeds (http_get/tls_handshake/datetime_now)
[x] Async (spawn blocks, channels)
    "#;
    /// Bootstrap/Self-host
    pub const BOOTSTRAP: &str = r#"
[x] Parse selfhost.z (concepts/impls/enums/structs/tokens)
[x] Typecheck selfhost (Parser/TypeChecker/Eval/Compile impls)
[x] MIR lower selfhost (stub build_ast/eval)
[x] Codegen selfhost (JIT compile_and_run)
[x] Eval 42+1=43 (main calls ZetaCompiler::compile)
[x] Full bootstrap (Zeta compiles Zeta)
    "#;
    /// Ergonomics (Ease of Use for Adoption)
    pub const ERGONOMICS: &str = r#"
[x] Unified strings (default UTF-8 str type, literals auto-validate, global constants)
[x] + operator sugar for concat (a + b)
[x] f-strings (f"Hello {name}!")
[x] Rich string methods (to_lowercase, replace, starts_with, etc.)
[x] Implicit &str borrows when needed
[x] Zero-cost str ↔ Vec<u8> interop
[x] Structural dispatch for string-like types
[x] Implicit conversions (&str <-> str, no lifetimes for strings)
[ ] Go-like simplicity (no-brace single-line fns, manual errors with ? prop)
[ ] Python expressiveness (dict literals map[key]=val, auto-imports)
[ ] Interactive REPL (JIT eval for quick prototyping)
[ ] Docs in concepts (/// comments, auto-gen)
[x] Barriers Solved: Steep learning curve (implicit affine, no lifetimes) 
[x] Barriers Solved: Verbose boilerplate (hybrid concepts with ?) 
[x] Barriers Solved: Interop friction (built-in actors + std embeds) 
[ ] Barriers Solved: Debugging pain (visual MIR dumps, auto-step JIT) 
[ ] Barriers Solved: Ecosystem lock-in (modular crates, easy FFI) 
[ ] Wins: Instant JIT REPL (Python-like eval) 
[x] Wins: Algebraic ergonomics (auto-SIMD semirings) 
[x] Wins: Stable ABI const-time (TimingOwned default) 
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
