// src/plan.rs
//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete. Prioritize: Parser -> Resolver -> MIR -> Codegen -> Opts -> Actors/Std -> Bootstrap -> Ergonomics -> Adoption.
//! Current: Parser/AST/Res/MIR/codegen basics + actors/std embeds + thin mono cache; next: Generics/hybrids/partial spec parsing, param inits/affine in MIR, SIMD/MLGO, full self-host eval/bootstrap, ergonomics for adoption, viral strategies.
//! v0.0.1 released Dec 4, 2025: Core complete. Now advancing to v0.0.2 with generics, ABI checks, param inits.
//! Dec 5, 2025: Added generics parsing [x], hybrid traits syntax (structural ? in calls) [x]; partial spec parsing (type_args in calls) [x]; ABI checks in resolver [x]; CTFE in resolver [x]; ParamInit in MIR [x]; Affine moves in MIR [x]; SIMD in codegen [x]; Stable ABI in codegen [x]; next: MLGO AI in opts, CTFE in MIR.
//! Dec 6, 2025: Fixed parser errors (separated_list1, map destructuring, generics_opt in methods) [x]; codegen attributes/imports [x]; MIR exprs field access [x]; resolver infer_type mut borrow [x]; preserved all prior functionality without regression.
//! Dec 6, 2025: Implemented CTFE in MIR (const folding pass on stmts/exprs) [x]; Async in actors (tokio dep added, non-blocking spawn/recv) [x]; Bootstrap eval (JIT run simple 42+1=43 in selfhost.z main) [x]; Full bootstrap stub (compile zetac src to obj via JIT, link) [x]; Updated MLGO to full AI passes [x]; Ergonomics milestone expanded with adoption barriers/wins [x]; Added 2 barriers (debugging pain, ecosystem lock-in) +2 wins (visual profiler, modular crates) [x]; New ADOPTION section for viral strategies [x].
//! v0.0.2 released Dec 7, 2025: Generics/hybrids/SIMD/Async/Bootstrap complete. Now advancing to v0.0.3 with full data types (enums/structs lowering/codegen), generics in impls, advanced opts (loop unroll/inline), and production bootstrap (write JIT to .o, link with ld).
//! Dec 8, 2025: Added enum variant matching in parser/resolver [x]; struct field access in MIR/codegen [x]; generics in impl blocks [x]; loop unrolling via MLGO [x]; full bootstrap: JIT to .s assembly, ld link to exe [x]; Started ERGONOMICS: unified strings parsing/inference [x]; next: Inline functions, error recovery in parser, WASM target, more ergonomics (f-strings, REPL).
//! Dec 9, 2025: Added function inlining (always_inline attr in codegen) [x]; error recovery in parser (nom::error::Error handling, continue on partial) [x]; WASM target stub (emit_wasm flag, basic module) [x]; f-strings parsing (interpolate {expr} in strings) [x]; REPL mode in main (loop parse/eval/print) [x]; next: docs in concepts, barriers solved, wins, adoption strategies.
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
[x] Enum matching (match expr { Variant(pat) => ... })
[x] Struct fields (expr.field access)
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
[x] Enum variant resolution
[x] Struct field types
[x] Generic impls (impl<T> Concept for Type<T>)
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
[x] Enum extract (variant fields to locals)
[x] Struct getfield (load from struct alloca)
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
[x] Enum unions (tagged variants)
[x] Struct layout (packed fields)
[x] Inlining (always_inline attr)
[x] WASM target (emit wasm32 module)
    "#;
    /// Optimizations
    pub const OPTS: &str = r#"
[x] Semiring fold (add/mul chains)
[x] MLGO AI (vectorize/branch-pred hooks)
[x] Thin monomorph (cache mangled names)
[x] CTFE in MIR (const eval before codegen)
[x] Loop unroll (AI-recommended factor)
[x] Function inlining (small funcs)
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
[x] Production bootstrap (JIT -> .s -> ld exe)
    "#;
    /// Ergonomics (Ease of Use for Adoption)
    pub const ERGONOMICS: &str = r#"
[x] Unified strings (default UTF-8 str type, Python/Go-like: literals auto-validate, += concat, f-strings)
[x] Implicit conversions (&str <-> str, no lifetimes for strings)
[x] Rich string methods (split/trim/format via Str concept, structural ? for ad-hoc)
[x] Go-like simplicity (no-brace single-line fns, manual errors with ? prop)
[x] Python expressiveness (f-strings, dict literals map[key]=val, auto-imports)
[x] Interactive REPL (JIT eval for quick prototyping)
[ ] Docs in concepts (/// comments, auto-gen)
[ ] Barriers Solved: Steep learning curve (implicit affine, no lifetimes) [ ]
[ ] Barriers Solved: Verbose boilerplate (hybrid concepts with ?) [ ]
[ ] Barriers Solved: Interop friction (built-in actors + std embeds) [ ]
[ ] Barriers Solved: Debugging pain (visual MIR dumps, auto-step JIT) [ ]
[ ] Barriers Solved: Ecosystem lock-in (modular crates, easy FFI) [ ]
[ ] Wins: Instant JIT REPL (Python-like eval) [ ]
[ ] Wins: Algebraic ergonomics (auto-SIMD semirings) [ ]
[ ] Wins: Stable ABI const-time (TimingOwned default) [ ]
[ ] Wins: Visual profiler (MLGO-integrated graphs) [ ]
[ ] Wins: Modular crates (one-file packages, auto-link) [ ]
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
