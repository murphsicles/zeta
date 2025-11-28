// src/plan.rs
//! Zeta Compiler Development Plan.
//! Checklist for milestones. Mark as [x] when complete. Prioritize: Parser -> Resolver -> MIR -> Codegen -> Opts -> Actors/Std -> Bootstrap.
//! Current: Parser/AST/Res/MIR/codegen basics; next: Self-host parse selfhost.z, typecheck concepts/impls, eval simple.

pub mod checklist {
    /// Core Language Features
    pub const PARSER: &str = r#"
[x] Basic literals/vars/assigns (i64, ident = expr;)
[x] Binary ops (e.g., + as method call)
[x] Function defs (fn name(params: Type) -> Ret { stmts })
[x] Concepts (concept Name { method_sigs })
[x] Impls (impl Concept for Type { method_sigs })
[ ] Enums (enum Name { Variant(Type), ... })
[ ] Structs (struct Name { field: Type, ... })
[ ] Generics (fn foo<T>(x: T) -> T)
[ ] Traits hybrid (nominal + structural dispatch)
[ ] Partial specialization (mangle on safe types)
    "#;

    /// Semantic Analysis
    pub const RESOLVER: &str = r#"
[x] Type inference (env, builtins i64 Addable)
[x] Func sig registry (params/ret)
[x] Call resolution (func/trait lookup)
[x] Borrow checker (Owned/Borrowed/Consumed affine)
[x] Typecheck (infer + borrow per fn)
[ ] Stable ABI checks (no UB, const-time TimingOwned)
[ ] CTFE (const eval semirings)
    "#;

    /// MIR (Mid-level IR)
    pub const MIR: &str = r#"
[x] Lower AST to MIR (stmts/exprs/locals)
[x] Params (alloc locals, no init)
[x] Calls (args materialize, dest)
[x] Defer (collect VoidCall at end, RAII)
[x] Spawn (Call to actor_spawn_)
[x] SemiringFold opt (add chains)
[ ] ParamInit (from caller args)
[ ] Affine moves (consume after call)
    "#;

    /// Codegen (LLVM)
    pub const CODEGEN: &str = r#"
[x] Multi-fn (gen_mirs from Mirs, entry main calls user main)
[x] Locals/param inits (alloca/store from args)
[x] Calls/VoidCalls (declare extern if missing)
[x] Intrinsics (datetime/free/channel/spawn/http/tls)
[x] TBAA (TimingOwned const-time metadata)
[x] JIT (verify/pass_mgr/ee/map hosts)
[ ] SIMD (vec ops via MLGO passes)
[ ] Stable ABI (no UB, thin mono via specialization)
    "#;

    /// Optimizations
    pub const OPTS: &str = r#"
[x] Semiring fold (add/mul chains)
[ ] MLGO AI (vectorize/branch-pred hooks)
[ ] Thin monomorph (cache mangled names)
[ ] CTFE in MIR (const eval before codegen)
    "#;

    /// Concurrency/Std
    pub const ACTORS_STD: &str = r#"
[x] Actor runtime (channels/scheduler/spawn host)
[x] Spawn lowering (intrinsic call)
[x] Channel send/recv (intrinsics)
[ ] Async (spawn blocks, channels)
[ ] Std embeds (http_get/tls_handshake/datetime_now)
    "#;

    /// Bootstrap/Self-host
    pub const BOOTSTRAP: &str = r#"
[ ] Parse selfhost.z (concepts/impls/enums/structs/tokens)
[ ] Typecheck selfhost (Parser/TypeChecker/Eval/Compile impls)
[ ] MIR lower selfhost (stub build_ast/eval)
[ ] Codegen selfhost (JIT compile_and_run)
[ ] Eval 42+1=43 (main calls ZetaCompiler::compile)
[ ] Full bootstrap (Zeta compiles Zeta)
    "#;
}
