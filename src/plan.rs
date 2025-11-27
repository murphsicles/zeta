// src/plan.rs
//! Zeta Development Plan: Checklist for core compiler and EOP integration.
//! Sections: Completed (fully working), Partially Completed (stubbed/partial), To Do (next milestones).
//! EOP Focus: Algebraic structures (semirings, semimodules, semigroups) for codegen/optimizations.
//! Prioritize: Speed (thin mono, CTFE eval), Safety (affine borrowck), Efficiency (SIMD/async).

pub enum Status {
    Completed,
    Partial,
    Todo,
}

pub struct PlanItem {
    pub name: &'static str,
    pub status: Status,
    pub description: &'static str,
    pub eop_link: Option<&'static str>, // EOP chapter/section reference
}

pub const PLAN: &[PlanItem] = &[
    // Completed
    PlanItem {
        name: "Parser (Nom-based)",
        status: Status::Completed,
        description: "Full Zeta syntax: fn defs, calls, lits, vars, assigns, TimingOwned. WS handling, expr chaining (+ as call).",
        eop_link: None,
    },
    PlanItem {
        name: "AST Nodes",
        status: Status::Completed,
        description: "Program, ConceptDef, ImplBlock, Method, FuncDef (generics/params/ret/body/attrs/ret_expr), Call (receiver/method/args/type_args), Lit, Var, Assign, TimingOwned.",
        eop_link: None,
    },
    PlanItem {
        name: "Borrow Checker (Affine + Speculative)",
        status: Status::Completed,
        description: "Owned/Borrowed/MutBorrowed/Consumed states. Affine moves, speculative states (Safe/Speculative/Poisoned). Check on Var/Assign/Call/TimingOwned.",
        eop_link: None,
    },
    PlanItem {
        name: "Actor Runtime (Channels + Scheduler)",
        status: Status::Completed,
        description: "Thread-safe Channel (Mutex<VecDeque<Message>> + Condvar). Work-stealing scheduler on thread pool (num_cpus). Spawn/FnOnce.",
        eop_link: None,
    },
    PlanItem {
        name: "Specialization Cache (Thin Monomorphization)",
        status: Status::Completed,
        description: "MonoKey (func_name + type_args), MonoValue (mangled_name + cache_safe). RwLock<HashMap>, is_cache_safe (primitives/pointers). Lookup/record.",
        eop_link: None,
    },
    PlanItem {
        name: "MIR (Mid IR)",
        status: Status::Completed,
        description: "Mir struct (stmts/locals/exprs). MirStmt (Assign/Call/Return/SemiringFold). MirExpr (Var/Lit/ConstEval). MirGen (gen_stmt/gen_expr/materialize/alloc_local).",
        eop_link: Some("Ch 1: Equality Comparable"),
    },
    PlanItem {
        name: "Semiring Ops in MIR (EOP Intro)",
        status: Status::Completed,
        description: "SemiringOp (Add/Mul). Fold chains (add/mul). CTFE eval stub.",
        eop_link: Some("Ch 10: Semirings"),
    },
    PlanItem {
        name: "LLVM Codegen (JIT/Intrinsics/SIMD/TBAA)",
        status: Status::Completed,
        description: "LLVMCodegen (module/builder/i64_type). Gen MIR (assign/call/return/semiring). Host fns (datetime_now/free). Finalize/JIT with mappings. Stable ABI/TimingOwned guarantees.",
        eop_link: None,
    },
    PlanItem {
        name: "Resolver (Type Inference/Trait Resolution)",
        status: Status::Completed,
        description: "Type enum (I64/F32/Bool/Named/Unknown). Infer (lit/var/assign/call). Register impls. Builtin Addable<i64>.",
        eop_link: Some("Ch 2: Ordered"),
    },
    PlanItem {
        name: "Typecheck + Borrow Integration",
        status: Status::Completed,
        description: "Typecheck on FuncDef body. Infer + borrow check per stmt.",
        eop_link: None,
    },
    PlanItem {
        name: "Lower to MIR + Opts",
        status: Status::Completed,
        description: "Lower FuncDef to Mir. Fold semiring chains.",
        eop_link: Some("Ch 10: Semirings"),
    },
    PlanItem {
        name: "Lib Exports + compile_and_run_zeta",
        status: Status::Completed,
        description: "Re-exports (parse_zeta/Resolver/init_runtime/spawn). Compile/run stub (no-codegen error).",
        eop_link: None,
    },
    PlanItem {
        name: "Main Entry (Self-Host Example)",
        status: Status::Completed,
        description: "Load/parse/register/typecheck. Lower main to MIR/codegen/JIT. Map free. Exec/print.",
        eop_link: None,
    },
    // Partially Completed
    PlanItem {
        name: "Hybrid Traits (Nominal + Structural)",
        status: Status::Partial,
        description: "Direct impls (nominal). Stub structural matching in resolver (type_args hash).",
        eop_link: Some("Ch 3: Iterator Concepts"),
    },
    PlanItem {
        name: "Partial Specialization",
        status: Status::Partial,
        description: "Cache key with type_args. Mangling. is_cache_safe stub (primitives only). Full: Match patterns in lookup.",
        eop_link: Some("Ch 8: Generic Algorithms"),
    },
    PlanItem {
        name: "Defer RAII",
        status: Status::Partial,
        description: "Stub in AST (defer expr). Lower to MIR (drop on exit). Integrate borrowck.",
        eop_link: None,
    },
    PlanItem {
        name: "TimingOwned Constant-Time",
        status: Status::Partial,
        description: "AST node. Codegen guarantees (TBAA/no-branch). Borrowck affine.",
        eop_link: None,
    },
    PlanItem {
        name: "Std Embeds (http/TLS/datetime/free)",
        status: Status::Partial,
        description: "Host fns (datetime_now/free). Stub http/TLS (reqwest/tokio-tls embeds).",
        eop_link: None,
    },
    PlanItem {
        name: "MLGO AI Hooks (Vectorize/Branch Pred)",
        status: Status::Partial,
        description: "Stub in codegen (pass manager). Full: MLGO integration for opts.",
        eop_link: Some("Ch 9: Sorting"),
    },
    PlanItem {
        name: "CTFE Semiring Eval",
        status: Status::Partial,
        description: "Stub in MIR (ConstEval). Full: Eval chains at compile-time.",
        eop_link: Some("Ch 10: Semirings"),
    },
    // To Do
    PlanItem {
        name: "Semimodules (EOP Full)",
        status: Status::Todo,
        description: "Extend semirings to modules (vector/matrix ops). MIR support for semimodule fold.",
        eop_link: Some("Ch 11: Semimodules"),
    },
    PlanItem {
        name: "Semigroups/Monoids/Groups/Rings (EOP Prereqs)",
        status: Status::Todo,
        description: "Trait defs (idempotent/additive/multiplicative). Resolver auto-impl for primitives. Opts for assoc/commute.",
        eop_link: Some("Ch 4-7: Semigroups to Rings"),
    },
    PlanItem {
        name: "Iterator Concepts (EOP)",
        status: Status::Todo,
        description: "Iterator traits (forward/random_access). Generic algos (fold/map).",
        eop_link: Some("Ch 3: Iterator"),
    },
    PlanItem {
        name: "Sorting/Searching (EOP Algos)",
        status: Status::Todo,
        description: "Generic sort/search with EOP predicates. SIMD intrinsics.",
        eop_link: Some("Ch 9: Sorting"),
    },
    PlanItem {
        name: "Full Async (Actors + Futures)",
        status: Status::Todo,
        description: "Async fn/await. Channel integration with tokio-like scheduler.",
        eop_link: None,
    },
    PlanItem {
        name: "SIMD Everywhere",
        status: Status::Todo,
        description: "Auto-vectorize in codegen (MLGO). Intrinsic support in MIR.",
        eop_link: Some("Ch 12: SIMD"),
    },
    PlanItem {
        name: "Self-Hosting",
        status: Status::Todo,
        description: "Compile zeta.zeta to LLVM. Bootstrap loop.",
        eop_link: None,
    },
    PlanItem {
        name: "Tests/Benches",
        status: Status::Todo,
        description: "Unit (parse/typecheck/codegen). Criterion benches (add chains, mono).",
        eop_link: None,
    },
    PlanItem {
        name: "Docs + Crates.io Publish",
        status: Status::Todo,
        description: "README (setup/EOP links). Publish zetac (core + codegen feat).",
        eop_link: None,
    },
];
