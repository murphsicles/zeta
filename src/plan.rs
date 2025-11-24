// src/plan.rs
/// Zeta Compiler Milestone Plan
/// 
/// v0.1: Core Language Foundation (Semirings + Basics)
/// 
/// Completed:
/// - Parser (nom): Full AST parsing incl. funcs, calls, assigns, TimingOwned.
/// - AST: Nodes for FuncDef, Call, Lit/Var/Assign, ImplBlock, Method, TimingOwned.
/// - Resolver: Type inference, trait lookup (fast-path Addable), monomorphization cache.
/// - BorrowChecker: Affine moves, borrow states, speculative checks.
/// - MIR: Mid-level IR with SemiringOp (Add/Mul), gen from AST.
/// - Codegen: LLVM JIT, intrinsics/SIMD/TBAA, std embeds (free/datetime), actors.
/// - Lib: compile_and_run_zeta, re-exports.
/// - Main: Self-host example loader.
/// - Examples: add.zeta test.
/// - Fold Semiring Chains: Multi-op fusion in resolver.
/// 
/// Partial:
/// - Hybrid Traits: Nominal impls only; structural dispatch pending.
/// - Partial Specialization: Thin mono cache; full conditional impls todo.
/// - Defer RAII: Not yet; lifetime elision basic.
/// - Actors: Channel-based spawn; async/await sugar pending.
/// - Opts: Semiring fold; MLGO hooks stubbed.
/// - CTFE: Basic const eval in MIRExpr; full semiring chains todo.
/// 
/// To-Do (v0.1):
/// - Stable ABI: Add resolver checks for ABI compatibility.
/// - Thin Monomorph: Integrate with codegen for func mangling.
/// - EOP Expansions:
///   - Rings (Ch6): Add RingOp to MIR/SemiringOp (inverses/subtract).
///   - Linear Algebra (Ch7): Vector/matrix types, ops in AST/MIR/codegen (SIMD intrinsics).
///   - Polynomials (Ch8): Poly eval/arithmetic as semimodule impls.
///   - Ordered Algebraic Structures (Ch5): Ordered semigroups for sort algos in std.
///   - Transformations/Orbits (Ch2): Perm/group actions in traits.
/// 
/// v0.2: Advanced Features + Opts
/// - Full hybrid traits, partial spec, defer RAII.
/// - Async actors with channels.
/// - AI opts: MLGO vectorize/branch pred hooks.
/// - CTFE full eval.
/// 
/// Bootstrap: Self-host zeta.zeta compiler.
