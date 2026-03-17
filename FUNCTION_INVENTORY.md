# Zeta v0.3.4 Function Inventory

**Total Functions:** 41
**Generated:** March 16, 2026

## Module: `lib`

**Count:** 1

| Function | Visibility | Parameters | Return Type | File |
|----------|------------|------------|-------------|------|
| `compile_and_run_zeta` | public | code: &str | `Result<i64, String>` | `src\lib.rs:31` |

## Module: `main`

**Count:** 3

| Function | Visibility | Parameters | Return Type | File |
|----------|------------|------------|-------------|------|
| `collect_func_asts` | private | asts: &[AstNode] | `Vec<AstNode>` | `src\main.rs:30` |
| `main` | private |  | `Result<(), Box<dyn std::error::Error>>` | `src\main.rs:43` |
| `repl` | private | _dump_mir: bool | `Result<(), Box<dyn std::error::Error>>` | `src\main.rs:265` |

## Module: `tests`

**Count:** 37

| Function | Visibility | Parameters | Return Type | File |
|----------|------------|------------|-------------|------|
| `test_parse_addable` | private |  | `()` | `src\tests.rs:13` |
| `add` | private | self: Self, rhs: Rhs | `Self;` | `src\tests.rs:16` |
| `test_typecheck_impl` | private |  | `()` | `src\tests.rs:28` |
| `test_codegen_run` | private |  | `()` | `src\tests.rs:42` |
| `simple` | private |  | `i32 { 42 }` | `src\tests.rs:44` |
| `test_ergonomics_infer` | private |  | `()` | `src\tests.rs:51` |
| `test_phantom<T>` | private |  | `T { TimingOwned<i32> 0 }` | `src\tests.rs:53` |
| `test_derive_copy` | private |  | `()` | `src\tests.rs:64` |
| `test_mir_gen` | private |  | `()` | `src\tests.rs:75` |
| `test` | private |  | `i32` | `src\tests.rs:77` |
| `test_parallel_resolve` | private |  | `()` | `src\tests.rs:93` |
| `test_ctfe_semiring` | private |  | `()` | `src\tests.rs:109` |
| `semiring_test` | private |  | `i32` | `src\tests.rs:111` |
| `test_thin_templates` | private |  | `()` | `src\tests.rs:130` |
| `generic_add<T>` | private | a: T, b: T | `T { a.add(b) }` | `src\tests.rs:132` |
| `test_cachesafe` | private |  | `()` | `src\tests.rs:147` |
| `test_algebraic_fusion` | private |  | `()` | `src\tests.rs:157` |
| `fusion_test` | private |  | `i32` | `src\tests.rs:159` |
| `test_eop_semiring_matrix` | private |  | `()` | `src\tests.rs:179` |
| `add` | private | self: Self, rhs: Self | `Self;` | `src\tests.rs:182` |
| `mul` | private | self: Self, rhs: Self | `Self;` | `src\tests.rs:183` |
| `zero` | private |  | `Self;` | `src\tests.rs:184` |
| `one` | private |  | `Self;` | `src\tests.rs:185` |
| `add` | private | self: i32, rhs: i32 | `i32 { self + rhs }` | `src\tests.rs:188` |
| `mul` | private | self: i32, rhs: i32 | `i32 { self * rhs }` | `src\tests.rs:189` |
| `zero` | private |  | `i32 { 0 }` | `src\tests.rs:190` |
| `one` | private |  | `i32 { 1 }` | `src\tests.rs:191` |
| `matrix_mul<A: Semiring>` | private | a: [[A; 2]; 2], b: [[A; 2]; 2] | `[[A; 2]; 2]` | `src\tests.rs:193` |
| `eop_test` | private |  | `i32` | `src\tests.rs:204` |
| `test_actor_counter` | private |  | `()` | `src\tests.rs:222` |
| `actor_test` | private |  | `i32` | `src\tests.rs:234` |
| `test_stable_abi` | private |  | `()` | `src\tests.rs:251` |
| `ffi_add` | private | a: i32, b: i32 | `i32 { a + b } // No generics` | `src\tests.rs:254` |
| `bench_semiring_add` | private | b: &mut criterion::Criterion | `()` | `src\tests.rs:265` |
| `bench_add` | private |  | `i32` | `src\tests.rs:267` |
| `test_assoc_fold_fusion` | private |  | `()` | `src\tests.rs:284` |
| `assoc_fold` | private |  | `i32` | `src\tests.rs:286` |

