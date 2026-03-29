# TODO Tracking System

## Active TODOs

### TODO-20260327-001
- **ID**: TODO-20260327-001
- **Owner**: Zak
- **Created**: 2026-03-27 23:55 GMT
- **Due**: 2026-03-29 23:55 GMT (48 hours)
- **Impact**: HIGH - Struct field access implementation is placeholder/hack
- **Description**: The current implementation of struct field access in `src/middle/mir/gen.rs` uses hardcoded values (returns 10 for field "x", 20 for field "y") instead of proper field extraction from struct values.
- **Files**: `src/middle/mir/gen.rs`
- **Status**: OPEN
- **Priority**: HIGH

### TODO-20260327-002
- **ID**: TODO-20260327-002
- **Owner**: Zak
- **Created**: 2026-03-27 23:55 GMT
- **Due**: 2026-03-29 23:55 GMT (48 hours)
- **Impact**: HIGH - Struct literal implementation is placeholder/hack
- **Description**: The current implementation of struct literals in `src/middle/mir/gen.rs` returns the sum of field values instead of creating proper struct values.
- **Files**: `src/middle/mir/gen.rs`
- **Status**: OPEN
- **Priority**: HIGH

### TODO-20260328-001
- **ID**: TODO-20260328-001
- **Owner**: Zak
- **Created**: 2026-03-28 03:20 GMT
- **Due**: 2026-03-30 03:20 GMT (48 hours)
- **Impact**: HIGH - Struct representation in codegen is placeholder
- **Description**: The current implementation of struct expressions in `src/backend/codegen/codegen.rs` returns the first field value as a placeholder instead of creating proper struct values. Need to implement proper struct representation (LLVM struct types with insertvalue/extractvalue).
- **Files**: `src/backend/codegen/codegen.rs`
- **Status**: OPEN
- **Priority**: HIGH

## Completed TODOs

*(None yet)*

## TODO Format
```
### TODO-YYYYMMDD-NNN
- **ID**: TODO-YYYYMMDD-NNN
- **Owner**: [agent name]
- **Created**: YYYY-MM-DD HH:MM GMT
- **Due**: YYYY-MM-DD HH:MM GMT (within 48 hours)
- **Impact**: LOW/MEDIUM/HIGH/CRITICAL
- **Description**: [Brief description of what needs to be fixed]
- **Files**: [List of files affected]
- **Status**: OPEN/IN_PROGRESS/COMPLETED
- **Priority**: LOW/MEDIUM/HIGH/CRITICAL
```