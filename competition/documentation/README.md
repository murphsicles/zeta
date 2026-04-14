# Zeta Runtime Static Library

## Deliverables

1. **`zeta_runtime.c`** - Minimal C runtime implementation with:
   - `print_i64(int64_t value)` - Print integer to stdout
   - `println_i64(int64_t value)` - Print integer with newline
   - `println(void)` - Print just a newline
   - `print_bool(int64_t value)` - Print boolean
   - `println_bool(int64_t value)` - Print boolean with newline
   - `print_str(int64_t ptr)` - Print null-terminated string
   - `println_str(int64_t ptr)` - Print string with newline
   - `flush(void)` - Flush stdout

2. **`libzeta.a`** - Unix static library (created with `ar rcs`)

3. **`zeta.lib`** - Windows static library (copy of `libzeta.a` for compatibility)

4. **`zeta_runtime.o`** - Compiled object file

## Building

```bash
# Compile C runtime
gcc -c zeta_runtime.c -o zeta_runtime.o

# Create Unix static library
ar rcs libzeta.a zeta_runtime.o

# Create Windows static library (copy)
copy libzeta.a zeta.lib
```

## Testing

The runtime has been tested with:
- Existing Zeta-generated object files (`test_print_i64.exe.o`, `test_println.exe.o`)
- Custom test programs
- All functions work correctly

## Usage

Link Zeta-generated executables with the runtime library:

```bash
# Link with object file directly
gcc zeta_generated.o zeta_runtime.o -o program.exe

# Or link with static library
gcc zeta_generated.o libzeta.a -o program.exe  # Unix
gcc zeta_generated.o zeta.lib -o program.exe   # Windows
```

## Features

- **Minimal dependencies**: Only uses standard C library (stdio.h, stdint.h)
- **Cross-platform**: Works on both Unix and Windows
- **Compatible**: Matches Rust runtime function signatures exactly
- **Tested**: Verified with actual Zeta-generated code