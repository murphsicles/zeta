# ZETA STUB TEMPLATES

## ZETA SYNTAX RULES
1. **No `pub` keyword** - everything is public by default in Zeta
2. **Struct syntax**: `struct Name<T> { fields }`
3. **Enum syntax**: `enum Name { Variants }`
4. **Function syntax**: `fn name<T>(params) -> ReturnType { body }`
5. **Impl syntax**: `impl Type<T> { methods }`
6. **Concept syntax** (for traits): `concept Name<T> { methods }`

## TEMPLATES

### 1. BASIC STRUCT STUB
```zeta
//! Stub for {module_path}
struct {StructName}<{generics}> {
    // Stub implementation
}
```

### 2. STRUCT WITH IMPL
```zeta
//! Stub for {module_path}
struct {StructName}<{generics}> {
    // Stub implementation
}

impl<{generics}> {StructName}<{generics}> {
    fn new() -> Self {
        {StructName} {}
    }
}
```

### 3. ENUM STUB
```zeta
//! Stub for {module_path}
enum {EnumName} {
    // Variant 1
    __variant1,
    // Variant 2  
    __variant2,
}
```

### 4. FUNCTION STUB
```zeta
//! Stub for {module_path}
fn {function_name}<{generics}>({params}) -> {return_type} {
    {default_return}
}
```

### 5. CONCEPT STUB (for traits)
```zeta
//! Stub for {module_path}
// Traits are not fully supported in Zeta, using empty concept as stub
concept {ConceptName}<{generics}> {
    // Stub methods
}
```

### 6. MODULE STUB (empty)
```zeta
//! Stub for {module_path}
// Empty module stub
```

## SPECIFIC STUB TEMPLATES

### std::collections::HashMap
```zeta
//! Stub for std::collections::HashMap
struct HashMap<K, V> {
    // Stub implementation
}

impl<K, V> HashMap<K, V> {
    fn new() -> Self {
        HashMap {}
    }

    fn insert(&mut self, key: K, value: V) -> Option<V> {
        None
    }

    fn get(&self, key: &K) -> Option<&V> {
        None
    }
}
```

### std::ffi::c_void
```zeta
//! Stub for std::ffi::c_void
// c_void is used for raw pointers to opaque C data

/// Opaque type representing a void pointer in C
enum c_void {
    /// Variant 1
    __variant1,
    /// Variant 2
    __variant2,
}
```

### serde traits
```zeta
//! Stub for serde
// Traits are not fully supported in Zeta, using empty concepts as stubs
concept Deserialize {}
concept Serialize {}
```

### serde_json functions
```zeta
//! Stub for serde_json
fn to_string<T>(_value: &T) -> Result<String, ()> {
    Ok("{}".to_string())
}

fn from_str<T>(_s: &str) -> Result<T, ()> {
    unimplemented!()
}
```