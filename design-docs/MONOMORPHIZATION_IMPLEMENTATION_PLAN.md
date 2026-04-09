# Monomorphization Implementation Plan

## Immediate Actions (Next 60 Minutes)

### Step 1: Analyze Current MIR for Generic Information (10 min)
- [ ] Check if MIR contains generic parameter information
- [ ] Examine how type arguments are passed in `MirStmt::Call`
- [ ] Verify type system can provide generic definitions

### Step 2: Implement Core Infrastructure (20 min)
- [ ] Add `specialized_fns` and `specialized_types` caches to `LLVMCodegen`
- [ ] Implement `mangle_type_name()` function
- [ ] Add `mangled_name()` method to `Type` enum
- [ ] Create `get_function_with_types()` method

### Step 3: Update Function Call Handling (15 min)
- [ ] Modify `gen_stmt()` to use type arguments in calls
- [ ] Update `get_function()` to handle mangled names
- [ ] Add fallback for non-generic functions

### Step 4: Create Basic Tests (15 min)
- [ ] Add test for type mangling
- [ ] Add test for generic function lookup
- [ ] Verify existing tests still pass

## Detailed Implementation Steps

### 1. Type Mangling Implementation

#### File: `src/middle/types/mod.rs`
```rust
impl Type {
    pub fn mangled_name(&self) -> String {
        match self {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Array(inner, size) => format!("Array_{}_{}", inner.mangled_name(), size),
            Type::Slice(inner) => format!("Slice_{}", inner.mangled_name()),
            Type::Tuple(types) => {
                let mut name = "Tuple".to_string();
                for ty in types {
                    name.push('_');
                    name.push_str(&ty.mangled_name());
                }
                name
            }
            Type::Ptr(inner) => format!("Ptr_{}", inner.mangled_name()),
            Type::Ref(inner, lifetime, mutability) => {
                let mut_str = match mutability {
                    Mutability::Immutable => "immut",
                    Mutability::Mutable => "mut",
                };
                format!("Ref_{}_{}_{}", lifetime.mangled_name(), inner.mangled_name(), mut_str)
            }
            Type::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let mut mangled = name.clone();
                    mangled.push('_');
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            mangled.push('_');
                        }
                        mangled.push_str(&arg.mangled_name());
                    }
                    mangled
                }
            }
            Type::Function(params, ret) => {
                let mut name = "Fn".to_string();
                for param in params {
                    name.push('_');
                    name.push_str(&param.mangled_name());
                }
                name.push_str("_to_");
                name.push_str(&ret.mangled_name());
                name
            }
            Type::Variable(var) => format!("Var_{}", var.0),
            Type::Error => "Error".to_string(),
        }
    }
}

impl Lifetime {
    pub fn mangled_name(&self) -> String {
        match self {
            Lifetime::Static => "static",
            Lifetime::Named(name) => name.clone(),
            Lifetime::Variable(var) => format!("ltvar_{}", var.0),
        }
    }
}
```

#### File: `src/backend/codegen/codegen.rs`
```rust
impl<'ctx> LLVMCodegen<'ctx> {
    fn mangle_function_name(&self, base_name: &str, type_args: &[Type]) -> String {
        if type_args.is_empty() {
            return base_name.to_string();
        }
        
        let mut mangled = base_name.to_string();
        mangled.push_str("_inst");
        
        for ty in type_args {
            mangled.push('_');
            mangled.push_str(&ty.mangled_name());
        }
        
        mangled
    }
}
```

### 2. Update LLVMCodegen Struct

```rust
pub struct LLVMCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub i64_type: IntType<'ctx>,
    pub f64_type: inkwell::types::FloatType<'ctx>,
    pub vec4_i64_type: VectorType<'ctx>,
    pub ptr_type: PointerType<'ctx>,
    pub locals: HashMap<u32, PointerValue<'ctx>>,
    pub fns: HashMap<String, FunctionValue<'ctx>>,
    
    // NEW: Caches for monomorphized functions and types
    pub specialized_fns: HashMap<String, FunctionValue<'ctx>>,
    pub specialized_types: HashMap<String, inkwell::types::StructType<'ctx>>,
    
    // NEW: Map from generic function names to their MIR definitions
    pub generic_defs: HashMap<String, Mir>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        // ... existing initialization ...
        
        Self {
            context,
            module,
            builder,
            i64_type,
            f64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            fns: HashMap::new(),
            specialized_fns: HashMap::new(),
            specialized_types: HashMap::new(),
            generic_defs: HashMap::new(),
        }
    }
}
```

### 3. Update gen_mirs() to Handle Generic Functions

```rust
pub fn gen_mirs(&mut self, mirs: &[Mir]) {
    // First pass: collect all functions
    for mir in mirs {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        
        // Check if this is a generic function
        let is_generic = self.is_generic_function(mir);
        
        if is_generic {
            // Store generic definition for later instantiation
            self.generic_defs.insert(fn_name.clone(), mir.clone());
        } else {
            // Non-generic function: declare as before
            let param_types: Vec<_> = (0..mir.param_indices.len())
                .map(|_| self.i64_type.into())
                .collect();
            let fn_type = self.i64_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(&fn_name, fn_type, None);
            self.fns.insert(fn_name.clone(), fn_val);
        }
    }
    
    // Second pass: generate non-generic function bodies
    for mir in mirs {
        if !self.is_generic_function(mir) {
            self.gen_fn(mir);
        }
    }
}

fn is_generic_function(&self, mir: &Mir) -> bool {
    // Check if function has type parameters in its signature
    // For now, we can check if any call in the function has type_args
    mir.stmts.iter().any(|stmt| match stmt {
        MirStmt::Call { type_args, .. } => !type_args.is_empty(),
        _ => false,
    })
}
```

### 4. Implement get_function_with_types()

```rust
fn get_function_with_types(&mut self, name: &str, type_args: &[Type]) -> FunctionValue<'ctx> {
    // If no type arguments, use regular lookup
    if type_args.is_empty() {
        return self.get_function(name);
    }
    
    // Generate mangled name
    let mangled_name = self.mangle_function_name(name, type_args);
    
    // Check cache first
    if let Some(&f) = self.specialized_fns.get(&mangled_name) {
        return f;
    }
    
    // Check if we have a generic definition
    if let Some(generic_mir) = self.generic_defs.get(name) {
        // Monomorphize the generic function
        let monomorphized_fn = self.monomorphize_function(generic_mir, name, type_args);
        self.specialized_fns.insert(mangled_name, monomorphized_fn);
        return monomorphized_fn;
    }
    
    // Fallback: try regular lookup (for non-generic functions called with empty type_args)
    self.get_function(name)
}
```

### 5. Basic Monomorphization Implementation

```rust
fn monomorphize_function(&mut self, generic_mir: &Mir, name: &str, type_args: &[Type]) -> FunctionValue<'ctx> {
    let mangled_name = self.mangle_function_name(name, type_args);
    
    // Create function with mangled name
    let param_types: Vec<_> = (0..generic_mir.param_indices.len())
        .map(|_| self.i64_type.into())
        .collect();
    let fn_type = self.i64_type.fn_type(&param_types, false);
    let fn_val = self.module.add_function(&mangled_name, fn_type, None);
    
    // Store in regular functions map too
    self.fns.insert(mangled_name.clone(), fn_val);
    
    // Generate function body (simplified - just copy for now)
    // TODO: Implement proper type substitution
    self.gen_fn(generic_mir);
    
    fn_val
}
```

### 6. Update gen_stmt() for Generic Calls

```rust
MirStmt::Call { func, args, dest, type_args } => {
    let callee = if !type_args.is_empty() {
        self.get_function_with_types(func, type_args)
    } else {
        self.get_function(func)
    };
    
    // Rest of call generation remains the same...
    let arg_vals: Vec<BasicMetadataValueEnum> = args
        .iter()
        .map(|&id| self.gen_expr_safe(&id, exprs).into())
        .collect();
    
    let call = self
        .builder
        .build_call(callee, &arg_vals, &format!("call_{dest}"))
        .unwrap();
    
    if let Some(val) = Self::call_site_to_basic_value(call) {
        let alloca = *self.locals.get(dest).unwrap();
        self.builder.build_store(alloca, val).unwrap();
    }
}
```

## Test Cases to Implement

### Test 1: Type Mangling
```rust
#[test]
fn test_type_mangling() {
    let ty_i32 = Type::I32;
    assert_eq!(ty_i32.mangled_name(), "i32");
    
    let ty_vec_i32 = Type::Named("Vec".to_string(), vec![Type::I32]);
    assert_eq!(ty_vec_i32.mangled_name(), "Vec_i32");
    
    let ty_option_box_i32 = Type::Named(
        "Option".to_string(), 
        vec![Type::Named("Box".to_string(), vec![Type::I32])]
    );
    assert_eq!(ty_option_box_i32.mangled_name(), "Option_Box_i32");
}
```

### Test 2: Function Name Mangling
```rust
#[test]
fn test_function_name_mangling() {
    let codegen = LLVMCodegen::new(&Context::create(), "test");
    
    let mangled = codegen.mangle_function_name("vec_new", &[Type::I32]);
    assert_eq!(mangled, "vec_new_inst_i32");
    
    let mangled = codegen.mangle_function_name("Vec::new", &[Type::I32]);
    assert_eq!(mangled, "Vec::new_inst_i32");
}
```

### Test 3: Generic Function Call
```rust
#[test]
fn test_generic_function_call() {
    // Test that vec_new::<i32>() generates vec_new_inst_i32
    let mut codegen = LLVMCodegen::new(&Context::create(), "test");
    
    // Create a MIR with generic call
    let mir = Mir {
        name: Some("main".to_string()),
        stmts: vec![MirStmt::Call {
            func: "vec_new".to_string(),
            args: vec![],
            dest: 1,
            type_args: vec![Type::I32],
        }],
        // ... rest of MIR ...
    };
    
    codegen.gen_mirs(&[mir]);
    
    // Verify function was generated with mangled name
    assert!(codegen.module.get_function("vec_new_inst_i32").is_some());
}
```

## Integration Points

### With Type System (SEM)
1. Need method to get generic function definitions
2. Need type substitution logic
3. Need to handle trait bounds (future)

### With Parser (LEX)
1. Verify `type_args` is populated in MIR
2. Test with `::<T>` syntax in source code

## Next Steps After Initial Implementation

1. **Type Substitution**: Implement proper type parameter substitution in function bodies
2. **Struct Type Generation**: Generate LLVM types for generic structs
3. **Performance Optimization**: Improve caching and avoid redundant generation
4. **Debug Information**: Add source mapping for monomorphized functions
5. **Error Handling**: Better error messages for monomorphization failures

## Risk Mitigation

1. **Start Simple**: Implement basic mangling first, then add complexity
2. **Test Incrementally**: Add tests for each component
3. **Fallback Paths**: Maintain compatibility with non-generic code
4. **Monitor Performance**: Watch for exponential blowup with many instantiations

## Success Metrics

1. ✅ Generic function calls compile without errors
2. ✅ Generated LLVM IR contains monomorphized functions
3. ✅ Existing non-generic code continues to work
4. ✅ Basic caching prevents redundant generation
5. ✅ Tests pass for type mangling and function instantiation