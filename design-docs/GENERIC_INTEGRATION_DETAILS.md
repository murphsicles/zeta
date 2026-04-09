# GENERIC INTEGRATION DETAILED SPECIFICATIONS

## 📊 DETAILED DATA FLOW

### 1. Parser Data Structures Enhancement

**Current AST (simplified):**
```rust
struct FuncDef {
    name: String,
    generics: Vec<String>,           // ["T", "U"]
    params: Vec<(String, String)>,   // [("x", "T"), ("y", "U")]
    ret: String,                     // "T"
    body: Vec<AstNode>,
}

struct Call {
    receiver: Option<Box<AstNode>>,
    method: String,
    args: Vec<AstNode>,
    type_args: Vec<String>,          // ["i32", "String"]
    structural: bool,
}
```

**Enhanced AST with Type Information:**
```rust
// New type for generic parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    Type {
        name: String,
        bounds: Vec<TraitBound>,     // Trait bounds: T: Debug + Clone
    },
    Lifetime {
        name: String,
        bounds: Vec<LifetimeBound>,  // Lifetime bounds: 'a: 'b
    },
    Const {
        name: String,
        ty: Type,                    // Const parameter type: const N: usize
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBound {
    trait_name: String,
    type_args: Vec<Type>,            // Generic trait: Debug<T>
}

// Enhanced AST nodes
pub enum AstNode {
    FuncDef {
        name: String,
        generics: Vec<GenericParam>,  // Enhanced
        params: Vec<(String, Type)>,  // Type instead of String
        ret: Type,                    // Type instead of String
        body: Vec<AstNode>,
        // ... other fields
    },
    
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<Type>,         // Type instead of String
        structural: bool,
    },
    
    // Type alias with generics
    TypeAlias {
        name: String,
        generics: Vec<GenericParam>,  // New
        ty: Type,                     // Type instead of String
        pub_: bool,
    },
    
    // Struct definition with generics
    StructDef {
        name: String,
        generics: Vec<GenericParam>,  // Enhanced
        fields: Vec<(String, Type)>,  // Type instead of String
        // ... other fields
    },
}
```

### 2. Type Context Propagation

**Type Environment Stack:**
```rust
struct TypeEnv {
    // Current scope variables
    variables: HashMap<String, Type>,
    
    // Generic parameters in scope
    generic_params: HashMap<String, GenericParam>,
    
    // Current substitutions for type variables
    substitution: Substitution,
    
    // Lifetime context
    lifetimes: LifetimeContext,
    
    // Parent environment (for nested scopes)
    parent: Option<Rc<TypeEnv>>,
    
    // Source location for error reporting
    location: SourceLocation,
}

impl TypeEnv {
    // Enter a new scope (e.g., function body)
    fn enter_scope(&self) -> TypeEnv {
        TypeEnv {
            variables: HashMap::new(),
            generic_params: self.generic_params.clone(),
            substitution: self.substitution.clone(),
            lifetimes: self.lifetimes.clone(),
            parent: Some(Rc::new(self.clone())),
            location: self.location.clone(),
        }
    }
    
    // Add generic parameters to scope
    fn add_generic_params(&mut self, params: &[GenericParam]) {
        for param in params {
            match param {
                GenericParam::Type { name, bounds } => {
                    // Create a fresh type variable for this generic parameter
                    let ty_var = Type::Variable(TypeVar::fresh());
                    self.generic_params.insert(name.clone(), param.clone());
                    self.variables.insert(name.clone(), ty_var);
                }
                GenericParam::Lifetime { name, bounds } => {
                    // Create a fresh lifetime variable
                    let lt_var = Lifetime::Variable(LifetimeVar::fresh());
                    self.lifetimes.add_lifetime(name.clone(), lt_var);
                }
                GenericParam::Const { name, ty } => {
                    // Const parameters are handled differently
                    self.variables.insert(name.clone(), ty.clone());
                }
            }
        }
    }
    
    // Look up a type, resolving generic parameters
    fn lookup_type(&self, name: &str) -> Option<Type> {
        // Check if it's a generic parameter
        if let Some(param) = self.generic_params.get(name) {
            match param {
                GenericParam::Type { .. } => {
                    // Return the type variable for this generic parameter
                    self.variables.get(name).cloned()
                }
                _ => None,
            }
        } else {
            // Check current variables
            self.variables.get(name).cloned().or_else(|| {
                // Check parent scope
                self.parent.as_ref().and_then(|p| p.lookup_type(name))
            })
        }
    }
}
```

### 3. Constraint Collection and Solving

**Constraint Types:**
```rust
#[derive(Debug, Clone)]
pub enum Constraint {
    // Type equality: T = U
    Equality(Type, Type, SourceLocation),
    
    // Trait bound: T: Debug
    TraitBound(Type, TraitBound, SourceLocation),
    
    // Lifetime bound: 'a: 'b
    LifetimeBound(Lifetime, Lifetime, SourceLocation),
    
    // Type well-formedness: T is a valid type
    WellFormed(Type, SourceLocation),
    
    // Generic instantiation: F<T> = ConcreteType
    Instantiation(Type, Vec<Type>, Type, SourceLocation),
}

struct ConstraintSet {
    constraints: Vec<Constraint>,
    unsolved: Vec<Constraint>,
    errors: Vec<TypeError>,
}

impl ConstraintSet {
    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
        self.unsolved.push(constraint);
    }
    
    fn solve(&mut self, env: &mut TypeEnv) -> Result<(), Vec<TypeError>> {
        while !self.unsolved.is_empty() {
            let constraint = self.unsolved.pop().unwrap();
            
            match constraint {
                Constraint::Equality(t1, t2, loc) => {
                    if let Err(e) = env.substitution.unify(&t1, &t2) {
                        self.errors.push(TypeError::UnificationError(e, loc));
                    }
                }
                Constraint::TraitBound(ty, bound, loc) => {
                    // Check trait implementation
                    if !self.check_trait_bound(env, &ty, &bound) {
                        self.errors.push(TypeError::TraitBoundError(ty, bound, loc));
                    }
                }
                // ... other constraint types
            }
        }
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
}
```

### 4. Monomorphization Data Flow

**Generic Function Representation:**
```rust
struct GenericFunction {
    name: String,
    generic_params: Vec<GenericParam>,
    param_types: Vec<Type>,
    return_type: Type,
    body: TypedAst,  // AST with type variables
    
    // Cache of concrete instantiations
    instantiations: HashMap<Vec<Type>, ConcreteFunction>,
}

struct ConcreteFunction {
    generic_fn: Rc<GenericFunction>,
    type_args: Vec<Type>,
    param_types: Vec<Type>,    // With type_args substituted
    return_type: Type,         // With type_args substituted
    mir: MIRFunction,
    machine_code: Option<MachineCode>,
}

struct Monomorphizer {
    // Cache of generic functions
    generic_fns: HashMap<String, Rc<GenericFunction>>,
    
    // Cache of concrete functions
    concrete_fns: HashMap<(String, Vec<Type>), ConcreteFunction>,
    
    // Specialization decisions
    specialization_policy: SpecializationPolicy,
}

impl Monomorphizer {
    fn monomorphize_call(
        &mut self,
        generic_fn_name: &str,
        type_args: &[Type],
        call_site: SourceLocation,
    ) -> Result<ConcreteFunction, MonomorphizationError> {
        // Get generic function
        let generic_fn = self.generic_fns.get(generic_fn_name)
            .ok_or_else(|| MonomorphizationError::UnknownFunction(generic_fn_name.to_string()))?;
        
        // Check if we already have this instantiation
        let key = (generic_fn_name.to_string(), type_args.to_vec());
        if let Some(concrete) = self.concrete_fns.get(&key) {
            return Ok(concrete.clone());
        }
        
        // Check type argument count
        if type_args.len() != generic_fn.generic_params.len() {
            return Err(MonomorphizationError::ArityMismatch(
                generic_fn_name.to_string(),
                generic_fn.generic_params.len(),
                type_args.len(),
                call_site,
            ));
        }
        
        // Create substitution map
        let mut substitution = Substitution::new();
        for (param, arg) in generic_fn.generic_params.iter().zip(type_args) {
            if let GenericParam::Type { name, .. } = param {
                let type_var = Type::Variable(TypeVar::from_name(name));
                substitution.mapping.insert(type_var, arg.clone());
            }
        }
        
        // Apply substitution to get concrete types
        let param_types: Vec<Type> = generic_fn.param_types.iter()
            .map(|ty| substitution.apply(ty))
            .collect();
        let return_type = substitution.apply(&generic_fn.return_type);
        
        // Generate MIR for concrete types
        let mir = self.generate_mir(generic_fn, &substitution);
        
        // Create concrete function
        let concrete = ConcreteFunction {
            generic_fn: generic_fn.clone(),
            type_args: type_args.to_vec(),
            param_types,
            return_type,
            mir,
            machine_code: None,
        };
        
        // Cache it
        self.concrete_fns.insert(key, concrete.clone());
        
        Ok(concrete)
    }
}
```

## 🔌 API SPECIFICATIONS DETAILED

### Parser API

```rust
// Enhanced parser module
mod parser {
    // Parse generic parameter list
    pub fn parse_generic_params(input: &str) -> IResult<&str, Vec<GenericParam>> {
        delimited(
            ws(tag("<")),
            separated_list0(
                ws(tag(",")),
                alt((
                    parse_type_param,
                    parse_lifetime_param,
                    parse_const_param,
                )),
            ),
            ws(tag(">")),
        ).parse(input)
    }
    
    // Parse type parameter: T, T: Debug, T: Debug + Clone
    fn parse_type_param(input: &str) -> IResult<&str, GenericParam> {
        let (input, name) = parse_ident(input)?;
        let (input, bounds) = opt(preceded(
            ws(tag(":")),
            separated_list1(ws(tag("+")), parse_trait_bound),
        )).parse(input)?;
        
        Ok((input, GenericParam::Type {
            name,
            bounds: bounds.unwrap_or_default(),
        }))
    }
    
    // Parse where clauses: where T: Debug, U: Clone
    pub fn parse_where_clauses(input: &str) -> IResult<&str, Vec<WhereClause>> {
        let (input, _) = ws(tag("where")).parse(input)?;
        separated_list1(
            ws(tag(",")),
            parse_where_clause,
        ).parse(input)
    }
}
```

### Type Checker API

```rust
// Main type checking interface
pub struct TypeChecker {
    env: TypeEnv,
    constraints: ConstraintSet,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    // Check a generic function definition
    pub fn check_generic_function(
        &mut self,
        fn_def: &FuncDef,
    ) -> Result<GenericFunction, Vec<TypeError>> {
        // Enter new scope for function
        let mut fn_env = self.env.enter_scope();
        
        // Add generic parameters to scope
        fn_env.add_generic_params(&fn_def.generics);
        
        // Check parameter types
        let param_types: Result<Vec<Type>, TypeError> = fn_def.params.iter()
            .map(|(name, ty)| {
                // Parse type string to Type (or use already-parsed Type)
                self.parse_type(ty)
            })
            .collect();
        let param_types = param_types?;
        
        // Check return type
        let return_type = self.parse_type(&fn_def.ret)?;
        
        // Check body in function environment
        let old_env = std::mem::replace(&mut self.env, fn_env);
        let body_result = self.check_block(&fn_def.body);
        self.env = old_env;
        
        body_result?;
        
        // Create generic function representation
        Ok(GenericFunction {
            name: fn_def.name.clone(),
            generic_params: fn_def.generics.clone(),
            param_types,
            return_type,
            body: self.current_typed_ast.clone(), // Simplified
        })
    }
    
    // Check a generic function call
    pub fn check_generic_call(
        &mut self,
        call: &Call,
    ) -> Result<Type, TypeError> {
        // Look up function
        let generic_fn = self.lookup_function(&call.method)?;
        
        // Infer type arguments if not explicitly provided
        let type_args = if call.type_args.is_empty() {
            self.infer_type_args(&generic_fn, call)?
        } else {
            // Parse provided type arguments
            call.type_args.iter()
                .map(|ty_str| self.parse_type(ty_str))
                .collect::<Result<Vec<Type>, _>>()?
        };
        
        // Check argument types
        for (arg, expected_ty) in call.args.iter().zip(&generic_fn.param_types) {
            let arg_ty = self.check_expr(arg)?;
            self.constraints.add_constraint(Constraint::Equality(
                arg_ty,
                expected_ty.clone(),
                call.location,
            ));
        }
        
        // Get return type with type arguments substituted
        let return_type = self.instantiate_type(
            &generic_fn.return_type,
            &type_args,
            call.location,
        )?;
        
        Ok(return_type)
    }
}
```

### Codegen API

```rust
// Monomorphization manager
pub struct MonomorphizationManager {
    generic_fns: HashMap<String, Rc<GenericFunction>>,
    concrete_fns: HashMap<ConcreteFnKey, ConcreteFunction>,
    codegen: CodeGenerator,
}

impl MonomorphizationManager {
    // Process a generic call in MIR
    pub fn process_generic_call(
        &mut self,
        mir_call: &MIRGenericCall,
    ) -> Result<MIRConcreteCall, CodegenError> {
        // Get or create concrete function
        let concrete_fn = self.get_concrete_function(
            &mir_call.function_name,
            &mir_call.type_args,
            mir_call.location,
        )?;
        
        // Generate code if not already generated
        if concrete_fn.machine_code.is_none() {
            let machine_code = self.codegen.generate_function(&concrete_fn.mir)?;
            // Update concrete function with generated code
            // (In real implementation, would need mutable reference or copy)
        }
        
        // Create concrete call
        Ok(MIRConcreteCall {
            function: concrete_fn,
            args: mir_call.args.clone(),
            location: mir_call.location,
        })
    }
    
    // Get or create concrete function
    fn get_concrete_function(
        &mut self,
        generic_name: &str,
        type_args: &[Type],
        location: SourceLocation,
    ) -> Result<ConcreteFunction, CodegenError> {
        let key = ConcreteFnKey::new(generic_name, type_args);
        
        // Check cache
        if let Some(concrete) = self.concrete_fns.get(&key) {
            return Ok(concrete.clone());
        }
        
        // Get generic function
        let generic_fn = self.generic_fns.get(generic_name)
            .ok_or_else(|| CodegenError::UnknownGenericFunction(
                generic_name.to_string(),
                location,
            ))?;
        
        // Create substitution
        let substitution = self.create_substitution(generic_fn, type_args)?;
        
        // Generate MIR with concrete types
        let mir = self.generate_concrete_mir(generic_fn, &substitution)?;
        
        // Create concrete function
        let concrete = ConcreteFunction {
            generic_fn: generic_fn.clone(),
            type_args: type_args.to_vec(),
            param_types: self.apply_substitution(&generic_fn.param_types, &substitution),
            return_type: self.apply_substitution(&[generic_fn.return_type.clone()], &substitution)[0].clone(),
            mir,
            machine_code: None,
        };
        
        // Cache it
        self.concrete_fns.insert(key, concrete.clone());
        
        Ok(concrete)
    }
}
```

## 🧩 INTEGRATION TEST CASES

### Test Case 1: Basic Generic Function
```zeta
fn identity<T>(x: T) -> T {
    return x;
}

fn main() {
    let x: i32 = identity(42);
    let y: &str = identity("hello");
}
```

**Integration Points:**
1. **Parser**: Parse `identity<T>` generic syntax
2. **Type Checker**: Infer `T` as `i32` and `&str`
3. **Monomorphizer**: Create `identity_i32` and `identity_str`
4. **Codegen**: Generate specialized machine code

### Test Case 2: Generic Struct
```zeta
struct Pair<T, U> {
    first: T,
    second: U,
}

impl<T, U> Pair<T, U> {
    fn new(first: T, second: U) -> Self {
        Pair { first, second }
    }
}

fn main() {
    let p1: Pair<i32, &str> = Pair::new(42, "hello");
    let p2 = Pair::new(3.14