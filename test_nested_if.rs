fn main() {
    // Test case 1: Simple if with comparison
    let result1 = if 1 < 2 { 123 } else { 0 };
    println!("Simple if result: {}", result1);
    
    // Test case 2: Nested if with comparisons
    let result2 = if 1 < 2 { 
        if 3 < 4 { 
            123 
        } else { 
            0 
        }
    } else { 
        0 
    };
    println!("Nested if result: {}", result2);
    
    // Test case 3: Direct nested if expression
    let result3 = if 1 < 2 { if 3 < 4 { 123 } else { 0 } } else { 0 };
    println!("Direct nested if result: {}", result3);
    
    // Test case 4: Check boolean representation
    let bool_val = 1 < 2;
    println!("1 < 2 evaluates to: {}", bool_val);
    println!("Type of bool_val: {}", std::any::type_name_of_val(&bool_val));
}