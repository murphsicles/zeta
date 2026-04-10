// Simple test for alloc module
use doc_workspace::std::alloc;

fn main() {
    println!("Testing alloc module...");
    
    // Test malloc_raw
    unsafe {
        let ptr = alloc::malloc_raw(100);
        println!("malloc_raw(100) = {}", ptr);
        
        // Test free_raw
        alloc::free_raw(ptr);
        println!("free_raw called successfully");
        
        // Test generic malloc
        let ptr2: i64 = alloc::malloc::<u8>(10);
        println!("malloc::<u8>(10) = {}", ptr2);
        
        // Test generic free
        alloc::free::<u8>(ptr2);
        println!("free::<u8> called successfully");
        
        // Test calloc
        let ptr3: i64 = alloc::calloc::<u32>(5);
        println!("calloc::<u32>(5) = {}", ptr3);
        
        // Test realloc
        let ptr4: i64 = alloc::realloc::<u32>(ptr3, 10);
        println!("realloc::<u32>(ptr, 10) = {}", ptr4);
        
        alloc::free::<u32>(ptr4);
    }
    
    println!("All tests passed!");
}