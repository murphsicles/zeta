#!/usr/bin/env python3
"""
Quick verification of PrimeZeta submission package
"""

import os
import sys

def check_file_exists(path, description):
    """Check if a file exists and print status"""
    if os.path.exists(path):
        print(f"✓ {description}: {path}")
        return True
    else:
        print(f"✗ {description}: {path} - MISSING")
        return False

def check_file_content(path, required_strings, description):
    """Check if file contains required strings"""
    try:
        with open(path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        all_found = True
        for req in required_strings:
            if req in content:
                print(f"  ✓ Contains: {req}")
            else:
                print(f"  ✗ Missing: {req}")
                all_found = False
        
        return all_found
    except Exception as e:
        print(f"  ✗ Error reading {path}: {e}")
        return False

def main():
    print("="*60)
    print("PrimeZeta Submission Package Verification")
    print("="*60)
    print()
    
    base_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(base_dir)
    
    # Check critical files
    critical_files = [
        ("src/prime.z", "Main algorithm file"),
        ("README.md", "Documentation"),
        ("Dockerfile", "Docker build file"),
        ("run.sh", "Execution script"),
        ("prime_benchmark.rs", "Benchmark runner"),
        ("BENCHMARK_RESULTS.md", "Performance analysis"),
        ("Cargo.toml", "Rust configuration"),
        ("LICENSE", "License file"),
    ]
    
    all_critical = True
    for file_path, description in critical_files:
        if not check_file_exists(file_path, description):
            all_critical = False
    
    print()
    
    # Check prime.z content
    print("Checking prime.z algorithm tags...")
    prime_tags = [
        "algorithm=wheel",
        "faithful=yes",
        "bits=8",
        "parallel=yes"
    ]
    
    if check_file_exists("src/prime.z", "prime.z"):
        check_file_content("src/prime.z", prime_tags, "Algorithm tags")
    
    print()
    
    # Check README badges
    print("Checking README.md for key information...")
    readme_checks = [
        "Algorithm: wheel",
        "Faithful: yes",
        "Bits: 8",
        "Parallel: yes",
        "78,498 primes"
    ]
    
    if check_file_exists("README.md", "README.md"):
        check_file_content("README.md", readme_checks, "README content")
    
    print()
    
    # Check Dockerfile
    print("Checking Dockerfile structure...")
    docker_checks = [
        "FROM rust:",
        "COPY src/prime.z",
        "ENTRYPOINT"
    ]
    
    if check_file_exists("Dockerfile", "Dockerfile"):
        check_file_content("Dockerfile", docker_checks, "Dockerfile content")
    
    print()
    
    # Check benchmark output format
    print("Checking benchmark output format...")
    benchmark_checks = [
        'zeta;',
        'algorithm=wheel;faithful=yes;bits=8;parallel=yes'
    ]
    
    if check_file_exists("prime_benchmark.rs", "prime_benchmark.rs"):
        check_file_content("prime_benchmark.rs", benchmark_checks, "Benchmark format")
    
    print()
    print("="*60)
    
    if all_critical:
        print("SUCCESS: All critical files present!")
        print()
        print("Next steps:")
        print("1. Run: ./run.sh (requires Docker)")
        print("2. Verify output matches competition format")
        print("3. Push to GitHub repository")
        print("4. Submit to competition!")
        return 0
    else:
        print("WARNING: Some files are missing or incomplete.")
        print("Please check the errors above before submission.")
        return 1

if __name__ == "__main__":
    sys.exit(main())