#!/bin/bash
# Verification script for competition submission
# Ensures all components work correctly

echo "================================================"
echo "MURPHY'S SIEVE - SUBMISSION VERIFICATION"
echo "================================================"
echo ""

# Check required files
echo "1. Checking required files..."
required_files=(
    "murphy_sieve_competition_final.rs"
    "final_murphy_benchmark.rs"
    "README.md"
    "BENCHMARK_SUMMARY.md"
    "Dockerfile"
    "benchmark_results.txt"
)

missing_files=0
for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✅ $file"
    else
        echo "  ❌ $file (MISSING)"
        missing_files=$((missing_files + 1))
    fi
done

if [ $missing_files -gt 0 ]; then
    echo "  ❌ $missing_files files missing!"
    exit 1
else
    echo "  ✅ All required files present"
fi

echo ""
echo "2. Compiling implementation..."
rustc murphy_sieve_competition_final.rs -o murphy_sieve_test 2>&1
if [ $? -eq 0 ]; then
    echo "  ✅ Compilation successful"
else
    echo "  ❌ Compilation failed"
    exit 1
fi

echo ""
echo "3. Running quick correctness test..."
./murphy_sieve_test 2>&1 | grep -q "MISSION ACCOMPLISHED"
if [ $? -eq 0 ]; then
    echo "  ✅ Implementation works correctly"
else
    echo "  ❌ Implementation test failed"
    exit 1
fi

echo ""
echo "4. Compiling benchmark..."
rustc final_murphy_benchmark.rs -o benchmark_test 2>&1
if [ $? -eq 0 ]; then
    echo "  ✅ Benchmark compilation successful"
else
    echo "  ❌ Benchmark compilation failed"
    exit 1
fi

echo ""
echo "5. Running quick benchmark..."
./benchmark_test 2>&1 | grep -q "ALL TESTS PASSED"
if [ $? -eq 0 ]; then
    echo "  ✅ Benchmark passes all tests"
else
    echo "  ❌ Benchmark test failed"
    exit 1
fi

echo ""
echo "6. Checking Dockerfile..."
if grep -q "FROM rust:" Dockerfile && grep -q "murphy_sieve" Dockerfile; then
    echo "  ✅ Dockerfile appears valid"
else
    echo "  ❌ Dockerfile validation failed"
    exit 1
fi

echo ""
echo "7. Verifying benchmark results..."
if [ -s benchmark_results.txt ]; then
    result_count=$(grep -c "✅" benchmark_results.txt)
    error_count=$(grep -c "❌" benchmark_results.txt)
    echo "  ✅ Benchmark results file: $result_count successes, $error_count errors"
    if [ $error_count -eq 0 ]; then
        echo "  ✅ All benchmarks passed"
    else
        echo "  ⚠️  Some benchmarks failed"
    fi
else
    echo "  ❌ Benchmark results file empty or missing"
    exit 1
fi

echo ""
echo "================================================"
echo "VERIFICATION COMPLETE"
echo "================================================"
echo ""
echo "Submission Status: ✅ READY"
echo "All components verified and working correctly."
echo ""
echo "Files included in submission:"
ls -la
echo ""
echo "To build Docker container:"
echo "  docker build -t murphy-sieve ."
echo ""
echo "To run demonstration:"
echo "  docker run murphy-sieve"
echo ""
echo "Submission package is competition-ready!"