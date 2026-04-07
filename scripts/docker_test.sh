#!/bin/bash
echo "=== Testing Zeta Compiler with Docker ==="
echo "Time: $(date)"
echo ""

WORKSPACE="/mnt/c/Users/mummy/.openclaw/workspace"
cd "$WORKSPACE"

echo "1. Building test Docker image..."
docker build -f Dockerfile.test -t zeta-test .

echo ""
echo "2. Running while loop test..."
docker run --rm zeta-test ./target/release/zetac /workspace/tests/test_while_final_correct.z
WHILE_RESULT=$?

echo ""
echo "3. Running Murphy's Sieve test..."
docker run --rm zeta-test ./target/release/zetac /workspace/tests/murphy_after_fix.z
MURPHY_RESULT=$?

echo ""
echo "=== Test Results ==="
echo "While loop test exit code: $WHILE_RESULT"
echo "Murphy's Sieve test exit code: $MURPHY_RESULT"

if [ $WHILE_RESULT -eq 0 ]; then
    echo "✓ While loop test PASSED - infinite loop bug appears FIXED"
else
    echo "❌ While loop test FAILED - exit code: $WHILE_RESULT"
fi

if [ $MURPHY_RESULT -eq 0 ]; then
    echo "✓ Murphy's Sieve test PASSED - infinite loop bug appears FIXED"
else
    echo "❌ Murphy's Sieve test FAILED - exit code: $MURPHY_RESULT"
fi

echo ""
echo "4. Testing competition Dockerfile..."
docker build -f Dockerfile.competition -t zeta-competition .

echo ""
echo "=== Docker Test Complete ==="
echo "If both tests passed (exit code 0), the infinite loop bug is FIXED in Linux."
echo "If tests timed out or failed, the bug is NOT fixed."