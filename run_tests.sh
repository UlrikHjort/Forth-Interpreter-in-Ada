#!/bin/bash
# Test runner for Forth interpreter
# Runs all tests and reports results

FORTH=./bin/forth
TESTS_DIR=tests
PASSED=0
FAILED=0
TOTAL=0

echo "Running Forth Interpreter Tests"
echo "================================"
echo ""

# Check if forth executable exists
if [ ! -f "$FORTH" ]; then
    echo "Error: Forth interpreter not found at $FORTH"
    echo "Please run 'make' first"
    exit 1
fi

# Run each test file
for test_file in $TESTS_DIR/*.fth; do
    TOTAL=$((TOTAL + 1))
    test_name=$(basename "$test_file" .fth)
    
    echo -n "Testing $test_name... "
    
    # Run the test and capture output
    output=$($FORTH < "$test_file" 2>&1)
    exit_code=$?
    
    # Extract just the test output (stars and F's)
    # Skip interpreter header and extract test markers
    test_output=$(echo "$output" | tail -n +6 | grep -o "[*F]" | tr -d '\n')
    
    # Check if test passed (all '*' characters, no 'F')
    if echo "$test_output" | grep -q "F"; then
        echo "FAILED"
        echo "  Expected all *, got: $test_output"
        FAILED=$((FAILED + 1))
    elif [ $exit_code -ne 0 ] && [ $exit_code -ne 1 ]; then
        echo "FAILED (exit code: $exit_code)"
        FAILED=$((FAILED + 1))
    elif [ -z "$test_output" ]; then
        echo "FAILED (no output)"
        FAILED=$((FAILED + 1))
    else
        echo "PASSED ($test_output)"
        PASSED=$((PASSED + 1))
    fi
done

echo ""
echo "================================"
echo "Test Results: $PASSED/$TOTAL passed, $FAILED failed"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "All tests passed! ✓"
    exit 0
else
    echo "Some tests failed. ✗"
    exit 1
fi
