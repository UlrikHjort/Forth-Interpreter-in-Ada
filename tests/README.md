# Forth Test Suite

This directory contains test files for the Forth interpreter.

## Test Files

Each `.fth` file is a Forth program that tests specific functionality:

- `arithmetic.fth` - Basic arithmetic operations (+, -, *, /, MOD)
- `stack.fth` - Stack manipulation (DUP, DROP, SWAP, OVER, ROT, etc.)
- `words.fth` - Word definitions and composition
- `variables.fth` - Variables and constants
- `conditionals.fth` - IF/THEN/ELSE control flow
- `loops.fth` - DO/LOOP and BEGIN/UNTIL loops
- `comparisons.fth` - Comparison operators (=, <, >, etc.)

## Test Convention

Tests use a simple assertion pattern:
- Print `*` (ASCII 42) if a test passes
- Print `F` (ASCII 70) if a test fails
- Each test should print a newline (CR) after the result

Example:
```forth
5 3 + 8 = IF 42 EMIT ELSE 70 EMIT THEN CR
```

This checks if `5 + 3 = 8`, printing `*` if true, `F` if false.

## Running Tests

### Run all tests:
```bash
./run_tests.sh
```

### Run a specific test:
```bash
./bin/forth < tests/arithmetic.fth
```

## Adding New Tests

1. Create a new `.fth` file in the `tests/` directory
2. Add test cases using the assertion pattern
3. Include a comment at the top describing what's being tested
4. End with `bye` to exit the interpreter
5. Run `./run_tests.sh` to verify
