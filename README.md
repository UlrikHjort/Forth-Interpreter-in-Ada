# Forth Interpreter in Ada

<p align="center">
  <img src="https://img.shields.io/badge/Ada-2012-blue.svg" alt="Ada 2012">
  <img src="https://img.shields.io/badge/Forth-000000.svg?style=for-the-badge&logo=forth&logoColor=white" alt="Forth">
  <img src="https://img.shields.io/badge/license-MIT-green.svg" alt="MIT License">
</p>


A **[Forth](https://en.wikipedia.org/wiki/Forth_(programming_language))** interpreter implementation in Ada with full support for nested loops, conditionals, variables, file loading, and an enhanced interactive REPL with arrow key history.


## Features

### Core Language Features
- Word definitions with `:` and `;`  
- Variables and constants  
- Control flow (IF/THEN/ELSE)  
- Loops (DO/LOOP, BEGIN/UNTIL, BEGIN/WHILE/REPEAT)  
- **Nested DO loops fully supported**  
- **IF/THEN inside DO loops**  
- Arithmetic operations (40+ operators)  
- Bitwise operations (AND, OR, XOR, NOT)  
- Stack manipulation (12+ words)  
- String literals with `." ... "`  
- Comments with `\` and `( )`  
- Recursion support  
- 4096 cells of memory  
- **Floating-point support** (25 FP words, separate FP stack)  

### Interactive REPL Features
- **Arrow key history** - Navigate previous commands  
- **Backspace editing** - Edit current line  
- **Persistent history** - Saved to `~/.forth_history`  
- **Ctrl+D** - Clean exit  
- **`.S`** - Show stack contents  
- **`STACK-ON/OFF`** - Toggle automatic stack display  
- **`WORDS`** - List all defined words  
- **`HELP`** - Show available commands  

### File Loading 
- **`LOAD filename`** - Execute Forth code from files  
- **`INCLUDE filename`** - Alternative to LOAD  
- **Error reporting** - Shows filename and line number on errors  


## REPL Features

The interpreter includes a powerful interactive REPL with **built-in line editing**:

- **Arrow keys** - Navigate command history (no rlwrap needed!)
- **Backspace** - Edit current line
- **Ctrl+D** - Exit (on empty line)
- **`.S`** - Show stack contents
- **`STACK-ON/OFF`** - Toggle automatic stack display
- **`WORDS`** - List all defined words
- **`HELP`** - Show available commands
- **`LOAD filename`** - Execute Forth code from a file

**Command history** is automatically saved to `~/.forth_history` and persists across sessions.

**No external dependencies required!** The interpreter includes built-in terminal handling with proper echo control and line feed support.

## Quick Start

### Build

```bash
make
```

Or manually:
```bash
gprbuild -P forth.gpr
```

### Run

```bash
./bin/forth
```

### Try Some Examples

```forth
> LOAD examples/hello.fth
 Hello, Forth World!
 Welcome to the interpreter!
  ok
  Loaded: examples/hello.fth

> 5 5 + .
 10

> : SQUARE DUP * ;
  ok

> 7 SQUARE .
 49

> bye
```

### Load Example Files

```bash
./bin/forth
> LOAD examples/hello.fth      # Beginner introduction
> LOAD examples/factorial.fth   # Factorials
> LOAD examples/fibonacci.fth   # Fibonacci sequence
> LOAD examples/gcd.fth         # Greatest common divisor
> LOAD examples/hanoi.fth       # Towers of Hanoi
```



## Testing

Run the comprehensive test suite:
```bash
make test
```

Or directly:
```bash
./run_tests.sh
```

## Example Session

```forth
Minimal Forth Interpreter
Type 'bye' to exit
Use ':' and ';' to define words
Commands: .S (show stack), WORDS (list words), STACK-ON/STACK-OFF

> 5 5 + .
 10

> : DOUBLE 2 * ;
  ok

> 21 DOUBLE .
 42

> STACK-ON
  Stack display enabled

 <1> 42 > DROP

 <0> > 10 20 30

 <3> 10 20 30 > .S
<3> 10 20 30

 <3> 10 20 30 > + +
 <1> 60 > .
 60

> LOAD examples/factorial.fth
  ok
  Loaded: examples/factorial.fth

> bye
```

## Notes & Limitations

### File Loading (LOAD/INCLUDE)

**What Works:**
- Single-line word definitions
- Simple commands and calculations
- Sequential execution of statements
- Loading from relative or absolute paths
- Error reporting with line numbers

**Current Limitation:**
- **Multi-line word definitions don't work with LOAD**
- LOAD processes files line-by-line without tracking definition state across lines

**Workaround for multi-line definitions:**
```bash
# Use input redirection instead:
./bin/forth < examples/complex-file.fth
```

**Example of what works vs. what doesn't:**

```forth
\  Works with LOAD (single line):
: SQUARE DUP * ;
: GREET ." Hello!" CR ;

\  Doesn't work with LOAD (multi-line):
: COMPLEX
   DUP 2 MOD 0= IF
      ." Even" CR
   THEN ;

\  Use piping for multi-line: ./bin/forth < file.fth
```

