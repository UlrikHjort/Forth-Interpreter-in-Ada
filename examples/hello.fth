\ Hello World - Simple introduction to Forth
\ Usage: LOAD examples/hello.fth

." Hello, Forth World!" CR
." Welcome to the interpreter!" CR

\ Define a simple greeting word
: GREET ." Hello from a Forth word!" CR ;

\ Test it
GREET

\ Some basic math
." 2 + 3 = " 2 3 + . CR
." 5 * 7 = " 5 7 * . CR

." Example complete!" CR
