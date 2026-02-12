\ Game of Life utilities - Useful Forth programming patterns
\ Usage: LOAD examples/utilities.fth

\ ===== Math Utilities =====

: SQUARE ( n -- n² )
   DUP * ;

: CUBE ( n -- n³ )
   DUP DUP * * ;

: BETWEEN ( n low high -- flag )
   \ Check if n is between low and high (inclusive)
   ROT DUP ROT >= SWAP ROT <= AND ;

: CLAMP ( n min max -- n' )
   \ Clamp n between min and max
   ROT SWAP OVER MIN SWAP MAX ;

\ ===== Logic Utilities =====

: BOOL ( n -- flag )
   \ Convert any number to boolean (-1 or 0)
   0<> IF -1 ELSE 0 THEN ;

: NOT ( flag -- !flag )
   0= ;

\ ===== Stack Utilities =====

: 3DUP ( a b c -- a b c a b c )
   DUP 2OVER ROT ;

: ?DUP ( n -- n n | 0 )
   \ Duplicate if non-zero
   DUP IF DUP THEN ;

\ ===== Output Utilities =====

: SPACES ( n -- )
   \ Print n spaces
   0 DO 32 EMIT LOOP ;

: STARS ( n -- )
   \ Print n stars
   0 DO 42 EMIT LOOP ;

: .HEX ( n -- )
   \ Print number in hexadecimal (simplified)
   BASE @ SWAP 16 BASE ! . BASE ! ;

\ ===== Test the utilities =====
." Utilities loaded!" CR
." Testing SQUARE: 7 SQUARE = " 7 SQUARE . CR
." Testing CUBE: 3 CUBE = " 3 CUBE . CR
." Testing BETWEEN: 5 BETWEEN 1 10 = " 5 1 10 BETWEEN . CR
