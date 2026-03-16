\ Collatz Conjecture - Famous number sequence
\ Usage: LOAD examples/collatz.fth
\ The Collatz conjecture: start with any positive integer n
\ If n is even, divide by 2; if odd, multiply by 3 and add 1
\ Repeat until reaching 1. Conjecture: always reaches 1!

: COLLATZ-STEP ( n -- n' )
   DUP 2 MOD 0= IF
      \ Even: divide by 2
      2 /
   ELSE
      \ Odd: multiply by 3 and add 1
      3 * 1 +
   THEN ;

: COLLATZ-PRINT ( n -- )
   \ Print the Collatz sequence for n
   BEGIN
      DUP . 32 EMIT  \ Print with space
      DUP 1 =        \ Check if we've reached 1
   UNTIL
   DROP CR ;

: COLLATZ-LENGTH ( n -- length )
   \ Calculate the stopping time (length of sequence)
   0 SWAP           \ counter on stack
   BEGIN
      SWAP 1 + SWAP \ Increment counter
      DUP 1 =       \ Check if done
      IF
         DROP       \ Remove the 1
         EXIT       \ Return with length on stack
      THEN
      COLLATZ-STEP
   UNTIL
   DROP ;

\ Test with various numbers
." Collatz Conjecture Examples" CR
." ============================" CR CR

." Sequence for 7: " 7 COLLATZ-PRINT
." Sequence for 13: " 13 COLLATZ-PRINT
." Sequence for 27: " 27 COLLATZ-PRINT

CR
." Stopping times:" CR
." 7 -> " 7 COLLATZ-LENGTH . ." steps" CR
." 13 -> " 13 COLLATZ-LENGTH . ." steps" CR
." 27 -> " 27 COLLATZ-LENGTH . ." steps" CR
