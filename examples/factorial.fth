\ Factorial - Recursive implementation
\ Computes factorial of a number

\ n -- n!
: FACTORIAL
   DUP 1 = IF
      DROP 1
   ELSE
      DUP 1 - FACTORIAL *
   THEN
;

." Factorial examples:" CR
." 5! = " 5 FACTORIAL . CR
." 6! = " 6 FACTORIAL . CR
." 7! = " 7 FACTORIAL . CR
." 10! = " 10 FACTORIAL . CR

bye
