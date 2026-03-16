\ Simple Math Examples
\ Usage: LOAD examples/stats.fth

\ Calculate factorial
: FACT ( n -- n! )
   DUP 1 <= IF
      DROP 1
   ELSE
      DUP 1 - FACT *
   THEN ;

\ Calculate power
: POW ( base exp -- result )
   DUP 0= IF
      2DROP 1
   ELSE
      1 SWAP 0 DO
         OVER *
      LOOP
      NIP
   THEN ;

\ Demonstrate
." Math Examples" CR
." ==============" CR CR

." Factorials:" CR
." 5! = " 5 FACT . CR
." 7! = " 7 FACT . CR
." 10! = " 10 FACT . CR

CR
." Powers:" CR
." 2^8 = " 2 8 POW . CR
." 3^4 = " 3 4 POW . CR  
." 5^3 = " 5 3 POW . CR
." 10^2 = " 10 2 POW . CR
