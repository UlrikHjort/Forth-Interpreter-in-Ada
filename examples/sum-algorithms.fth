\ Sum algorithms
\ Various ways to compute sums

\ n -- sum
: SUM-TO-N
   0 SWAP 1 SWAP 1 + DO I + LOOP
;

\ n -- sum
: SUM-SQUARES
   0 SWAP 1 SWAP 1 + DO I DUP * + LOOP
;

\ n -- sum
: SUM-EVENS
   0 SWAP 1 SWAP 1 + DO
      I 2 MOD 0= IF I + THEN
   LOOP
;

\ Fibonacci sum - sum of first N fibonacci numbers
VARIABLE FIB-A
VARIABLE FIB-B
VARIABLE FIB-SUM

\ n -- sum
: FIBONACCI-SUM
   0 FIB-SUM !
   1 FIB-A !
   1 FIB-B !
   
   1 SWAP DO
      FIB-A @ FIB-SUM @ + FIB-SUM !
      FIB-A @ FIB-B @ +
      FIB-A @ FIB-B !
      FIB-A !
   LOOP
   FIB-SUM @
;

." Sum Algorithms:" CR
CR
." Sum 1 to 10: " 10 SUM-TO-N . CR
." Sum 1 to 100: " 100 SUM-TO-N . CR
CR
." Sum of squares 1 to 10: " 10 SUM-SQUARES . CR
." Sum of squares 1 to 20: " 20 SUM-SQUARES . CR
CR
." Sum of evens 1 to 10: " 10 SUM-EVENS . CR
." Sum of evens 1 to 20: " 20 SUM-EVENS . CR
CR
." Sum of first 10 Fibonacci: " 10 FIBONACCI-SUM . CR

bye
