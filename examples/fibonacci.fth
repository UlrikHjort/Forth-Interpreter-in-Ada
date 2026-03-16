\ Fibonacci sequence generator

VARIABLE FIB-A
VARIABLE FIB-B

\ n -- fib-n
: FIBONACCI-ITER
   DUP 0= IF
      DROP 0
   ELSE DUP 1 = IF
      DROP 1
   ELSE
      1 FIB-A !
      1 FIB-B !
      2 SWAP DO
         FIB-A @ FIB-B @ +
         FIB-A @ FIB-B !
         FIB-A !
      LOOP
      FIB-A @
   THEN THEN
;

\ n --
: FIBONACCI-SEQ
   ." First " DUP . ." Fibonacci numbers:" CR
   DUP 0 SWAP DO
      I FIBONACCI-ITER . 32 EMIT
   LOOP
   CR
;

15 FIBONACCI-SEQ
CR
." Fib(10) = " 10 FIBONACCI-ITER . CR
." Fib(15) = " 15 FIBONACCI-ITER . CR

bye
