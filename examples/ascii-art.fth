\ ASCII Art Generator
\ Usage: LOAD examples/ascii-art.fth

: LINE ( -- )
   \ Draw a horizontal line
   40 0 DO 45 EMIT LOOP CR ;

: BOX ( width height -- )
   \ Draw a box
   SWAP \ height width
   0 DO
      DUP 0 DO 42 EMIT LOOP CR
   LOOP
   DROP ;

: TRIANGLE ( size -- )
   \ Draw a triangle
   1 SWAP 1 + 1 DO
      DUP 0 DO 42 EMIT LOOP CR
      1 +
   LOOP
   DROP ;

: DIAMOND ( size -- )
   \ Draw a diamond shape
   \ Top half
   1 SWAP DUP 1 + 1 DO
      DUP I - SPACES
      I 0 DO 42 EMIT 32 EMIT LOOP CR
   LOOP
   \ Bottom half
   1 - 1 SWAP DO
      I SPACES
      DUP I - 0 DO 42 EMIT 32 EMIT LOOP CR
   -1 +LOOP
   DROP ;

: CHECKERBOARD ( size -- )
   \ Draw a checkerboard pattern
   0 DO
      I 2 MOD 0= IF
         ." # # # # " CR
      ELSE
         ."  # # # #" CR
      THEN
   LOOP ;

\ Display examples
." ASCII Art Gallery" CR
." =================" CR CR

." Box (5x3):" CR
5 3 BOX CR

." Triangle (5):" CR
5 TRIANGLE CR

." Diamond (4):" CR
4 DIAMOND CR

." Checkerboard (8):" CR
8 CHECKERBOARD CR
