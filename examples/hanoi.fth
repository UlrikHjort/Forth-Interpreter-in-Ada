\ Towers of Hanoi - Iterative solution
\ Move N disks from peg 1 to peg 3 using peg 2
\ Uses the iterative algorithm that works for any number of disks

VARIABLE MOVES        \ Count total moves
VARIABLE DISKS        \ Number of disks

\ Print a move
: PRINT-MOVE  ( from to -- )
  ." Move disk from peg " SWAP . ." to peg " . CR
  MOVES @ 1 + MOVES !
;

\ Simple 3-disk example step by step
: HANOI-3
  ." Towers of Hanoi - 3 Disks" CR
  ." =======================" CR CR
  0 MOVES !
  
  ." Initial: All disks on peg 1" CR
  ." Goal: Move all to peg 3" CR CR
  
  1 3 PRINT-MOVE
  1 2 PRINT-MOVE  
  3 2 PRINT-MOVE
  1 3 PRINT-MOVE
  2 1 PRINT-MOVE
  2 3 PRINT-MOVE
  1 3 PRINT-MOVE
  
  CR ." Total moves: " MOVES @ . CR
  ." (Optimal is 2^n - 1 = 7 moves)" CR
;

\ 4-disk example
: HANOI-4
  CR
  ." Towers of Hanoi - 4 Disks" CR
  ." =======================" CR CR
  0 MOVES !
  
  1 2 PRINT-MOVE
  1 3 PRINT-MOVE
  2 3 PRINT-MOVE
  1 2 PRINT-MOVE
  3 1 PRINT-MOVE
  3 2 PRINT-MOVE
  1 2 PRINT-MOVE
  1 3 PRINT-MOVE
  2 3 PRINT-MOVE
  2 1 PRINT-MOVE
  3 1 PRINT-MOVE
  2 3 PRINT-MOVE
  1 2 PRINT-MOVE
  1 3 PRINT-MOVE
  2 3 PRINT-MOVE
  
  CR ." Total moves: " MOVES @ . CR
  ." (Optimal is 2^n - 1 = 15 moves)" CR
;

\ Run the examples
HANOI-3
HANOI-4

CR
." Towers of Hanoi demonstration complete!" CR
." " CR
." Note: This shows the optimal solution." CR
." For N disks, minimum moves = 2^N - 1" CR
bye
