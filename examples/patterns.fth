\ Star patterns

\ n --  : Emit n stars
: STARS
   DUP 0 SWAP DO 42 EMIT LOOP DROP
;

\ n --  : Print triangle
: TRIANGLE
   ." Triangle:" CR
   1 SWAP 1 + DO
      I STARS CR
   LOOP
;

\ n --  : Print pyramid
: PYRAMID
   ." Pyramid:" CR
   DUP 1 SWAP 1 + DO
      \ Spaces before stars
      DUP I - DUP 0 SWAP DO 32 EMIT LOOP DROP
      \ Stars
      I 2 * 1 - STARS CR
   LOOP
   DROP
;

\ n --  : Print square
: SQUARE
   ." Square:" CR
   DUP DUP 0 SWAP DO
      DUP STARS CR
   LOOP
   DROP
;

\ n --  : Print diamond
: DIAMOND
   ." Diamond:" CR
   \ Top half (including middle)
   DUP 1 SWAP 1 + DO
      DUP I - DUP 0 SWAP DO 32 EMIT LOOP DROP
      I 2 * 1 - STARS CR
   LOOP
   \ Bottom half
   1 - DUP 1 SWAP DO
      I DUP 0 SWAP DO 32 EMIT LOOP DROP
      DUP I - 2 * 1 - STARS CR
   LOOP
   DROP
;

5 TRIANGLE CR
5 PYRAMID CR
5 SQUARE CR
5 DIAMOND

bye
