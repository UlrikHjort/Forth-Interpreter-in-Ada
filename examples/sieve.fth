\ Sieve of Eratosthenes - Simplified version
\ Find primes up to 30 and display them

." Sieve of Eratosthenes - Simple Demo" CR
." =====================================" CR CR

\ Memory layout: address 1000-1030 = is_prime[0..30]
\ 1 = prime, 0 = composite

1000 CONSTANT BASE

\ Initialize: mark all as potential primes
: INIT
  31 0 DO
    1 I BASE + !
  LOOP
  \ 0 and 1 are not prime
  0 BASE !
  0 1 BASE + !
;

\ Mark multiples of 2
: MARK-2
  2 DUP * 31 SWAP DO
    0 I BASE + !
  2 +LOOP
;

\ Mark multiples of 3  
: MARK-3
  3 DUP * 31 SWAP DO
    0 I BASE + !
  3 +LOOP
;

\ Mark multiples of 5
: MARK-5
  5 DUP * 31 SWAP DO
    0 I BASE + !
  5 +LOOP
;

\ Print all primes
: SHOW
  ." Primes up to 30:" CR
  31 0 DO
    I BASE + @ 1 = IF
      I .
    THEN
  LOOP
  CR
;

\ Run the sieve
INIT
MARK-2
MARK-3
MARK-5
SHOW

CR
." Done!" CR
bye
