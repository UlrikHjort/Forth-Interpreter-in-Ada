\ Greatest Common Divisor using Euclidean algorithm

\ a b -- gcd
: GCD
   DUP 0= IF
      DROP
   ELSE
      SWAP OVER MOD GCD
   THEN
;

\ a b -- lcm
: LCM
   2DUP * ROT ROT GCD /
;

." GCD and LCM examples:" CR
CR
." GCD(48, 18) = " 48 18 GCD . CR
." GCD(100, 35) = " 100 35 GCD . CR
." GCD(17, 19) = " 17 19 GCD . CR
CR
." LCM(12, 18) = " 12 18 LCM . CR
." LCM(4, 6) = " 4 6 LCM . CR

bye
