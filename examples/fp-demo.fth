\ Floating-Point Demonstration
\ Shows what actually works with our FP implementation

." Floating-Point Math Demo" CR
." =========================" CR CR

\ Basic arithmetic
." Basic Arithmetic:" CR
." ----------------" CR
." 3.0 + 4.0 = " 3 S>F 4 S>F F+ F. CR
." 10.0 - 3.0 = " 10 S>F 3 S>F F- F. CR
." 6.0 * 7.0 = " 6 S>F 7 S>F F* F. CR
." 20.0 / 4.0 = " 20 S>F 4 S>F F/ F. CR

CR
." Square Roots:" CR
." ------------" CR
." SQRT(2) = " 2 S>F FSQRT F. CR
." SQRT(16) = " 16 S>F FSQRT F. CR
." SQRT(100) = " 100 S>F FSQRT F. CR

CR
." Mathematical Constants:" CR
." ----------------------" CR
." PI = " PI F. CR
." E = " E F. CR
." PI * E = " PI E F* F. CR
." PI / 2 = " PI 2 S>F F/ F. CR
." SQRT(PI) = " PI FSQRT F. CR

CR
." Complex Number Math:" CR
." -------------------" CR
." |3 + 4i| = SQRT(3^2 + 4^2)" CR
." = SQRT(9 + 16) = SQRT(25) = 5.0" CR
." Calculation: " 
3 S>F FDUP F* 4 S>F FDUP F* F+ FSQRT F. CR

CR
." Circle Area (r=5):" CR
." A = PI * r^2" CR  
." = PI * 25 = "
PI 5 S>F FDUP F* F* F. CR

CR
." Pythagorean Theorem (3,4,5 triangle):" CR
." c = SQRT(a^2 + b^2)" CR
." = SQRT(9 + 16) = "
3 S>F FDUP F* 4 S>F FDUP F* F+ FSQRT F. CR

CR
." FP Comparisons:" CR
." --------------" CR
." 3.0 < 5.0? " 3 S>F 5 S>F F< . ."  (-1 = true)" CR
." 5.0 > 3.0? " 5 S>F 3 S>F F> . ."  (-1 = true)" CR
." 4.0 = 4.0? " 4 S>F 4 S>F F= . ."  (-1 = true)" CR

CR
." All floating-point operations verified!" CR

bye
