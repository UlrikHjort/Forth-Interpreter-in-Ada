-- ***************************************************************************
--                      Forth -Stack
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

package Forth.Stack is

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   type Cell is range -(2**31) .. (2**31 - 1);

   --  Bitwise logical operations
   function Bit_And (A, B : Cell) return Cell;
   function Bit_Or (A, B : Cell) return Cell;
   function Bit_Xor (A, B : Cell) return Cell;
   function Bit_Not (A : Cell) return Cell;

   --  Push a value onto the stack
   procedure Push (Value : Cell);

   --  Pop and return the top value from the stack
   function Pop return Cell;

   --  Return the top value without removing it
   function Peek return Cell;

   --  Return stack item at given depth (1 = top)
   function Peek_At (Depth_From_Top : Positive) return Cell;

   --  Remove the top value from the stack
   procedure Drop;

   --  Duplicate the top stack value
   procedure Dup;

   --  Swap the top two stack values
   procedure Swap;

   --  Copy the second stack item to the top
   procedure Over;

   --  Rotate the third item to the top (a b c -- b c a)
   procedure Rot;

   --  Duplicate the top two items (a b -- a b a b)
   procedure TwoDup;

   --  Drop the top two items
   procedure TwoDrop;

   --  Swap the top two pairs (a b c d -- c d a b)
   procedure TwoSwap;

   --  Copy top item below second item (a b -- b a b)
   procedure Tuck;

   --  Remove second item (a b -- b)
   procedure Nip;

   --  Copy nth stack item to top (0 PICK = DUP, 1 PICK = OVER)
   procedure Pick;

   --  Return the current stack depth
   function Depth return Natural;

   --  Clear all values from the stack
   procedure Clear;

   --  Return stack for loops and control flow
   procedure RPush (Value : Cell);
   function RPop return Cell;
   function RPeek return Cell;
   function RDepth return Natural;

   --  Display the current stack contents
   procedure Display_Stack;

   --  Get stack display as a string
   function Get_Stack_Display return String;

end Forth.Stack;
