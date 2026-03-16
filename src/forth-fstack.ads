-- ***************************************************************************
--                      Forth FStack
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

package Forth.FStack is

   FStack_Overflow  : exception;
   FStack_Underflow : exception;

   type FCell is new Long_Float;

   --  Push a value onto the floating-point stack
   procedure FPush (Value : FCell);

   --  Pop and return the top value from the floating-point stack
   function FPop return FCell;

   --  Return the top value without removing it
   function FPeek return FCell;

   --  Remove the top value from the floating-point stack
   procedure FDrop;

   --  Duplicate the top stack value
   procedure FDup;

   --  Swap the top two stack values
   procedure FSwap;

   --  Copy the second stack item to the top
   procedure FOver;

   --  Rotate the third item to the top (a b c -- b c a)
   procedure FRot;

   --  Duplicate the top two items (a b -- a b a b)
   procedure F2Dup;

   --  Drop the top two items
   procedure F2Drop;

   --  Return the current floating-point stack depth
   function FDepth return Natural;

   --  Clear all values from the floating-point stack
   procedure FClear;

end Forth.FStack;
