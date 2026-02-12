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

package body Forth.FStack is

   Max_FStack_Size : constant := 1024;
   type FStack_Array is array (1 .. Max_FStack_Size) of FCell;

   FData : FStack_Array;
   FSP   : Natural := 0;

   --  Push a value onto the floating-point stack
   procedure FPush (Value : FCell) is
   begin
      if FSP >= Max_FStack_Size then
         raise FStack_Overflow;
      end if;
      FSP := FSP + 1;
      FData (FSP) := Value;
   end FPush;

   --  Pop and return the top value from the floating-point stack
   function FPop return FCell is
   begin
      if FSP = 0 then
         raise FStack_Underflow;
      end if;
      declare
         Value : constant FCell := FData (FSP);
      begin
         FSP := FSP - 1;
         return Value;
      end;
   end FPop;

   --  Return the top value without removing it
   function FPeek return FCell is
   begin
      if FSP = 0 then
         raise FStack_Underflow;
      end if;
      return FData (FSP);
   end FPeek;

   --  Remove the top value from the floating-point stack
   procedure FDrop is
      Dummy : FCell;
   begin
      Dummy := FPop;
   end FDrop;

   --  Duplicate the top stack value
   procedure FDup is
      Value : constant FCell := FPeek;
   begin
      FPush (Value);
   end FDup;

   --  Swap the top two stack values
   procedure FSwap is
      A, B : FCell;
   begin
      B := FPop;
      A := FPop;
      FPush (B);
      FPush (A);
   end FSwap;

   --  Copy the second stack item to the top
   procedure FOver is
      A, B : FCell;
   begin
      B := FPop;
      A := FPeek;
      FPush (B);
      FPush (A);
   end FOver;

   --  Rotate the third item to the top (a b c -- b c a)
   procedure FRot is
      A, B, C : FCell;
   begin
      C := FPop;
      B := FPop;
      A := FPop;
      FPush (B);
      FPush (C);
      FPush (A);
   end FRot;

   --  Duplicate the top two items (a b -- a b a b)
   procedure F2Dup is
      A, B : FCell;
   begin
      B := FPop;
      A := FPeek;
      FPush (B);
      FPush (A);
      FPush (B);
   end F2Dup;

   --  Drop the top two items
   procedure F2Drop is
      Dummy : FCell;
   begin
      Dummy := FPop;
      Dummy := FPop;
   end F2Drop;

   --  Return the current floating-point stack depth
   function FDepth return Natural is
   begin
      return FSP;
   end FDepth;

   --  Clear all values from the floating-point stack
   procedure FClear is
   begin
      FSP := 0;
   end FClear;

end Forth.FStack;
