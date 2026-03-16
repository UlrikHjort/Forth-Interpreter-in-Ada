-- ***************************************************************************
--                       Forth Stack
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

with Ada.Unchecked_Conversion;
with Ada.Text_IO;

package body Forth.Stack is

   Max_Stack_Size : constant := 1024;
   type Stack_Array is array (1 .. Max_Stack_Size) of Cell;

   Data : Stack_Array;
   SP   : Natural := 0;

   --  Return stack
   RData : Stack_Array;
   RSP   : Natural := 0;

   --  Modular type for bitwise operations
   type Unsigned_32 is mod 2**32;

   function To_Unsigned is new Ada.Unchecked_Conversion (Cell, Unsigned_32);
   function To_Cell is new Ada.Unchecked_Conversion (Unsigned_32, Cell);

   --  Bitwise logical operations
   function Bit_And (A, B : Cell) return Cell is
   begin
      return To_Cell (To_Unsigned (A) and To_Unsigned (B));
   end Bit_And;

   function Bit_Or (A, B : Cell) return Cell is
   begin
      return To_Cell (To_Unsigned (A) or To_Unsigned (B));
   end Bit_Or;

   function Bit_Xor (A, B : Cell) return Cell is
   begin
      return To_Cell (To_Unsigned (A) xor To_Unsigned (B));
   end Bit_Xor;

   function Bit_Not (A : Cell) return Cell is
   begin
      return To_Cell (not To_Unsigned (A));
   end Bit_Not;

   --  Push a value onto the stack
   procedure Push (Value : Cell) is
   begin
      if SP >= Max_Stack_Size then
         raise Stack_Overflow;
      end if;
      SP := SP + 1;
      Data (SP) := Value;
   end Push;

   --  Pop and return the top value from the stack
   function Pop return Cell is
      Value : Cell;
   begin
      if SP = 0 then
         raise Stack_Underflow;
      end if;
      Value := Data (SP);
      SP := SP - 1;
      return Value;
   end Pop;

   --  Return the top value without removing it
   function Peek return Cell is
   begin
      if SP = 0 then
         raise Stack_Underflow;
      end if;
      return Data (SP);
   end Peek;

   --  Return stack item at given depth (1 = top)
   function Peek_At (Depth_From_Top : Positive) return Cell is
   begin
      if Depth_From_Top > SP then
         raise Stack_Underflow;
      end if;
      return Data (SP - Depth_From_Top + 1);
   end Peek_At;

   --  Remove the top value from the stack
   procedure Drop is
      Dummy : Cell;
   begin
      Dummy := Pop;
   end Drop;

   --  Duplicate the top stack value
   procedure Dup is
   begin
      Push (Peek);
   end Dup;

   --  Swap the top two stack values
   procedure Swap is
      A, B : Cell;
   begin
      A := Pop;
      B := Pop;
      Push (A);
      Push (B);
   end Swap;

   --  Copy the second stack item to the top
   procedure Over is
      A, B : Cell;
   begin
      A := Pop;
      B := Pop;
      Push (B);
      Push (A);
      Push (B);
   end Over;

   --  Rotate the third item to the top (a b c -- b c a)
   procedure Rot is
      A, B, C : Cell;
   begin
      C := Pop;
      B := Pop;
      A := Pop;
      Push (B);
      Push (C);
      Push (A);
   end Rot;

   --  Duplicate the top two items (a b -- a b a b)
   procedure TwoDup is
      A, B : Cell;
   begin
      B := Pop;
      A := Pop;
      Push (A);
      Push (B);
      Push (A);
      Push (B);
   end TwoDup;

   --  Drop the top two items
   procedure TwoDrop is
      Dummy : Cell;
   begin
      Dummy := Pop;
      Dummy := Pop;
   end TwoDrop;

   --  Swap the top two pairs (a b c d -- c d a b)
   procedure TwoSwap is
      A, B, C, D : Cell;
   begin
      D := Pop;
      C := Pop;
      B := Pop;
      A := Pop;
      Push (C);
      Push (D);
      Push (A);
      Push (B);
   end TwoSwap;

   --  Copy top item below second item (a b -- b a b)
   procedure Tuck is
      A, B : Cell;
   begin
      B := Pop;
      A := Pop;
      Push (B);
      Push (A);
      Push (B);
   end Tuck;

   --  Remove second item (a b -- b)
   procedure Nip is
      A : Cell;
   begin
      A := Pop;
      Drop;
      Push (A);
   end Nip;

   --  Copy nth stack item to top (0 PICK = DUP, 1 PICK = OVER)
   procedure Pick is
      N : constant Cell := Pop;
      Index : constant Natural := Natural (N) + 1;
   begin
      if Index > SP then
         raise Stack_Underflow;
      end if;
      Push (Data (SP - Index + 1));
   end Pick;

   --  Return the current stack depth
   function Depth return Natural is
   begin
      return SP;
   end Depth;

   --  Clear all values from the stack
   procedure Clear is
   begin
      SP := 0;
   end Clear;

   --  Push a value onto the return stack
   procedure RPush (Value : Cell) is
   begin
      if RSP >= Max_Stack_Size then
         raise Stack_Overflow;
      end if;
      RSP := RSP + 1;
      RData (RSP) := Value;
   end RPush;

   --  Pop a value from the return stack
   function RPop return Cell is
      Value : Cell;
   begin
      if RSP = 0 then
         raise Stack_Underflow;
      end if;
      Value := RData (RSP);
      RSP := RSP - 1;
      return Value;
   end RPop;

   --  Peek at top of return stack
   function RPeek return Cell is
   begin
      if RSP = 0 then
         raise Stack_Underflow;
      end if;
      return RData (RSP);
   end RPeek;

   --  Return the current return stack depth
   function RDepth return Natural is
   begin
      return RSP;
   end RDepth;

   --  Display the current stack contents
   procedure Display_Stack is
   begin
      if SP = 0 then
         Ada.Text_IO.Put ("<empty>");
      else
         Ada.Text_IO.Put ("<" & Natural'Image (SP) & ">");
         for I in 1 .. SP loop
            Ada.Text_IO.Put (Cell'Image (Data (I)));
         end loop;
      end if;
   end Display_Stack;

   --  Get stack display as a string
   function Get_Stack_Display return String is
      Result : String (1 .. 256);
      Last : Natural := 0;
      
      procedure Append (S : String) is
      begin
         if Last + S'Length <= Result'Last then
            Result (Last + 1 .. Last + S'Length) := S;
            Last := Last + S'Length;
         end if;
      end Append;
   begin
      if SP = 0 then
         return "<empty>";
      else
         Append ("<" & Natural'Image (SP) & ">");
         for I in 1 .. SP loop
            Append (Cell'Image (Data (I)));
         end loop;
         return Result (1 .. Last);
      end if;
   end Get_Stack_Display;

end Forth.Stack;
