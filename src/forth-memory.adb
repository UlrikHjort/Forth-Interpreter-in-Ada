-- ***************************************************************************
--                      Forth Memory
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

with Forth.Stack;
use Forth.Stack;
with Ada.Unchecked_Conversion;

package body Forth.Memory is

   Memory_Size : constant := 4096;
   type Memory_Array is array (0 .. Memory_Size - 1) of Forth.Stack.Cell;

   Memory : Memory_Array := (others => 0);
   Memory_Pointer : Forth.Stack.Cell := 0;

   --  Store a value at an address
   procedure Store (Address : Forth.Stack.Cell; Value : Forth.Stack.Cell) is
   begin
      if Address < 0 or Address >= Forth.Stack.Cell (Memory_Size) then
         raise Invalid_Address;
      end if;
      Memory (Integer (Address)) := Value;
   end Store;

   --  Fetch a value from an address
   function Fetch (Address : Forth.Stack.Cell) return Forth.Stack.Cell is
   begin
      if Address < 0 or Address >= Forth.Stack.Cell (Memory_Size) then
         raise Invalid_Address;
      end if;
      return Memory (Integer (Address));
   end Fetch;

   --  Allocate memory and return address
   function Allot (Cells : Natural) return Forth.Stack.Cell is
      Old_Pointer : constant Forth.Stack.Cell := Memory_Pointer;
   begin
      Memory_Pointer := Memory_Pointer + Forth.Stack.Cell (Cells);
      if Memory_Pointer >= Forth.Stack.Cell (Memory_Size) then
         raise Invalid_Address with "Memory exhausted";
      end if;
      return Old_Pointer;
   end Allot;

   --  Get current memory pointer
   function Here return Forth.Stack.Cell is
   begin
      return Memory_Pointer;
   end Here;

   --  Clear all memory
   procedure Clear is
   begin
      Memory_Pointer := 0;
      Memory := (others => 0);
   end Clear;

end Forth.Memory;
