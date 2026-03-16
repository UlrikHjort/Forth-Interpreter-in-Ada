-- ***************************************************************************
--                      Forth Dictionary
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

package Forth.Dictionary is

   Word_Not_Found : exception;

   --  Define a new word in the dictionary
   procedure Define (Name : String; Definition : String);

   --  Define a variable (returns address)
   procedure Define_Variable (Name : String; Address : Forth.Stack.Cell);

   --  Define a constant (stores value)
   procedure Define_Constant (Name : String; Value : Forth.Stack.Cell);

   --  Check if a word exists in the dictionary
   function Exists (Name : String) return Boolean;

   --  Check if a word is a variable
   function Is_Variable (Name : String) return Boolean;

   --  Check if a word is a constant
   function Is_Constant (Name : String) return Boolean;

   --  Get the definition of a word
   function Get_Definition (Name : String) return String;

   --  Get variable address
   function Get_Variable_Address (Name : String) return Forth.Stack.Cell;

   --  Get constant value
   function Get_Constant_Value (Name : String) return Forth.Stack.Cell;

   --  Clear all user-defined words
   procedure Clear;

   --  List all defined words
   procedure List_Words;

   --  Get words matching prefix for tab completion
   --  Returns space-separated list of matching words
   procedure Get_Matching_Words (Prefix : String; Names : out String; Last : out Natural);

end Forth.Dictionary;
