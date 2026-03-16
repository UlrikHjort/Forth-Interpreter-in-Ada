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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Forth.Stack;

package body Forth.Dictionary is

   use Ada.Strings.Unbounded;

   Max_Words : constant := 256;
   Max_Name_Length : constant := 32;

   type Entry_Type is (Word_Entry, Variable_Entry, Constant_Entry);

   type Dictionary_Entry is record
      Name       : Unbounded_String;
      Definition : Unbounded_String;
      Kind       : Entry_Type := Word_Entry;
      Address    : Forth.Stack.Cell := 0;
      Value      : Forth.Stack.Cell := 0;
      In_Use     : Boolean := False;
   end record;

   type Dictionary_Array is array (1 .. Max_Words) of Dictionary_Entry;

   Dict       : Dictionary_Array;
   Word_Count : Natural := 0;

   --  Find the index of a word in the dictionary
   function Find_Word (Name : String) return Natural is
   begin
      for I in 1 .. Word_Count loop
         if Dict (I).In_Use and then To_String (Dict (I).Name) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Word;

   --  Define a new word in the dictionary
   procedure Define (Name : String; Definition : String) is
      Index : Natural := Find_Word (Name);
   begin
      if Index > 0 then
         Dict (Index).Definition := To_Unbounded_String (Definition);
      else
         if Word_Count >= Max_Words then
            raise Constraint_Error with "Dictionary full";
         end if;
         Word_Count := Word_Count + 1;
         Dict (Word_Count).Name := To_Unbounded_String (Name);
         Dict (Word_Count).Definition := To_Unbounded_String (Definition);
         Dict (Word_Count).Kind := Word_Entry;
         Dict (Word_Count).In_Use := True;
      end if;
   end Define;

   --  Define a variable (returns address)
   procedure Define_Variable (Name : String; Address : Forth.Stack.Cell) is
      Index : Natural := Find_Word (Name);
   begin
      if Index > 0 then
         Dict (Index).Kind := Variable_Entry;
         Dict (Index).Address := Address;
      else
         if Word_Count >= Max_Words then
            raise Constraint_Error with "Dictionary full";
         end if;
         Word_Count := Word_Count + 1;
         Dict (Word_Count).Name := To_Unbounded_String (Name);
         Dict (Word_Count).Kind := Variable_Entry;
         Dict (Word_Count).Address := Address;
         Dict (Word_Count).In_Use := True;
      end if;
   end Define_Variable;

   --  Define a constant (stores value)
   procedure Define_Constant (Name : String; Value : Forth.Stack.Cell) is
      Index : Natural := Find_Word (Name);
   begin
      if Index > 0 then
         Dict (Index).Kind := Constant_Entry;
         Dict (Index).Value := Value;
      else
         if Word_Count >= Max_Words then
            raise Constraint_Error with "Dictionary full";
         end if;
         Word_Count := Word_Count + 1;
         Dict (Word_Count).Name := To_Unbounded_String (Name);
         Dict (Word_Count).Kind := Constant_Entry;
         Dict (Word_Count).Value := Value;
         Dict (Word_Count).In_Use := True;
      end if;
   end Define_Constant;

   --  Check if a word exists in the dictionary
   function Exists (Name : String) return Boolean is
   begin
      return Find_Word (Name) /= 0;
   end Exists;

   --  Check if a word is a variable
   function Is_Variable (Name : String) return Boolean is
      Index : constant Natural := Find_Word (Name);
   begin
      return Index /= 0 and then Dict (Index).Kind = Variable_Entry;
   end Is_Variable;

   --  Check if a word is a constant
   function Is_Constant (Name : String) return Boolean is
      Index : constant Natural := Find_Word (Name);
   begin
      return Index /= 0 and then Dict (Index).Kind = Constant_Entry;
   end Is_Constant;

   --  Get the definition of a word
   function Get_Definition (Name : String) return String is
      Index : constant Natural := Find_Word (Name);
   begin
      if Index = 0 then
         raise Word_Not_Found;
      end if;
      return To_String (Dict (Index).Definition);
   end Get_Definition;

   --  Get variable address
   function Get_Variable_Address (Name : String) return Forth.Stack.Cell is
      Index : constant Natural := Find_Word (Name);
   begin
      if Index = 0 or else Dict (Index).Kind /= Variable_Entry then
         raise Word_Not_Found;
      end if;
      return Dict (Index).Address;
   end Get_Variable_Address;

   --  Get constant value
   function Get_Constant_Value (Name : String) return Forth.Stack.Cell is
      Index : constant Natural := Find_Word (Name);
   begin
      if Index = 0 or else Dict (Index).Kind /= Constant_Entry then
         raise Word_Not_Found;
      end if;
      return Dict (Index).Value;
   end Get_Constant_Value;

   --  Clear all user-defined words
   procedure Clear is
   begin
      Word_Count := 0;
      for I in Dict'Range loop
         Dict (I).In_Use := False;
      end loop;
   end Clear;

   --  List all defined words
   procedure List_Words is
   begin
      if Word_Count = 0 then
         Ada.Text_IO.Put_Line ("No user-defined words");
         return;
      end if;

      Ada.Text_IO.Put_Line ("Defined words:");
      for I in 1 .. Word_Count loop
         if Dict (I).In_Use then
            Ada.Text_IO.Put_Line ("  " & To_String (Dict (I).Name));
         end if;
      end loop;
   end List_Words;

   --  Get words matching prefix for tab completion
   procedure Get_Matching_Words (Prefix : String; Names : out String; Last : out Natural) is
      Upper_Prefix : String := Prefix;
      Match_Count : Natural := 0;
   begin
      Last := 0;
      
      -- Convert prefix to uppercase for case-insensitive matching
      for I in Upper_Prefix'Range loop
         if Upper_Prefix (I) >= 'a' and Upper_Prefix (I) <= 'z' then
            Upper_Prefix (I) := Character'Val (Character'Pos (Upper_Prefix (I)) - 32);
         end if;
      end loop;
      
      -- Find all matching words
      for I in 1 .. Word_Count loop
         if Dict (I).In_Use then
            declare
               Word_Name : constant String := To_String (Dict (I).Name);
               Upper_Name : String := Word_Name;
            begin
               -- Convert to uppercase
               for J in Upper_Name'Range loop
                  if Upper_Name (J) >= 'a' and Upper_Name (J) <= 'z' then
                     Upper_Name (J) := Character'Val (Character'Pos (Upper_Name (J)) - 32);
                  end if;
               end loop;
               
               -- Check if it matches prefix
               if Upper_Name'Length >= Upper_Prefix'Length and then
                  Upper_Name (Upper_Name'First .. Upper_Name'First + Upper_Prefix'Length - 1) = Upper_Prefix
               then
                  -- Add to result
                  if Last > 0 and then Last + 1 + Word_Name'Length <= Names'Last then
                     Last := Last + 1;
                     Names (Last) := ' ';
                  end if;
                  
                  if Last + Word_Name'Length <= Names'Last then
                     Names (Last + 1 .. Last + Word_Name'Length) := Word_Name;
                     Last := Last + Word_Name'Length;
                     Match_Count := Match_Count + 1;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Get_Matching_Words;

end Forth.Dictionary;
