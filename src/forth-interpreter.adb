-- ***************************************************************************
--                      Forth Interpreter
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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Directories;
with Interfaces.C;
with Forth.Stack;
with Forth.FStack;
with Forth.Dictionary;
with Forth.Memory;
with Forth.Readline;
use Forth.Stack;
use Forth.FStack;

package body Forth.Interpreter is

   --  Import isatty from C library
   function isatty (fd : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, isatty, "isatty");
   
   function Is_Terminal return Boolean is
      use Interfaces.C;
   begin
      return isatty (0) /= 0;  -- 0 is stdin file descriptor
   end Is_Terminal;

   Compiling      : Boolean := False;
   New_Word_Name  : String (1 .. 32);
   New_Word_Len   : Natural := 0;
   New_Word_Def   : String (1 .. 1024);
   New_Word_Def_Len : Natural := 0;

   --  Execute a primitive word or number
   procedure Execute_Primitive (Word : String);

   --  Load and execute a Forth file
   procedure Load_File (Filename : String) is
      use Ada.Text_IO;
      File : File_Type;
      Line_Num : Natural := 0;
   begin
      if not Ada.Directories.Exists (Filename) then
         Put_Line ("Error: File not found: " & Filename);
         return;
      end if;
      
      Open (File, In_File, Filename);
      
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Line_Num := Line_Num + 1;
            if Line'Length > 0 then
               begin
                  Eval (Line);
               exception
                  when E : others =>
                     Put_Line ("Error in " & Filename & " line" & Natural'Image (Line_Num) & 
                              ": " & Ada.Exceptions.Exception_Message (E));
                     Close (File);
                     raise;
               end;
            end if;
         end;
      end loop;
      
      Close (File);
      Put_Line ("  Loaded: " & Filename);
   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("Error: Cannot open file: " & Filename);
      when E : others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Put_Line ("Error loading " & Filename & ": " & Ada.Exceptions.Exception_Message (E));
   end Load_File;

   --  Find matching LOOP or +LOOP for a DO, accounting for nesting
   function Find_Matching_Loop (Input : String; Start_Pos : Positive) return Natural is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      Pos : Positive := Start_Pos;
      Token_Start, Token_Stop : Natural;
      Nesting_Level : Natural := 1;
   begin
      while Pos <= Input'Last loop
         Find_Token (Input (Pos .. Input'Last),
                    To_Set (" " & ASCII.HT),
                    Ada.Strings.Outside, Token_Start, Token_Stop);
         exit when Token_Start > Token_Stop;
         
         declare
            Word : constant String := Input (Token_Start .. Token_Stop);
         begin
            if Word = "DO" or Word = "do" then
               Nesting_Level := Nesting_Level + 1;
            elsif (Word = "LOOP" or Word = "loop" or Word = "+LOOP" or Word = "+loop") then
               Nesting_Level := Nesting_Level - 1;
               if Nesting_Level = 0 then
                  return Token_Start;
               end if;
            end if;
         end;
         
         Pos := Token_Stop + 1;
      end loop;
      
      return 0;  --  Not found
   end Find_Matching_Loop;

   --  Process conditional: skip to ELSE or THEN
   function Skip_To_Else_Or_Then (Definition : String; 
                                   Start_Pos : Positive;
                                   Found_Else : out Boolean) return Natural is
      use Ada.Strings.Fixed;
      Depth : Natural := 1;
      Start : Positive := Start_Pos;
      Stop : Natural;
   begin
      Found_Else := False;
      while Start <= Definition'Last loop
         Find_Token (Definition (Start .. Definition'Last),
                     Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                     Ada.Strings.Outside, Start, Stop);
         exit when Start > Stop;

         declare
            Word : constant String := Definition (Start .. Stop);
         begin
            if (Word = "IF" or Word = "if") then
               Depth := Depth + 1;
            elsif (Word = "THEN" or Word = "then") then
               Depth := Depth - 1;
               if Depth = 0 then
                  return Stop + 1;
               end if;
            elsif (Word = "ELSE" or Word = "else") and Depth = 1 then
               Found_Else := True;
               return Stop + 1;
            end if;
         end;

         Start := Stop + 1;
      end loop;
      return Definition'Last + 1;
   end Skip_To_Else_Or_Then;

   --  Find the start of a loop construct
   function Find_Loop_Start (Definition : String; End_Pos : Positive; 
                             Loop_Type : out String) return Natural;

   function Find_Loop_Start (Definition : String; End_Pos : Positive;
                             Loop_Type : out String) return Natural is
      use Ada.Strings.Fixed;
      Depth : Natural := 1;
      Start : Positive := End_Pos;
      Stop : Natural;
   begin
      while Start > Definition'First loop
         Start := Start - 1;
         if Start < Definition'First then
            return 0;
         end if;
         
         --  Find previous word
         while Start > Definition'First and then 
               (Definition (Start) = ' ' or Definition (Start) = ASCII.HT) loop
            Start := Start - 1;
         end loop;
         
         Stop := Start;
         while Start > Definition'First and then 
               (Definition (Start - 1) /= ' ' and 
                Definition (Start - 1) /= ASCII.HT) loop
            Start := Start - 1;
         end loop;
         
         declare
            Word : constant String := Definition (Start .. Stop);
         begin
            if (Word = "UNTIL" or Word = "until") or 
               (Word = "REPEAT" or Word = "repeat") or
               (Word = "LOOP" or Word = "loop") or
               (Word = "+LOOP" or Word = "+loop") then
               Depth := Depth + 1;
            elsif (Word = "BEGIN" or Word = "begin") then
               Depth := Depth - 1;
               if Depth = 0 then
                  Loop_Type := "BEGIN";
                  return Start;
               end if;
            elsif (Word = "DO" or Word = "do") then
               Depth := Depth - 1;
               if Depth = 0 then
                  Loop_Type := "DO";
                  return Start;
               end if;
            end if;
         end;
      end loop;
      return 0;
   end Find_Loop_Start;

   --  Execute with conditional support
   procedure Eval_With_Conditionals (Input : String);

   procedure Eval_With_Conditionals (Input : String) is
      use Ada.Strings.Fixed;
      Start : Positive := Input'First;
      Stop : Natural;
      Quote_Pos : Natural;
      Quote_End : Natural;
   begin
      --  First, check for and handle any string literals
      Quote_Pos := Index (Input, ".""");
      if Quote_Pos > 0 then
         --  Process everything before the string literal
         if Quote_Pos > Input'First then
            Eval_With_Conditionals (Input (Input'First .. Quote_Pos - 1));
         end if;
         
         --  Find the closing quote
         Quote_End := Index (Input (Quote_Pos + 2 .. Input'Last), """");
         if Quote_End > 0 then
            --  Print the string
            Ada.Text_IO.Put (Input (Quote_Pos + 2 .. Quote_End - 1));
            --  Process everything after the string literal
            if Quote_End + 1 <= Input'Last then
               Eval_With_Conditionals (Input (Quote_End + 1 .. Input'Last));
            end if;
         else
            Ada.Text_IO.Put_Line ("Error: Unclosed string literal");
         end if;
         return;
      end if;

      --  No string literals, process normally
      while Start <= Input'Last loop
         Find_Token (Input (Start .. Input'Last),
                     Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                     Ada.Strings.Outside, Start, Stop);
         exit when Start > Stop;

         declare
            Word : constant String := Input (Start .. Stop);
         begin
            if (Word = "IF" or Word = "if") then
               declare
                  Condition : constant Stack.Cell := Stack.Pop;
                  Found_Else : Boolean;
                  Next_Pos : Natural;
               begin
                  if Condition /= 0 then
                     Start := Stop + 1;
                  else
                     Next_Pos := Skip_To_Else_Or_Then (Input, Stop + 1, Found_Else);
                     if Found_Else then
                        Start := Next_Pos;
                     else
                        Next_Pos := Skip_To_Else_Or_Then (Input (Next_Pos .. Input'Last) , Next_Pos, Found_Else);
                        Start := Next_Pos;
                     end if;
                  end if;
               end;
            elsif (Word = "ELSE" or Word = "else") then
               declare
                  Found_Else : Boolean;
                  Next_Pos : Natural := Skip_To_Else_Or_Then (Input, Stop + 1, Found_Else);
               begin
                  Start := Next_Pos;
               end;
            elsif (Word = "THEN" or Word = "then") then
               Start := Stop + 1;
            elsif (Word = "BEGIN" or Word = "begin") then
               --  Mark the position after BEGIN for looping back
               declare
                  Loop_Start : constant Positive := Stop + 1;
                  Continue_Loop : Boolean := True;
               begin
                  while Continue_Loop loop
                     declare
                        Loop_Body_Start : Positive := Loop_Start;
                        Loop_Body_Stop : Natural;
                        Found_Until : Boolean := False;
                        Found_While : Boolean := False;
                        Found_Repeat : Boolean := False;
                     begin
                        --  Execute until we find UNTIL, WHILE, or REPEAT
                        while Loop_Body_Start <= Input'Last loop
                           Find_Token (Input (Loop_Body_Start .. Input'Last),
                                      Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                                      Ada.Strings.Outside, Loop_Body_Start, Loop_Body_Stop);
                           exit when Loop_Body_Start > Loop_Body_Stop;

                           declare
                              Loop_Word : constant String := Input (Loop_Body_Start .. Loop_Body_Stop);
                           begin
                              if (Loop_Word = "UNTIL" or Loop_Word = "until") then
                                 Found_Until := True;
                                 Start := Loop_Body_Stop + 1;
                                 Continue_Loop := (Stack.Pop = 0);
                                 exit;
                              elsif (Loop_Word = "WHILE" or Loop_Word = "while") then
                                 Found_While := True;
                                 Continue_Loop := (Stack.Pop /= 0);
                                 if not Continue_Loop then
                                    --  Skip to REPEAT
                                    while Loop_Body_Start <= Input'Last loop
                                       Find_Token (Input (Loop_Body_Start .. Input'Last),
                                                  Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                                                  Ada.Strings.Outside, Loop_Body_Start, Loop_Body_Stop);
                                       exit when Loop_Body_Start > Loop_Body_Stop;
                                       if (Input (Loop_Body_Start .. Loop_Body_Stop) = "REPEAT" or
                                           Input (Loop_Body_Start .. Loop_Body_Stop) = "repeat") then
                                          Start := Loop_Body_Stop + 1;
                                          exit;
                                       end if;
                                       Loop_Body_Start := Loop_Body_Stop + 1;
                                    end loop;
                                    exit;
                                 end if;
                              elsif (Loop_Word = "REPEAT" or Loop_Word = "repeat") then
                                 Found_Repeat := True;
                                 Start := Loop_Body_Stop + 1;
                                 --  Loop continues (back to BEGIN)
                                 exit;
                              else
                                 Execute_Primitive (Loop_Word);
                              end if;
                           end;

                           Loop_Body_Start := Loop_Body_Stop + 1;
                        end loop;

                        if not (Found_Until or Found_While or Found_Repeat) then
                           Ada.Text_IO.Put_Line ("Error: BEGIN without UNTIL/WHILE/REPEAT");
                           exit;
                        end if;
                     end;
                  end loop;
               end;
            elsif (Word = "DO" or Word = "do") then
               declare
                  Loop_Limit : constant Stack.Cell := Stack.Pop;  
                  Loop_Index : Stack.Cell := Stack.Pop;  
                  Loop_Start : constant Positive := Stop + 1;
                  Loop_End_Pos : constant Natural := Find_Matching_Loop (Input, Loop_Start);
                  Loop_Body : String (1 .. 2048);
                  Loop_Body_Len : Natural;
                  Saved_RDepth : constant Natural := Stack.RDepth;
                  Term_Start, Term_Stop : Natural;
               begin
                  if Loop_End_Pos = 0 then
                     Ada.Text_IO.Put_Line ("Error: DO without LOOP");
                     Start := Input'Last + 1;
                  else
                     --  Extract loop body (everything between DO and LOOP/+LOOP)
                     Loop_Body_Len := Loop_End_Pos - Loop_Start;
                     if Loop_Body_Len > 0 then
                        Loop_Body (1 .. Loop_Body_Len) := Input (Loop_Start .. Loop_End_Pos - 1);
                     end if;
                     
                     while Loop_Index < Loop_Limit loop
                        --  Push index for I to access
                        Stack.RPush (Loop_Index);
                        
                        --  Execute loop body with full conditional support
                        if Loop_Body_Len > 0 then
                           Eval_With_Conditionals (Loop_Body (1 .. Loop_Body_Len));
                        end if;
                        
                        --  Clean up any extra return stack entries from nested loops
                        while Stack.RDepth > Saved_RDepth + 1 loop
                           declare
                              Dummy : Stack.Cell := Stack.RPop;
                           begin
                              null;
                           end;
                        end loop;
                        
                        --  Pop our index
                        declare
                           Dummy : Stack.Cell := Stack.RPop;
                        begin
                           null;
                        end;
                        
                        --  Check what kind of loop terminator we have and update index
                        Find_Token (Input (Loop_End_Pos .. Input'Last),
                                   Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                                   Ada.Strings.Outside, Term_Start, Term_Stop);
                        if Term_Start <= Term_Stop then
                           declare
                              Term_Word : constant String := Input (Term_Start .. Term_Stop);
                           begin
                              if Term_Word = "+LOOP" or Term_Word = "+loop" then
                                 declare
                                    Increment : constant Stack.Cell := Stack.Pop;
                                 begin
                                    Loop_Index := Loop_Index + Increment;
                                 end;
                              else  --  Regular LOOP
                                 Loop_Index := Loop_Index + 1;
                              end if;
                           end;
                        end if;
                     end loop;
                     
                     --  Clean up return stack after loop completes
                     while Stack.RDepth > Saved_RDepth loop
                        declare
                           Dummy : Stack.Cell := Stack.RPop;
                        begin
                           null;
                        end;
                     end loop;
                     
                     --  Skip past the loop terminator
                     Find_Token (Input (Loop_End_Pos .. Input'Last),
                                Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                                Ada.Strings.Outside, Term_Start, Term_Stop);
                     if Term_Start <= Term_Stop then
                        Start := Term_Stop + 1;
                     else
                        Start := Input'Last + 1;
                     end if;
                  end if;
               end;
            elsif (Word = "I" or Word = "i") then
               if Stack.RDepth > 0 then
                  Stack.Push (Stack.RPeek);
               else
                  Ada.Text_IO.Put_Line ("Error: I outside of DO loop");
               end if;
               Start := Stop + 1;
            elsif (Word = "VARIABLE" or Word = "variable") then
               Find_Token (Input (Stop + 1 .. Input'Last),
                          Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                          Ada.Strings.Outside, Start, Stop);
               if Start <= Stop then
                  declare
                     Var_Name : constant String := Input (Start .. Stop);
                     Address  : constant Stack.Cell := Memory.Allot (1);
                  begin
                     Dictionary.Define_Variable (Var_Name, Address);
                  end;
               end if;
               Start := Stop + 1;
            elsif Word = "\" then
               --  Comment to end of line - skip rest
               exit;
            elsif Word = "(" then
               --  Comment until closing ) - skip to )
               declare
                  Paren_Pos : Natural := Stop + 1;
               begin
                  while Paren_Pos <= Input'Last loop
                     if Input (Paren_Pos) = ')' then
                        Start := Paren_Pos + 1;
                        goto Continue_Loop;
                     end if;
                     Paren_Pos := Paren_Pos + 1;
                  end loop;
                  --  No closing ) found, treat as end of input
                  exit;
               end;
               <<Continue_Loop>>
            elsif Word = ".""" or (Word'Length >= 2 and then Word (Word'First .. Word'First + 1) = ".""") then
               --  Print string literal until closing quote
               declare
                  Str_Start : Positive;
                  Str_End : Natural := Stop;
                  InQuote : Boolean := True;
               begin
                  --  Check if quote is part of this word or next
                  if Word'Length > 2 then
                     --  Quote starts in this word (like ."Hello)
                     Str_Start := Word'First + 2;
                     --  Check if quote ends in this word
                     for I in Str_Start .. Word'Last loop
                        if Word (I) = '"' then
                           Ada.Text_IO.Put (Word (Str_Start .. I - 1));
                           InQuote := False;
                           exit;
                        end if;
                     end loop;
                     if InQuote then
                        Ada.Text_IO.Put (Word (Str_Start .. Word'Last));
                     end if;
                  end if;

                  --  Continue to closing quote if needed
                  if InQuote then
                     Str_Start := Stop + 1;
                     while Str_Start <= Input'Last loop
                        Str_End := Index (Input (Str_Start .. Input'Last), """");
                        if Str_End > 0 then
                           if Str_Start < Str_End then
                              Ada.Text_IO.Put (Input (Str_Start .. Str_End - 1));
                           end if;
                           Start := Str_End + 1;
                           InQuote := False;
                           exit;
                        else
                           Ada.Text_IO.Put (Input (Str_Start .. Input'Last));
                           exit;
                        end if;
                     end loop;
                  end if;

                  if InQuote then
                     Ada.Text_IO.Put_Line ("");
                     Ada.Text_IO.Put_Line ("Error: Unclosed string literal");
                  end if;
               end;
               Start := Stop + 1;
            elsif (Word = "CONSTANT" or Word = "constant") then
               Find_Token (Input (Stop + 1 .. Input'Last),
                          Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                          Ada.Strings.Outside, Start, Stop);
               if Start <= Stop then
                  declare
                     Const_Name : constant String := Input (Start .. Stop);
                     Value      : constant Stack.Cell := Stack.Pop;
                  begin
                     Dictionary.Define_Constant (Const_Name, Value);
                  end;
               end if;
               Start := Stop + 1;
            elsif (Word = "LOAD" or Word = "load" or Word = "INCLUDE" or Word = "include") then
               --  Load a Forth source file
               Find_Token (Input (Stop + 1 .. Input'Last),
                          Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                          Ada.Strings.Outside, Start, Stop);
               if Start <= Stop then
                  declare
                     Filename : constant String := Input (Start .. Stop);
                  begin
                     Load_File (Filename);
                  end;
               else
                  Ada.Text_IO.Put_Line ("Error: LOAD requires a filename");
               end if;
               Start := Stop + 1;
            elsif (Word = "UNTIL" or Word = "until" or 
                   Word = "WHILE" or Word = "while" or
                   Word = "REPEAT" or Word = "repeat" or
                   Word = "LOOP" or Word = "loop" or
                   Word = "+LOOP" or Word = "+loop") then
               --  These are handled by their corresponding BEGIN/DO
               Start := Stop + 1;
            else
               Execute_Primitive (Word);
               Start := Stop + 1;
            end if;
         end;
      end loop;
   end Eval_With_Conditionals;

   --  Execute a primitive word or number
   procedure Execute_Primitive (Word : String) is
   begin
      if Word = "+" then
         Stack.Push (Stack.Pop + Stack.Pop);
      elsif Word = "-" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (A - B);
         end;
      elsif Word = "*" then
         Stack.Push (Stack.Pop * Stack.Pop);
      elsif Word = "/" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (A / B);
         end;
      elsif Word = "MOD" or Word = "mod" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (A mod B);
         end;
      elsif Word = "/MOD" or Word = "/mod" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (A mod B);
            Stack.Push (A / B);
         end;
      elsif Word = "NEGATE" or Word = "negate" then
         Stack.Push (-Stack.Pop);
      elsif Word = "ABS" or Word = "abs" then
         declare
            Val : constant Stack.Cell := Stack.Pop;
         begin
            if Val < 0 then
               Stack.Push (-Val);
            else
               Stack.Push (Val);
            end if;
         end;
      elsif Word = "MIN" or Word = "min" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A < B then
               Stack.Push (A);
            else
               Stack.Push (B);
            end if;
         end;
      elsif Word = "MAX" or Word = "max" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A > B then
               Stack.Push (A);
            else
               Stack.Push (B);
            end if;
         end;
      elsif Word = "1+" then
         Stack.Push (Stack.Pop + 1);
      elsif Word = "1-" then
         Stack.Push (Stack.Pop - 1);
      elsif Word = "2*" then
         Stack.Push (Stack.Pop * 2);
      elsif Word = "2/" then
         Stack.Push (Stack.Pop / 2);
      elsif Word = "AND" or Word = "and" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (Stack.Bit_And (A, B));
         end;
      elsif Word = "OR" or Word = "or" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (Stack.Bit_Or (A, B));
         end;
      elsif Word = "XOR" or Word = "xor" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (Stack.Bit_Xor (A, B));
         end;
      elsif Word = "NOT" or Word = "not" then
         Stack.Push (Stack.Bit_Not (Stack.Pop));
      elsif Word = "INVERT" or Word = "invert" then
         Stack.Push (Stack.Bit_Not (Stack.Pop));
      elsif Word = "=" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A = B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = "<" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A < B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = ">" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A > B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = "<=" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A <= B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = ">=" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A >= B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = "<>" then
         declare
            B : constant Stack.Cell := Stack.Pop;
            A : constant Stack.Cell := Stack.Pop;
         begin
            if A /= B then
               Stack.Push (-1);
            else
               Stack.Push (0);
            end if;
         end;
      elsif Word = "0=" then
         if Stack.Pop = 0 then
            Stack.Push (-1);
         else
            Stack.Push (0);
         end if;
      elsif Word = "0<" then
         if Stack.Pop < 0 then
            Stack.Push (-1);
         else
            Stack.Push (0);
         end if;
      elsif Word = "0>" then
         if Stack.Pop > 0 then
            Stack.Push (-1);
         else
            Stack.Push (0);
         end if;
      elsif Word = "." then
         Ada.Text_IO.Put_Line (Stack.Cell'Image (Stack.Pop));
      elsif Word = ".S" or Word = ".s" then
         Stack.Display_Stack;
         Ada.Text_IO.New_Line;
      elsif Word = "WORDS" or Word = "words" then
         Dictionary.List_Words;
      elsif Word = "EMIT" or Word = "emit" then
         Ada.Text_IO.Put (Character'Val (Integer (Stack.Pop)));
      elsif Word = "CR" or Word = "cr" then
         Ada.Text_IO.New_Line;
      elsif Word = "DUP" or Word = "dup" then
         Stack.Dup;
      elsif Word = "DROP" or Word = "drop" then
         Stack.Drop;
      elsif Word = "SWAP" or Word = "swap" then
         Stack.Swap;
      elsif Word = "OVER" or Word = "over" then
         Stack.Over;
      elsif Word = "ROT" or Word = "rot" then
         Stack.Rot;
      elsif Word = "2DUP" or Word = "2dup" then
         Stack.TwoDup;
      elsif Word = "2DROP" or Word = "2drop" then
         Stack.TwoDrop;
      elsif Word = "2SWAP" or Word = "2swap" then
         Stack.TwoSwap;
      elsif Word = "TUCK" or Word = "tuck" then
         Stack.Tuck;
      elsif Word = "NIP" or Word = "nip" then
         Stack.Nip;
      elsif Word = "PICK" or Word = "pick" then
         Stack.Pick;
      elsif Word = ".S" or Word = ".s" then
         Ada.Text_IO.Put ("<" & Natural'Image (Stack.Depth) & "> ");
         for I in 1 .. Stack.Depth loop
            Ada.Text_IO.Put (Stack.Cell'Image (Stack.Peek_At (Stack.Depth - I + 1)));
            Ada.Text_IO.Put (" ");
         end loop;
         Ada.Text_IO.New_Line;
      elsif Word = "WORDS" or Word = "words" then
         Dictionary.List_Words;
      elsif Word = "!" then
         declare
            Address : constant Stack.Cell := Stack.Pop;
            Value   : constant Stack.Cell := Stack.Pop;
         begin
            Memory.Store (Address, Value);
         end;
      elsif Word = "@" then
         declare
            Address : constant Stack.Cell := Stack.Pop;
         begin
            Stack.Push (Memory.Fetch (Address));
         end;
      elsif Word = "+!" then
         declare
            Address : constant Stack.Cell := Stack.Pop;
            Value   : constant Stack.Cell := Stack.Pop;
            Current : constant Stack.Cell := Memory.Fetch (Address);
         begin
            Memory.Store (Address, Current + Value);
         end;
      elsif Word = "HERE" or Word = "here" then
         Stack.Push (Memory.Here);
      elsif Word = "ALLOT" or Word = "allot" then
         declare
            Cells : constant Integer := Integer (Stack.Pop);
            Dummy : constant Stack.Cell := Memory.Allot (Cells);
         begin
            null;
         end;
      elsif Dictionary.Is_Variable (Word) then
         Stack.Push (Dictionary.Get_Variable_Address (Word));
      elsif Dictionary.Is_Constant (Word) then
         Stack.Push (Dictionary.Get_Constant_Value (Word));
      elsif Dictionary.Exists (Word) then
         Eval_With_Conditionals (Dictionary.Get_Definition (Word));
      
      --  Floating-point operations
      elsif Word = "F+" or Word = "f+" then
         FPush (FPop + FPop);
      elsif Word = "F-" or Word = "f-" then
         declare
            B : constant FCell := FPop;
         begin
            FPush (FPop - B);
         end;
      elsif Word = "F*" or Word = "f*" then
         FPush (FPop * FPop);
      elsif Word = "F/" or Word = "f/" then
         declare
            B : constant FCell := FPop;
         begin
            FPush (FPop / B);
         end;
      elsif Word = "FSQRT" or Word = "fsqrt" then
         FPush (FCell (Ada.Numerics.Long_Elementary_Functions.Sqrt (Long_Float (FPop))));
      elsif Word = "FABS" or Word = "fabs" then
         FPush (abs FPop);
      elsif Word = "FNEGATE" or Word = "fnegate" then
         FPush (-FPop);
      elsif Word = "F." or Word = "f." then
         Ada.Text_IO.Put (Long_Float'Image (Long_Float (FPop)));
      elsif Word = "FDUP" or Word = "fdup" then
         FDup;
      elsif Word = "FDROP" or Word = "fdrop" then
         FDrop;
      elsif Word = "FSWAP" or Word = "fswap" then
         FSwap;
      elsif Word = "FOVER" or Word = "fover" then
         FOver;
      elsif Word = "FROT" or Word = "frot" then
         FRot;
      elsif Word = "F2DUP" or Word = "f2dup" then
         F2Dup;
      elsif Word = "F2DROP" or Word = "f2drop" then
         F2Drop;
      elsif Word = "S>F" or Word = "s>f" then
         FPush (FCell (Long_Float (Stack.Pop)));
      elsif Word = "F>S" or Word = "f>s" then
         Stack.Push (Stack.Cell (Integer (Long_Float (FPop))));
      elsif Word = "F<" or Word = "f<" then
         declare
            B : constant FCell := FPop;
         begin
            Stack.Push (if FPop < B then -1 else 0);
         end;
      elsif Word = "F=" or Word = "f=" then
         declare
            B : constant FCell := FPop;
         begin
            Stack.Push (if FPop = B then -1 else 0);
         end;
      elsif Word = "F>" or Word = "f>" then
         declare
            B : constant FCell := FPop;
         begin
            Stack.Push (if FPop > B then -1 else 0);
         end;
      elsif Word = "F0<" or Word = "f0<" then
         Stack.Push (if FPop < 0.0 then -1 else 0);
      elsif Word = "F0=" or Word = "f0=" then
         Stack.Push (if FPop = 0.0 then -1 else 0);
      elsif Word = "PI" or Word = "pi" then
         FPush (FCell (Ada.Numerics.Pi));
      elsif Word = "E" or Word = "e" then
         FPush (FCell (Ada.Numerics.e));
      
      else
         begin
            Stack.Push (Stack.Cell'Value (Word));
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line ("Unknown word: " & Word);
         end;
      end if;
   end Execute_Primitive;

   --  Evaluate a line of Forth code
   procedure Eval (Input : String) is
      use Ada.Strings.Fixed;
      Start : Positive := Input'First;
      Stop  : Natural;
   begin
      --  Check if we're defining a word, variable, or constant
      Find_Token (Input, Ada.Strings.Maps.To_Set (" " & ASCII.HT), Ada.Strings.Outside, Start, Stop);
      
      if Start <= Stop then
         declare
            First_Word : constant String := Input (Start .. Stop);
         begin
            if First_Word = ":" then
               --  Handle word definition
               Compiling := True;
               New_Word_Len := 0;
               New_Word_Def_Len := 0;
               Start := Stop + 1;
               
               while Start <= Input'Last loop
                  Find_Token (Input (Start .. Input'Last), Ada.Strings.Maps.To_Set (" " & ASCII.HT), Ada.Strings.Outside, Start, Stop);
                  exit when Start > Stop;

                  declare
                     Word : constant String := Input (Start .. Stop);
                  begin
                     if Word = ";" then
                        Dictionary.Define (
                           New_Word_Name (1 .. New_Word_Len),
                           New_Word_Def (1 .. New_Word_Def_Len)
                        );
                        Compiling := False;
                        Ada.Text_IO.Put_Line ("  ok");
                     elsif New_Word_Len = 0 then
                        New_Word_Len := Word'Length;
                        New_Word_Name (1 .. New_Word_Len) := Word;
                     else
                        if New_Word_Def_Len > 0 then
                           New_Word_Def_Len := New_Word_Def_Len + 1;
                           New_Word_Def (New_Word_Def_Len) := ' ';
                        end if;
                        New_Word_Def (New_Word_Def_Len + 1 .. New_Word_Def_Len + Word'Length) := Word;
                        New_Word_Def_Len := New_Word_Def_Len + Word'Length;
                     end if;
                  end;

                  Start := Stop + 1;
               end loop;
            elsif First_Word = "VARIABLE" or First_Word = "variable" then
               --  Already handled in Eval_With_Conditionals
               Eval_With_Conditionals (Input);
            elsif First_Word = "CONSTANT" or First_Word = "constant" then
               --  Already handled in Eval_With_Conditionals
               Eval_With_Conditionals (Input);
            else
               --  Normal execution with full loop/conditional support
               Eval_With_Conditionals (Input);
            end if;
         end;
      end if;
   end Eval;

   --  Start the interactive REPL
   procedure Run is
      use Ada.Text_IO;
      Input : String (1 .. 2048);
      Line_Buffer : String (1 .. 256);
      Last  : Natural := 0;
      Line_Last : Natural;
      In_Definition : Boolean := False;
      Show_Stack : Boolean := False;  -- Toggle for auto stack display
      
      --  Track nesting depth for control structures
      function Count_Control_Depth (S : String) return Integer is
         use Ada.Strings.Fixed;
         Pos : Positive := S'First;
         Token_Start, Token_Stop : Natural;
         Loop_Depth : Integer := 0;
         If_Depth : Integer := 0;
      begin
         while Pos <= S'Last loop
            Find_Token (S (Pos .. S'Last), Ada.Strings.Maps.To_Set (" " & ASCII.HT),
                       Ada.Strings.Outside, Token_Start, Token_Stop);
            exit when Token_Start > Token_Stop;
            
            declare
               Word : constant String := S (Token_Start .. Token_Stop);
            begin
               if Word = "DO" or Word = "do" or Word = "BEGIN" or Word = "begin" then
                  Loop_Depth := Loop_Depth + 1;
               elsif Word = "LOOP" or Word = "loop" or Word = "+LOOP" or Word = "+loop" or 
                     Word = "UNTIL" or Word = "until" or Word = "REPEAT" or Word = "repeat" then
                  Loop_Depth := Loop_Depth - 1;
               elsif Word = "IF" or Word = "if" then
                  If_Depth := If_Depth + 1;
               elsif Word = "THEN" or Word = "then" then
                  If_Depth := If_Depth - 1;
               end if;
            end;
            
            Pos := Token_Stop + 1;
         end loop;
         
         return Loop_Depth + If_Depth;
      end Count_Control_Depth;
   begin
      Put_Line ("Minimal Forth Interpreter");
      Put_Line ("Type 'bye' to exit");
      Put_Line ("Use ':' and ';' to define words");
      Put_Line ("Commands: .S (show stack), WORDS (list words), STACK-ON/STACK-OFF");
      Put_Line ("");
      
      -- Initialize readline (safe even if not used)
      begin
         Readline.Init;
      exception
         when others => null;
      end;

      loop
         declare
            Prompt : String (1 .. 256);
            Prompt_Last : Natural := 0;
            Line_Str : String (1 .. 256);
            Line_Len : Natural := 0;
            Use_Readline : Boolean := Is_Terminal;
         begin
            if Use_Readline then
               --  Build prompt for readline
               if Show_Stack and Stack.Depth > 0 then
                  Prompt (1) := ' ';
                  Prompt_Last := 1;
                  declare
                     Stack_Display : constant String := Stack.Get_Stack_Display;
                  begin
                     if Prompt_Last + Stack_Display'Length <= Prompt'Last then
                        Prompt (Prompt_Last + 1 .. Prompt_Last + Stack_Display'Length) := Stack_Display;
                        Prompt_Last := Prompt_Last + Stack_Display'Length;
                     end if;
                  end;
                  if Prompt_Last + 1 <= Prompt'Last then
                     Prompt (Prompt_Last + 1) := ' ';
                     Prompt_Last := Prompt_Last + 1;
                  end if;
               end if;
               
               --  Add prompt symbol
               if In_Definition or (Last > 0 and Count_Control_Depth (Input (1 .. Last)) > 0) then
                  if Prompt_Last + 4 <= Prompt'Last then
                     Prompt (Prompt_Last + 1 .. Prompt_Last + 4) := "... ";
                     Prompt_Last := Prompt_Last + 4;
                  end if;
               else
                  if Prompt_Last + 2 <= Prompt'Last then
                     Prompt (Prompt_Last + 1 .. Prompt_Last + 2) := "> ";
                     Prompt_Last := Prompt_Last + 2;
                  end if;
               end if;
               
               --  Read line with readline support
               declare
                  Result : constant String := Readline.Read_Line (Prompt (1 .. Prompt_Last));
               begin
                  Line_Len := Result'Length;
                  
                  --  Handle EOF (empty string from Ctrl+D)
                  if Line_Len = 0 then
                     Readline.Save_History;
                     Readline.Restore_Terminal;
                     exit;
                  end if;
                  
                  --  Copy to Line_Str
                  if Line_Len > Line_Str'Length then
                     Line_Len := Line_Str'Length;
                  end if;
                  Line_Str (1 .. Line_Len) := Result (1 .. Line_Len);
               end;
            else
               --  Non-interactive mode - use regular Get_Line
               if Show_Stack and Stack.Depth > 0 then
                  Put (" ");
                  Stack.Display_Stack;
               end if;
               
               --  Show different prompt
               if In_Definition or (Last > 0 and Count_Control_Depth (Input (1 .. Last)) > 0) then
                  Put ("... ");
               else
                  Put ("> ");
               end if;
               
               begin
                  Get_Line (Line_Buffer, Line_Last);
                  Line_Len := Line_Last;
                  if Line_Len > 0 then
                     Line_Str (1 .. Line_Len) := Line_Buffer (1 .. Line_Last);
                  end if;
               exception
                  when Ada.IO_Exceptions.End_Error =>
                     exit;
               end;
            end if;
            
            --  Copy to Line_Buffer
            Line_Last := Line_Len;
            if Line_Last > Line_Buffer'Last then
               Line_Last := Line_Buffer'Last;
            end if;
            if Line_Last > 0 then
               Line_Buffer (1 .. Line_Last) := Line_Str (1 .. Line_Last);
               
               --  Add to history only in interactive mode
               if Use_Readline then
                  Readline.Add_History (Line_Buffer (1 .. Line_Last));
               end if;
            end if;
            
            --  Check for bye command
            if Line_Last >= 3 and then Line_Buffer (1 .. Line_Last) = "bye" then
               if Use_Readline then
                  Readline.Save_History;
               end if;
               exit;
            end if;
         end;

         --  Check for REPL-specific commands
         if Line_Last > 0 then
            declare
               Cmd : constant String := Line_Buffer (1 .. Line_Last);
            begin
               if Cmd = "STACK-ON" or Cmd = "stack-on" then
                  Show_Stack := True;
                  Put_Line ("  Stack display enabled");
                  goto Continue_Loop;
               elsif Cmd = "STACK-OFF" or Cmd = "stack-off" then
                  Show_Stack := False;
                  Put_Line ("  Stack display disabled");
                  goto Continue_Loop;
               elsif Cmd = "HELP" or Cmd = "help" then
                  Put_Line ("  Commands:");
                  Put_Line ("    .S or .s        - Show stack");
                  Put_Line ("    WORDS or words  - List defined words");
                  Put_Line ("    STACK-ON        - Auto-display stack");
                  Put_Line ("    STACK-OFF       - Disable auto-display");
                  Put_Line ("    HELP            - This help");
                  Put_Line ("    bye             - Exit");
                  goto Continue_Loop;
               end if;
            end;
         end if;

         --  Check if starting or in a word definition
         if Line_Last > 0 then
            declare
               Trimmed : constant String := Line_Buffer (1 .. Line_Last);
               Processed_Line : String (1 .. 256);
               Processed_Last : Natural := 0;
            begin
               --  Strip backslash comments from the line
               for I in Trimmed'Range loop
                  if Trimmed (I) = '\' then
                     --  Rest of line is comment
                     exit;
                  else
                     Processed_Last := Processed_Last + 1;
                     Processed_Line (Processed_Last) := Trimmed (I);
                  end if;
               end loop;

               --  Check if line starts with ':'
               for I in 1 .. Processed_Last loop
                  if Processed_Line (I) /= ' ' and Processed_Line (I) /= ASCII.HT then
                     if Processed_Line (I) = ':' then
                        In_Definition := True;
                     end if;
                     exit;
                  end if;
               end loop;

               --  Accumulate the processed line
               if Processed_Last > 0 and Last + Processed_Last + 1 <= Input'Last then
                  if Last > 0 then
                     Input (Last + 1) := ' ';
                     Last := Last + 1;
                  end if;
                  Input (Last + 1 .. Last + Processed_Last) := Processed_Line (1 .. Processed_Last);
                  Last := Last + Processed_Last;
               end if;

               --  Check if line contains ';' to end definition
               if In_Definition then
                  for I in 1 .. Processed_Last loop
                     if Processed_Line (I) = ';' then
                        In_Definition := False;
                        exit;
                     end if;
                  end loop;
               end if;
            end;
         end if;

         --  Execute when not in definition mode AND control structures are balanced
         if not In_Definition and Last > 0 then
            --  Check if control structures are balanced
            if Count_Control_Depth (Input (1 .. Last)) = 0 then
               begin
                  Eval (Input (1 .. Last));
               exception
                  when Stack.Stack_Overflow =>
                     Put_Line ("Error: Stack overflow");
                  when Stack.Stack_Underflow =>
                     Put_Line ("Error: Stack underflow");
                  when E : others =>
                     Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
               end;
               Last := 0;  --  Reset buffer
            --  else keep accumulating lines until balanced
            end if;
         end if;
         
         <<Continue_Loop>>
      end loop;
      
      --  Cleanup on exit
      Readline.Restore_Terminal;
   exception
      when Ada.IO_Exceptions.End_Error =>
         --  Ctrl+D or EOF - clean exit
         Readline.Restore_Terminal;
         New_Line;
      when E : others =>
         --  Any other exception - restore terminal first!
         Readline.Restore_Terminal;
         New_Line;
         Put_Line ("Fatal error: " & Ada.Exceptions.Exception_Message (E));
   end Run;

end Forth.Interpreter;
