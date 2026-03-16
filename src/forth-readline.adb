-- ***************************************************************************
--                      Forth - Line editor 
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
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Interfaces.C;

package body Forth.Readline is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces.C;

   -- Import C terminal control functions
   function forth_terminal_set_raw return int;
   pragma Import (C, forth_terminal_set_raw, "forth_terminal_set_raw");
   
   function forth_terminal_restore return int;
   pragma Import (C, forth_terminal_restore, "forth_terminal_restore");
   
   Terminal_Configured : Boolean := False;
   
   procedure Set_Raw_Mode is
      Result : int;
   begin
      if not Terminal_Configured then
         Result := forth_terminal_set_raw;
         if Result = 0 then
            Terminal_Configured := True;
         end if;
      end if;
   exception
      when others => null;
   end Set_Raw_Mode;
   
   procedure Restore_Terminal is
      Result : int;
   begin
      if Terminal_Configured then
         Result := forth_terminal_restore;
         Terminal_Configured := False;
      end if;
   exception
      when others => null;
   end Restore_Terminal;

   package History_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   History : History_Vectors.Vector;
   History_File : constant String := ".forth_history";
   Max_History : constant := 100;

   procedure Init is
   begin
      Load_History;
      Set_Raw_Mode;
   end Init;

   function Read_Line (Prompt : String) return String is
      Line : Unbounded_String;
      C : Character;
      Available : Boolean;
      History_Pos : Integer := -1;  -- -1 means not browsing history
   begin
      Put (Prompt);
      Flush;

      loop
         Get_Immediate (C, Available);
         
         if not Available then
            delay 0.01;  -- Small delay to not busy-wait
         else
            case C is
               when ASCII.CR | ASCII.LF =>
                  -- Enter pressed
                  New_Line;
                  return To_String (Line);
                  
               when ASCII.BS | ASCII.DEL =>
                  -- Backspace
                  if Length (Line) > 0 then
                     Delete (Line, Length (Line), Length (Line));
                     Put (ASCII.BS & ' ' & ASCII.BS);  -- Erase character on screen
                     Flush;
                  end if;
                  
               when ASCII.ESC =>
                  -- Escape sequence - might be arrow key
                  -- Wait a bit for the rest of the sequence
                  delay 0.02;
                  Get_Immediate (C, Available);
                  if Available and then C = '[' then
                     delay 0.02;
                     Get_Immediate (C, Available);
                     if Available then
                        case C is
                           when 'A' =>  -- Up arrow
                              if not History.Is_Empty then
                                 if History_Pos = -1 then
                                    History_Pos := Integer (History.Last_Index);
                                 elsif History_Pos > 0 then
                                    History_Pos := History_Pos - 1;
                                 end if;
                                 
                                 -- Clear current line
                                 for I in 1 .. Length (Line) loop
                                    Put (ASCII.BS & ' ' & ASCII.BS);
                                 end loop;
                                 Flush;
                                 
                                 -- Show history entry
                                 Line := History.Element (History_Pos);
                                 Put (To_String (Line));
                                 Flush;
                              end if;
                              
                           when 'B' =>  -- Down arrow
                              if History_Pos >= 0 then
                                 if History_Pos < Integer (History.Last_Index) then
                                    History_Pos := History_Pos + 1;
                                    
                                    -- Clear and show
                                    for I in 1 .. Length (Line) loop
                                       Put (ASCII.BS & ' ' & ASCII.BS);
                                    end loop;
                                    Line := History.Element (History_Pos);
                                    Put (To_String (Line));
                                    Flush;
                                 else
                                    -- Back to empty line
                                    for I in 1 .. Length (Line) loop
                                       Put (ASCII.BS & ' ' & ASCII.BS);
                                    end loop;
                                    Line := Null_Unbounded_String;
                                    History_Pos := -1;
                                    Flush;
                                 end if;
                              end if;
                              
                           when others =>
                              null;  -- Ignore other escape sequences like C (right), D (left)
                        end case;
                     else
                        -- Incomplete sequence - consume the '[' and ignore
                        null;
                     end if;
                  else
                     -- Not an arrow key sequence - ignore the ESC
                     null;
                  end if;
                  
               when Character'Val (4) =>  -- Ctrl+D (EOF)
                  if Length (Line) = 0 then
                     New_Line;
                     return "";  -- Signal EOF
                  end if;
                  
               when ' ' .. '~' =>
                  -- Printable character
                  Append (Line, C);
                  Put (C);
                  Flush;
                  
               when others =>
                  null;  -- Ignore other control characters
            end case;
         end if;
      end loop;
   end Read_Line;

   procedure Add_History (Command : String) is
   begin
      if Command'Length = 0 then
         return;
      end if;
      
      -- Don't add duplicates of last command
      if not History.Is_Empty and then
         To_String (History.Last_Element) = Command then
         return;
      end if;
      
      History.Append (To_Unbounded_String (Command));
      
      -- Limit history size
      while Natural (History.Length) > Max_History loop
         History.Delete_First;
      end loop;
   end Add_History;

   procedure Save_History is
      File : File_Type;
      Home : String := Ada.Environment_Variables.Value ("HOME", ".");
      Path : String := Home & "/" & History_File;
   begin
      Create (File, Out_File, Path);
      for Cmd of History loop
         Put_Line (File, To_String (Cmd));
      end loop;
      Close (File);
   exception
      when others =>
         null;  -- Ignore errors saving history
   end Save_History;

   procedure Load_History is
      File : File_Type;
      Home : String := Ada.Environment_Variables.Value ("HOME", ".");
      Path : String := Home & "/" & History_File;
   begin
      if Ada.Directories.Exists (Path) then
         Open (File, In_File, Path);
         while not End_Of_File (File) loop
            declare
               Line : String := Get_Line (File);
            begin
               if Line'Length > 0 then
                  History.Append (To_Unbounded_String (Line));
               end if;
            end;
         end loop;
         Close (File);
      end if;
   exception
      when others =>
         null;  -- Ignore errors loading history
   end Load_History;

end Forth.Readline;
