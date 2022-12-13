with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.Regpat;

procedure Day_5 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;

   package Header_Lines_Type is new
      Ada.Containers.Vectors (Index_Type => Positive,
                              Element_Type => SU.Unbounded_String);

   subtype One_Character_String is String (1 .. 1);
   package Crate_Vectors is new
      Ada.Containers.Vectors (Index_Type => Positive,
                              Element_Type => One_Character_String);
   use type Crate_Vectors.Vector;
   subtype Crate is Crate_Vectors.Vector;

   type Cargo_Hold is array (Positive range <>) of Crate;

   type One_Character_String_Array is array (Positive range <>) of One_Character_String;
   
   function Middle_Of_3_Characters (S : SU.Unbounded_String) return One_Character_String_Array is
      Cs : One_Character_String_Array (1 .. (SU.Length (S) + 1) / 4);
      Cs_Idx : Positive := 1;
      Empty_Start_Idx : Positive := 1;
   begin
      while Empty_Start_Idx <= SU.Length (S) - 2  loop
         Cs (Cs_Idx) := SU.Slice (S, Low => Empty_Start_Idx + 1, High => Empty_Start_Idx + 2 - 1);
         Cs_Idx := Cs_Idx + 1;
         Empty_Start_Idx := Empty_Start_Idx + 2 + 2;
      end loop;
      return Cs;
   end Middle_Of_3_Characters;

   function Stacks_Count (Stacks_Line : SU.Unbounded_String) return Positive is
      Stacks : constant One_Character_String_Array := Middle_Of_3_Characters (Stacks_Line);
   begin
      return Stacks'Last;
   end Stacks_Count;

   function Load_Cargo (Cargo_Data : Header_Lines_Type.Vector) return Cargo_Hold is
       V : Cargo_Hold (1 .. Stacks_Count (Cargo_Data.Last_Element));
   begin
      for I in Cargo_Data.First_Index .. Cargo_Data.Last_Index - 1 loop
         declare
            L : constant One_Character_String_Array := Middle_Of_3_Characters (Cargo_Data (I));
         begin
            for I in L'Range loop
               if L (I) /= " " then
                  V (I).Append (L (I));
               end if;
            end loop;
         end;
      end loop;

      return V;
   end Load_Cargo;

   function File_Header_Lines (F : TIO.File_Type) return Header_Lines_Type.Vector is
      Header_Lines : Header_Lines_Type.Vector;
      One_Line : SU.Unbounded_String;
   begin
      One_Line := SU.To_Unbounded_String (TIO.Get_Line (F));
      Header_Lines.Append (One_Line);
      while True loop
         One_Line := SU.To_Unbounded_String (TIO.Get_Line (F));
         exit when One_Line = "";
         Header_Lines.Append (One_Line);
      end loop;
      return Header_Lines;
   end File_Header_Lines;

   procedure Parse_Command (S : String; Move, From, To : out Natural) is
      package Re renames GNAT.Regpat;
      use Re;
      Pattern : constant Re.Pattern_Matcher := Re.Compile ("move\s(\d+)\sfrom\s(\d+)\sto\s(\d+)");
      Matches : Re.Match_Array (0 .. 3);
   begin
      Re.Match (Pattern, S, Matches);
      if Matches (0) = Re.No_Match then
         TIO.Put_Line ("The pattern did not match");
      else
         Move := Natural'Value (S (Matches (1).First .. Matches (1).Last));
         From := Natural'Value (S (Matches (2).First .. Matches (2).Last));
         To := Natural'Value (S (Matches (3).First .. Matches (3).Last));
      end if;
   end Parse_Command;

   procedure Apply_Command_Part_1 (C : in out Cargo_Hold; Move, From, To : Natural) is
   begin
      for I in 1 .. Move loop
         C (To).Prepend (C (From).First_Element);
         C (From).Delete_First;
      end loop;
   end Apply_Command_Part_1;

   procedure Apply_Command_Part_2 (C : in out Cargo_Hold; Move, From, To : Natural) is
   begin
      for I in reverse 1 .. Move loop
         C (To).Prepend (C (From) (I));
         C (From).Delete (I);
      end loop;
   end Apply_Command_Part_2;

   function Populate_Cargo_Hold (F : TIO.File_Type) return Cargo_Hold is
      Header : Header_Lines_Type.Vector := File_Header_Lines (F);
      Cargo_Stacks : Cargo_Hold := Load_Cargo (Header);
   begin
      return Load_Cargo (Header);
   end Populate_Cargo_Hold;

   procedure Read_File (File_Name : String) is
      F : TIO.File_Type;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      declare
         C : Cargo_Hold := Populate_Cargo_Hold (F);
         Move_N, From_Crate, To_Crate : Natural;
      begin
         while not TIO.End_Of_File (F) loop
            Parse_Command (TIO.Get_Line (F), Move_N, From_Crate, To_Crate);
            Apply_Command_Part_2 (C, Move_N, From_Crate, To_Crate);
         end loop;
         for E of C loop
            TIO.Put (E.First_Element);
         end loop;
         TIO.New_Line;
      end;

      TIO.Close (F);
   end Read_File;

begin
   Read_File ("files/Test_Day_5.txt");
   Read_File ("files/Day_5.txt");
end Day_5;


