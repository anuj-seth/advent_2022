with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Day_1 is
   package Calories_Vector is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Natural);
   package Calories_Sorter is
      new Calories_Vector.Generic_Sorting;

   Calories_Per_Elf : Calories_Vector.Vector;

   procedure Load_Calories is
      Input_File : String := "files/Day_1.txt";
      F : Ada.Text_IO.File_Type;
      Running_Sum : Natural := 0;
   begin
      Ada.Text_IO.Open (F, 
                        Ada.Text_IO.In_File,
                        Input_File);
      while not Ada.Text_IO.End_Of_File (F) loop
         declare
            Line : String := Ada.Text_IO.Get_Line (F);
         begin
            if Line = "" then
               Calories_Per_Elf.Append (Running_Sum);
               Running_Sum := 0;
            else
               Running_Sum := Running_Sum + Natural'Value (Line);
            end if;
         end;
      end loop;
      
      Calories_Sorter.Sort (Container => Calories_Per_Elf);

      Ada.Text_IO.Close (F);
   end Load_Calories;

   procedure Part_1 is
   begin
      Ada.Text_IO.Put ("Max Calories ");
      Ada.Integer_Text_IO.Put (Calories_Per_Elf.Last_Element);
      Ada.Text_IO.New_Line;
   end Part_1;

   procedure Part_2 is
      Sum_Of_Top_3 : Natural := 0;
   begin
      for Idx in Calories_Per_Elf.Last_Index - 2 .. Calories_Per_Elf.Last_Index loop
         Sum_Of_Top_3 := Sum_Of_Top_3 + Calories_Per_Elf (Idx);
      end loop;
      Ada.Text_IO.Put ("Sum of Top 3");
      Ada.Integer_Text_IO.Put (Sum_Of_Top_3);
      Ada.Text_IO.New_Line;
   end Part_2;

begin
   Load_Calories;
   Part_1;
   Part_2;
end Day_1;
