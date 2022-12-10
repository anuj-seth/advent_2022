with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Hashed_Sets;

procedure Day_4 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package Fixed renames Ada.Strings.Fixed;

   function Hash (P : Positive) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P));
   package H_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Positive,
       Hash => Hash,
       Equivalent_Elements => "=");

   function To_Range_Set (S : String) return H_Sets.Set is
      Ranged : H_Sets.Set;
      Hyphen_Index : Natural := Fixed.Index (S, "-");
      Start_Range : Natural := Natural'Value (Fixed.Head (S, Hyphen_Index - S'First));
      End_Range : Natural := Natural'Value (Fixed.Tail (S, S'Last - Hyphen_Index));
   begin
      for I in Start_Range .. End_Range loop
         Ranged.Insert (I);
      end loop;
      return Ranged;
   end To_Range_Set;

   procedure Day_4_Part_2 is
      F : TIO.File_Type;
      File_Name : constant String := "files/Day_4.txt";
      Contained_Ranges : Natural := 0;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      while not TIO.End_Of_File (F) loop
         declare
            E : constant String := TIO.Get_Line (F);
            Comma_Index : Natural := Fixed.Index (E, ",");
            Part_1 : String := Fixed.Head (E, Comma_Index - E'First);
            Part_2 : String := Fixed.Tail (E, E'Last - Comma_Index);
            R_1 : H_Sets.Set := To_Range_Set (Part_1);
            R_2 : H_Sets.Set := To_Range_Set (Part_2);
         begin
            if H_Sets.Overlap (R_1, R_2) then
               Contained_Ranges := Contained_Ranges + 1;
            end if;
         end;
      end loop;
      TIO.Close (F);
      IIO.Put (Contained_Ranges);
   end Day_4_Part_2;

   procedure Day_4_Part_1 is
      F : TIO.File_Type;
      File_Name : constant String := "files/Day_4.txt";
      Contained_Ranges : Natural := 0;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      while not TIO.End_Of_File (F) loop
         declare
            E : constant String := TIO.Get_Line (F);
            Comma_Index : Natural := Fixed.Index (E, ",");
            Part_1 : String := Fixed.Head (E, Comma_Index - E'First);
            Part_2 : String := Fixed.Tail (E, E'Last - Comma_Index);
            R_1 : H_Sets.Set := To_Range_Set (Part_1);
            R_2 : H_Sets.Set := To_Range_Set (Part_2);
         begin
            if H_Sets.Is_Subset (R_1, R_2) or else H_Sets.Is_Subset (R_2, R_1) then
               Contained_Ranges := Contained_Ranges + 1;
            end if;
         end;
      end loop;
      TIO.Close (F);
      IIO.Put (Contained_Ranges);
   end Day_4_Part_1;
   procedure Test_Day_4_Part_1 is
      Inputs : array (1 .. 6) of String (1 .. 7)
         := ("2-4,6-8",
             "2-3,4-5",
             "5-7,7-9",
             "2-8,3-7",
             "6-6,4-6",
             "2-6,4-8");
      Contained_Ranges : Natural := 0;
   begin
      for E of Inputs loop
         declare 
            Comma_Index : Natural := Fixed.Index (E, ",");
            Part_1 : String := Fixed.Head (E, Comma_Index - E'First);
            Part_2 : String := Fixed.Tail (E, E'Last - Comma_Index);
            R_1 : H_Sets.Set := To_Range_Set (Part_1);
            R_2 : H_Sets.Set := To_Range_Set (Part_2);
         begin
            if H_Sets.Is_Subset (R_1, R_2) or else H_Sets.Is_Subset (R_2, R_1) then
               Contained_Ranges := Contained_Ranges + 1;
            end if;
         end;
      end loop;
      IIO.Put (Contained_Ranges);
      TIO.New_Line;
   end Test_Day_4_Part_1;

begin
   Test_Day_4_Part_1;
   Day_4_Part_1;
   Day_4_Part_2;
end Day_4;

