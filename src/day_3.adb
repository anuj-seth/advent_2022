with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Sets;

procedure Day_3 is

   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;

   function Hash (C : Character) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (Character'Pos (C)));

   package H_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Character,
       Hash => Hash,
       Equivalent_Elements => "=");
   use type H_Sets.Set;

   function String_To_Set (S : String) return H_Sets.Set is
      Set_Of_Characters : H_Sets.Set;
   begin
      for Elt of S loop
         Set_Of_Characters.Include (Elt);
      end loop;
      return Set_Of_Characters;
   end String_To_Set;

   Invalid_Item : exception; 
   function Priority (C : Character) return Natural is
   begin
      case C is
         when 'a' ..'z' => return Character'Pos (C) - 97 + 1;
         when 'A' .. 'Z' => return Character'Pos (C) - 65 + 1 + 26;
         when others => raise Invalid_Item;
      end case;
   end Priority;

   function Duplicate_Item (All_Items : String) return Character is
      Mid : Natural := All_Items'Length / 2;
      Compartment_1 : H_Sets.Set := String_To_Set (All_Items (All_Items'First .. Mid));
      Compartment_2 : H_Sets.Set := String_To_Set (All_Items (Mid + 1 .. All_Items'Last));
      Common : H_Sets.Set := Compartment_1 and Compartment_2;
   begin
      return H_Sets.Element (Common.First);
   end Duplicate_Item;

   function Duplicate_Item_In_Group (R_1, R_2, R_3 : String) return Character is
      Set_1 : H_Sets.Set := String_To_Set (R_1);
      Set_2 : H_Sets.Set := String_To_Set (R_2);
      Set_3 : H_Sets.Set := String_To_Set (R_3);
      Common : H_Sets.Set := Set_1 and Set_2 and Set_3;
   begin
      return H_Sets.Element (Common.First);
   end Duplicate_Item_In_Group;



   procedure Test_Day_3_Part_1 is
      package SU renames Ada.Strings.Unbounded;
      function "+" (X : String) return SU.Unbounded_String
         renames SU.To_Unbounded_String;

      Inputs : array (1 .. 6) of SU.Unbounded_String
         := (+ "vJrwpWtwJgWrhcsFMMfFFhFp",
             + "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
             + "PmmdzqPrVvPwwTWBwg",
             + "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
             + "ttgJtRGJQctTZtZT",
             + "CrZsJsPPZsGzwwsLwLmpwMDw");
      Priority_Sum : Natural := 0; 
   begin
      for L of Inputs loop
         Priority_Sum := Priority_Sum + Priority (Duplicate_Item (SU.To_String(L)));
      end loop;
      IIO.Put (Priority_Sum);
      TIO.New_Line;
   end Test_Day_3_Part_1;

   procedure Test_Day_3_Part_2 is
      package SU renames Ada.Strings.Unbounded;
      function "+" (X : String) return SU.Unbounded_String
         renames SU.To_Unbounded_String;

      Inputs : array (1 .. 6) of SU.Unbounded_String
         := (+ "vJrwpWtwJgWrhcsFMMfFFhFp",
             + "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
             + "PmmdzqPrVvPwwTWBwg",
             + "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
             + "ttgJtRGJQctTZtZT",
             + "CrZsJsPPZsGzwwsLwLmpwMDw");
      Priority_Sum : Natural := 0; 
   begin
      Priority_Sum := Priority (Duplicate_Item_In_Group (SU.To_String(Inputs (4)),
                                                         SU.To_String (Inputs (5)),
                                                         SU.To_String (Inputs (6))))
                      + Priority (Duplicate_Item_In_Group (SU.To_String (Inputs (1)), 
                                                           SU.To_String (Inputs (2)), 
                                                           SU.To_String (Inputs (3))));
      IIO.Put (Priority_Sum);
      TIO.New_Line;
   end Test_Day_3_Part_2;

   procedure Day_3_Part_1 is
      F : TIO.File_Type;
      File_Name : constant String := "files/Day_3.txt";
      P_Sum : Natural := 0;
   begin
      TIO.Open (F, 
                TIO.In_File,
                File_Name);
      while not TIO.End_Of_File (F) loop
         declare
            Input_Line : constant String := TIO.Get_Line (F);
         begin
            P_Sum := P_Sum + Priority (Duplicate_Item (Input_Line));
         end;
      end loop;
      IIO.Put (P_Sum);
      TIO.New_Line;
      TIO.Close (F);
   end Day_3_Part_1;

   procedure Day_3_Part_2 is
      F : TIO.File_Type;
      File_Name : constant String := "files/Day_3.txt";
      P_Sum : Natural := 0;
   begin
      TIO.Open (F, 
                TIO.In_File,
                File_Name);
      while not TIO.End_Of_File (F) loop
            P_Sum := P_Sum + Priority (Duplicate_Item_In_Group (TIO.Get_Line (F),
                                                                TIO.Get_Line (F),
                                                                TIO.Get_Line (F)));
      end loop;
      IIO.Put (P_Sum);
      TIO.New_Line;
   end Day_3_Part_2;

begin
   Test_Day_3_Part_1;
   Day_3_Part_1;
   Test_Day_3_Part_2;
   Day_3_Part_2;
end Day_3;
