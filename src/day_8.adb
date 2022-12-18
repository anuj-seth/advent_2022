with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

procedure Day_8 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;

   package Count_IO is
      new Ada.Text_IO.Integer_IO (Num => Ada.Containers.Count_Type);

   subtype Tree_Height is Natural range 0 .. 9;
   package Heights is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Tree_Height);
   use type Heights.Vector;

   package Tree_Lines is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Heights.Vector);

   function Trees_At_Border (V : Tree_Lines.Vector) return Positive is
      (2 * (V.Last_Index + V (1).Last_Index) - 4);

   function Is_Visible_From_South (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Boolean is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Row_Idx + 1;
   begin
      while Changing_Idx <= V.Last_Index loop
         if V (Changing_Idx) (Col_Idx) >= Height then
            return False;
         end if;
         Changing_Idx := Changing_Idx + 1;
      end loop;
      return True;
   end Is_Visible_From_South;

   function Scenic_Score_From_South (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Natural is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Row_Idx + 1;
   begin
      while Changing_Idx <= V.Last_Index loop
         if V (Changing_Idx) (Col_Idx) >= Height then
            return Changing_Idx - Row_Idx;
         end if;
         Changing_Idx := Changing_Idx + 1;
      end loop;
      return V.Last_Index - Row_Idx;
   end Scenic_Score_From_South;

   function Is_Visible_From_North (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Boolean is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Row_Idx - 1;
   begin
      while Changing_Idx >= V.First_Index loop
         if V (Changing_Idx) (Col_Idx) >= Height then
            return False;
         end if;
         Changing_Idx := Changing_Idx - 1;
      end loop;
      return True;
   end Is_Visible_From_North;

   function Scenic_Score_From_North (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Natural is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Row_Idx - 1;
   begin
      while Changing_Idx >= V.First_Index loop
         if V (Changing_Idx) (Col_Idx) >= Height then
            return Row_Idx - Changing_Idx;
         end if;
         Changing_Idx := Changing_Idx - 1;
      end loop;
      return Row_Idx - V.First_Index;
   end Scenic_Score_From_North;

   function Is_Visible_From_West (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Boolean is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Col_Idx - 1;
   begin
      while Changing_Idx >= V (V.First_Index).First_Index loop
         if V (Row_Idx) (Changing_Idx) >= Height then
            return False;
         end if;
         Changing_Idx := Changing_Idx - 1;
      end loop;
      return True;
   end Is_Visible_From_West;

   function Scenic_Score_From_West (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Natural is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Col_Idx - 1;
   begin
      while Changing_Idx >= V (V.First_Index).First_Index loop
         if V (Row_Idx) (Changing_Idx) >= Height then
            return Col_Idx - Changing_Idx;
         end if;
         Changing_Idx := Changing_Idx - 1;
      end loop;
      return Col_Idx - V (V.First_Index).First_Index;
   end Scenic_Score_From_West;

   function Is_Visible_From_East (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Boolean is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Col_Idx + 1;
   begin
      while Changing_Idx <= V (V.First_Index).Last_Index loop
         if V (Row_Idx) (Changing_Idx) >= Height then
            return False;
         end if;
         Changing_Idx := Changing_Idx + 1;
      end loop;
      return True;
   end Is_Visible_From_East;

   function Scenic_Score_From_East (V : Tree_Lines.Vector; Row_Idx, Col_Idx : Positive)
      return Natural is
      Height : Tree_Height := V (Row_Idx) (Col_Idx);
      Changing_Idx : Positive := Col_Idx + 1;
   begin
      while Changing_Idx <= V (V.First_Index).Last_Index loop
         if V (Row_Idx) (Changing_Idx) >= Height then
            return Changing_Idx - Col_Idx;
         end if;
         Changing_Idx := Changing_Idx + 1;
      end loop;
      return V (V.First_Index).Last_Index - Col_Idx;
   end Scenic_Score_From_East;

   function Best_Scenic_Score (V : Tree_Lines.Vector) return Natural is
      Best_Score : Natural := 0;
      Current_Score : Natural;
   begin
      for Row_Idx in V.First_Index + 1 .. V.Last_Index - 1 loop
         for Col_Idx in V (Row_Idx).First_Index + 1 .. V (Row_Idx).Last_Index - 1 loop
            Current_Score := Scenic_Score_From_East (V, Row_Idx, Col_Idx)
               * Scenic_Score_From_West (V, Row_Idx, Col_Idx)
               * Scenic_Score_From_North (V, Row_Idx, Col_Idx)
               * Scenic_Score_From_South (V, Row_Idx, Col_Idx);
            if Current_Score > Best_Score then
               Best_Score := Current_Score;
            end if;
         end loop;
      end loop;
      return Best_Score;
   end Best_Scenic_Score;

   function Visible_Trees (V : Tree_Lines.Vector) return Natural is
      N : Natural := 0;
   begin
      for Row_Idx in V.First_Index + 1 .. V.Last_Index - 1 loop
         for Col_Idx in V (Row_Idx).First_Index + 1 .. V (Row_Idx).Last_Index - 1 loop
            if Is_Visible_From_East (V, Row_Idx, Col_Idx)
               or Is_Visible_From_West (V, Row_Idx, Col_Idx)
               or Is_Visible_From_North (V, Row_Idx, Col_Idx)
               or Is_Visible_From_South (V, Row_Idx, Col_Idx) then
               N := N + 1;
            end if;
         end loop;
      end loop;
      return N;
   end Visible_Trees;

   procedure Day_8_Part_2 (File_Name : String) is
      F : TIO.File_Type;
      H : Heights.Vector;
      V : Tree_Lines.Vector;
   begin
      TIO.Open (F, 
                TIO.In_File, 
                File_Name);

      while not TIO.End_Of_File (F) loop
         H.Clear;
         for E of TIO.Get_Line (F) loop
            H.Append (Tree_Height'Value ("" & E));
         end loop;
            V.Append (H);
      end loop;
      TIO.Close (F);
      TIO.Put ("Number of lines: ");
      Count_IO.Put (V.Length);
      TIO.New_Line;
      IIO.Put (Best_Scenic_Score (V));
      TIO.New_Line;
   end Day_8_Part_2;

   procedure Day_8_Part_1 (File_Name : String) is
      F : TIO.File_Type;
      H : Heights.Vector;
      V : Tree_Lines.Vector;
   begin
      TIO.Open (F, 
                TIO.In_File, 
                File_Name);

      while not TIO.End_Of_File (F) loop
         H.Clear;
         for E of TIO.Get_Line (F) loop
            H.Append (Tree_Height'Value ("" & E));
         end loop;
            V.Append (H);
      end loop;
      TIO.Close (F);
      TIO.Put ("Number of lines: ");
      Count_IO.Put (V.Length);
      TIO.New_Line;
      IIO.Put (Trees_At_Border (V));
      IIO.Put (Visible_Trees (V));
      TIO.New_Line;
   end Day_8_Part_1;
begin
   Day_8_Part_1 ("files/Day_8.txt");
   Day_8_Part_1 ("files/Test_Day_8.txt");
   Day_8_Part_2 ("files/Test_Day_8.txt");
   Day_8_Part_2 ("files/Day_8.txt");
end Day_8;
