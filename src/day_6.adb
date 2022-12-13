with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Day_6 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;

   function Hash (C : Character) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (Character'Pos (C)));

   package H_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Character,
       Hash => Hash,
       Equivalent_Elements => "=");

   function String_To_Set (S : String) return H_Sets.Set is
      Set_Of_Characters : H_Sets.Set;
   begin
      for Elt of S loop
         Set_Of_Characters.Include (Elt);
      end loop;
      return Set_Of_Characters;
   end String_To_Set;

   function Packet_Starts_After (S : String; N_Unique_Elements : Positive) return Positive is
      use Ada.Containers;
      N_Character_Set : H_Sets.Set;
      Idx : Positive := 1;
   begin
      while Idx <= S'Length - N_Unique_Elements loop
         N_Character_Set := String_To_Set (S (Idx .. Idx + N_Unique_Elements - 1));
         exit when N_Character_Set.Length = Count_Type (N_Unique_Elements);
         Idx := Idx + 1;
      end loop;
      return Idx + N_Unique_Elements - 1;
   end Packet_Starts_After;

   procedure Part_N (N_Unique_Elements : Positive) is
   begin
      IIO.Put (Packet_Starts_After ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", N_Unique_Elements));
      IIO.Put (Packet_Starts_After ("bvwbjplbgvbhsrlpgdmjqwftvncz", N_Unique_Elements));
      IIO.Put (Packet_Starts_After ("nppdvjthqldpwncqszvftbrmjlhg", N_Unique_Elements));
      IIO.Put (Packet_Starts_After ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", N_Unique_Elements));
      IIO.Put (Packet_Starts_After ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", N_Unique_Elements));

      declare
         F : TIO.File_Type;
      begin
         TIO.Open (F,
                   TIO.In_File,
                   "files/Day_6.txt");
         IIO.Put (Packet_Starts_After (TIO.Get_Line (F), N_Unique_Elements));
         TIO.Close (F);
      end;
      TIO.New_Line;
   end Part_N;

begin
   Part_N (4);
   Part_N (14);
end Day_6;
