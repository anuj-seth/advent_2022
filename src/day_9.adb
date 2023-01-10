with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Numerics.Elementary_Functions;

procedure Day_9 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package NEF renames Ada.Numerics.Elementary_Functions;

   type Direction is (R, L, U, D);

   type Instruction is
      record
         Move_In : Direction;
         Move : Positive;
      end record;

   package Instruction_Vectors is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Instruction);
   use type Instruction_Vectors.Vector;

   type Co_Ordinate is 
      record
         X : Integer;
         Y : Integer;
      end record;

   package Co_Ordinate_Vectors is 
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Co_Ordinate);
   use type Co_Ordinate_Vectors.Vector;

   function Hash (C : Co_Ordinate) return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (Key => C.X'Image & " " & C.Y'Image));

   package Co_Ordinate_Sets is
      new Ada.Containers.Hashed_Sets (Element_Type => Co_Ordinate,
                                      Hash => Hash,
                                      Equivalent_Elements => "=");

   function Instruction_To_Delta (Ix : Instruction) return Co_Ordinate is
   begin
      case Ix.Move_In is
         when R => return (X => 1, Y => 0);
         when L => return (X => -1, Y => 0);
         when D => return (X => 0, Y => -1);
         when U => return (X => 0, Y => 1);
      end case;
   end Instruction_To_Delta;

   function Instruction_To_Deltas (Ix : Instruction) 
      return Co_Ordinate_Vectors.Vector is
      (Co_Ordinate_Vectors.To_Vector (New_Item => Instruction_To_Delta (Ix),
                                      Length => Ada.Containers.Count_Type (Ix.Move)));

   function Add_Co_Ordinates (C : Co_Ordinate; D : Co_Ordinate)
      return Co_Ordinate is
      (C.X + D.X, C.Y + D.Y);

   function Head_Locations (Instructions : Instruction_Vectors.Vector;
                            Head_Start : Co_Ordinate)
      return Co_Ordinate_Vectors.Vector is
      V : Co_Ordinate_Vectors.Vector;
      Current_Head : Co_Ordinate := Head_Start;
   begin
      for Ix of Instructions loop
         for D of Instruction_To_Deltas (Ix) loop
            Current_Head := Add_Co_Ordinates (Current_Head, D);
            V.Append (Current_Head);
         end loop;
      end loop;
      return V;
   end Head_Locations;

   function Is_Overlapping (C_1, C_2 : Co_Ordinate) return Boolean is
      (C_1 = C_2);

   function Is_Touching (C_1, C_2 : Co_Ordinate) return Boolean is
      Delta_X : Integer := C_1.X - C_2.X;
      Delta_Y : Integer := C_1.Y - C_2.Y;
      Distance : Float := NEF.Sqrt (Float (Delta_X ** 2) + Float (Delta_Y ** 2));
   begin
      if Distance = Float (1) or Distance = NEF.Sqrt (Float (2)) then
         return True;
      end if;

      return False;
   end Is_Touching;

   function Next_Tail_Location (Tail, Head : Co_Ordinate)
      return Co_Ordinate is
      Delta_X, Delta_Y : Integer := 0;
   begin
      if Is_Overlapping(Tail, Head) or Is_Touching (Tail, Head) then
         return Tail;
      end if;

      if Tail.X = Head.X then
         Delta_X := 0;
      elsif Tail.X < Head.X then
         Delta_X := 1;
      else
         Delta_X := -1;
      end if;

      if Tail.Y = Head.Y then
         Delta_Y := 0;
      elsif Tail.Y < Head.Y then
         Delta_Y := 1;
      else
         Delta_Y := -1;
      end if;

      return Add_Co_Ordinates (Tail, (X => Delta_X, Y => Delta_Y));
   end Next_Tail_Location;

   function Tail_Locations (Head_Locs : Co_Ordinate_Vectors.Vector; 
                            Tail_Start : Co_Ordinate)
      return Co_Ordinate_Vectors.Vector is
      T : Co_Ordinate_Vectors.Vector;
      Current_Tail : Co_Ordinate := Tail_Start;
   begin
      for E of Head_Locs loop
         Current_Tail := Next_Tail_Location (Tail => Current_Tail,
                                             Head => E);
         T.Append (Current_Tail);
      end loop;
      return T;
   end Tail_Locations;

   function Line_To_Instruction (S : String) return Instruction is
      (Move_In => Direction'Value (S (S'First .. S'First)), 
       Move => Positive'Value (S (S'First + 2 .. S'Last)));

   function Load_Instructions (F : TIO.File_Type) return Instruction_Vectors.Vector is
      V : Instruction_Vectors.Vector;
   begin
      while not TIO.End_Of_File (F) loop
         V.Append (Line_To_Instruction (TIO.Get_Line (F)));
      end loop;
      return V;
   end Load_Instructions;

   procedure Day_9_Part_1 (File_Name : String) is
      F : TIO.File_Type;
      Ix : Instruction_Vectors.Vector; 
      Head, Tail : Co_Ordinate := (X => 0, Y => 0);
      Head_Locs, Tail_Locs : Co_Ordinate_Vectors.Vector;
      C : Co_Ordinate_Vectors.Vector;
      Co_Ord_Set : Co_Ordinate_Sets.Set;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      Ix := Load_Instructions (F);
      TIO.Close (F);
      Head_Locs := Head_Locations (Instructions => Ix, Head_Start => Head);
      Tail_Locs := Tail_Locations (Head_Locs => Head_Locs, Tail_Start => Tail);
      for E of Tail_Locs loop
         Co_Ord_Set.Include (E);
      end loop;
      TIO.Put ("Part 1 Co-ordinates visited ");
      IIO.Put (Integer (Co_Ord_Set.Length));
      TIO.New_Line;
   end Day_9_Part_1;

   procedure Day_9_Part_2 (File_Name : String) is
      F : TIO.File_Type;
      Ix : Instruction_Vectors.Vector; 
      Head, Tail : Co_Ordinate := (X => 0, Y => 0);
      Head_Locs : Co_Ordinate_Vectors.Vector;
      Tail_Locs : array (1 .. 9) of Co_Ordinate_Vectors.Vector;
      C : Co_Ordinate_Vectors.Vector;
      Co_Ord_Set : Co_Ordinate_Sets.Set;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      Ix := Load_Instructions (F);
      TIO.Close (F);
      Head_Locs := Head_Locations (Instructions => Ix, Head_Start => Head);
      Tail_Locs (1) := Tail_Locations (Head_Locs => Head_Locs, Tail_Start => Tail);
      for Idx in Tail_Locs'First + 1 .. Tail_Locs'Last loop
         Tail_Locs (Idx) := Tail_Locations (Head_Locs => Tail_Locs (Idx - 1), Tail_Start => Tail);
      end loop;

      for E of Tail_Locs (9) loop
         Co_Ord_Set.Include (E);
      end loop;
      TIO.Put ("Part 2 Co-ordinates visited ");
      IIO.Put (Integer (Co_Ord_Set.Length));
      TIO.New_Line;
   end Day_9_Part_2;
begin
   --Day_9_Part_1 ("files/Test_Day_9.txt");
   Day_9_Part_1 ("files/Day_9.txt");
   Day_9_Part_2 ("files/Day_9.txt");
end Day_9;
