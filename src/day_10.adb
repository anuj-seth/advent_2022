with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

procedure Day_10 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;

   type Instruction_Code is (noop, addx);
   for Instruction_Code use (noop => 1, addx => 2);

   type Instruction (Code : Instruction_Code) is
      record
         case Code is
            when noop =>
               null;
            when addx =>
               Add : Integer;
         end case;
      end record;

   package Instruction_Sequence is
      new Ada.Containers.Indefinite_Vectors (Index_Type => Positive,
                                             Element_Type => Instruction);

   package Register_Over_Time is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Integer);

   function Make_Instruction (S : String) return Instruction is
      Instruction_Arg : Integer;
   begin
      case Instruction_Code'Value (S (S'First .. S'First + 3)) is
         when addx =>
            Instruction_Arg := Integer'Value (S (S'First + 5 .. S'Last));
            return (Code => addx, Add => Instruction_Arg);
         when noop =>
            return (Code => noop);
      end case;
   end Make_Instruction;

   function Load_Instructions (File_Name : String)
      return Instruction_Sequence.Vector is
      Ix : Instruction_Sequence.Vector;
      F : TIO.File_Type;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      while not TIO.End_Of_File (F) loop
         declare
            L : String := TIO.Get_Line (F);
            I : Instruction := Make_Instruction (L);
         begin
            Ix.Append (I);
         end;
      end loop;
      return Ix;
   end Load_Instructions;

   procedure Track_X (Do_Instruction : Instruction;
                      X : Integer;
                      Track_In : in out Register_Over_Time.Vector) is
   begin
      Track_In.Append (New_Item => X,
                       Count => Instruction_Code'Enum_Rep (Do_Instruction.Code));
   end Track_X;

   function Apply_Instruction (Ins : Instruction; X : Integer) return Integer is
   begin
      case Ins.Code is
         when addx =>
            return X + Ins.Add;
         when noop =>
            return X;
      end case;
   end Apply_Instruction;


   function Sum_Signal_Strength (Over_Time : Register_Over_Time.Vector;
                                 Start, Increment, End_Idx : Positive)
      return Integer is
      Current_Idx : Positive := Start;
      Strength : Integer := 0;
   begin
      while Current_Idx <= End_Idx loop
         Strength := Strength + (Over_Time (Current_Idx) * Current_Idx);
         Current_Idx := Current_Idx + Increment;
      end loop;
      return Strength;
   end Sum_Signal_Strength;

   procedure Print (Over_Time : Register_Over_Time.Vector; Start, Increment : Positive) is
      Current_Idx : Positive := Start;
   begin
      while Current_Idx <= Over_Time.Last_Index loop
         IIO.Put (Current_Idx);
         IIO.Put (Over_Time (Current_Idx));
         TIO.New_Line;
         Current_Idx := Current_Idx + Increment;
      end loop;
   end Print;

   procedure Day_10_Part_1 (File_Name : String) is
      Ix : Instruction_Sequence.Vector := Load_Instructions (File_Name);
      X_Start : Integer := 1;
      X_Current : Integer := X_Start;
      X_Over_Time : Register_Over_Time.Vector;
   begin
      for E of Ix loop
         Track_X (Do_Instruction => E,
                  X => X_Current,
                  Track_In => X_Over_Time);
         X_Current := Apply_Instruction (E, X_Current);
      end loop;
      X_Over_Time.Append (New_Item => X_Current);

      Print (Over_Time => X_Over_Time, 
             Start => X_Over_Time.First_Index, 
             Increment => 1);
      IIO.Put (Sum_Signal_Strength (X_Over_Time, 20, 40, 220));
      TIO.New_Line;
   end Day_10_Part_1;

   procedure Day_10_Part_2 (File_Name : String) is
      Ix : Instruction_Sequence.Vector := Load_Instructions (File_Name);
      X_Start : Integer := 1;
      X_Current : Integer := X_Start;
      X_Over_Time : Register_Over_Time.Vector;
      type Row_Index_Type is mod 6;
      type Column_Index_Type is mod 40;
      Crt : array (Row_Index_Type, Column_Index_Type) of Character
         := (others => (others => '.')); 
      Crt_Row : Row_Index_Type := 0;
      Crt_Column : Column_Index_Type := 0;
   begin
      for E of Ix loop
         Track_X (Do_Instruction => E,
                  X => X_Current,
                  Track_In => X_Over_Time);
         X_Current := Apply_Instruction (E, X_Current);
      end loop;
      X_Over_Time.Append (New_Item => X_Current);

      for E of X_Over_Time loop
         if Integer (Crt_Column) = E - 1 or else Integer (Crt_Column) = E or else  Integer (Crt_Column) = E + 1 then
            Crt (Crt_Row, Crt_Column) := '#';
         end if;
         Crt_Column := Crt_Column + 1;
         if Crt_Column = 0 then
            Crt_Row := Crt_Row + 1;
         end if;
      end loop;

      for I in Crt'Range (1) loop
         for J in Crt'Range (2) loop
            TIO.Put (Crt (I, J));
         end loop;
         TIO.New_Line;
      end loop;
   end Day_10_Part_2;
begin
   -- Day_10_Part_1 ("files/Small_Day_10.txt");
   -- Day_10_Part_1 ("files/Test_Day_10.txt");
   --  Day_10_Part_1 ("files/Day_10.txt");
   Day_10_Part_2 ("files/Day_10.txt");
end Day_10;
   
