with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with GNAT.Regpat;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure Day_11 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package SU renames Ada.Strings.Unbounded;

   package Monkey_Items is
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => Integer);
   
   type Operation_Type is (Add, Multiply, Square);

   type Monkey_Type (Operation : Operation_Type) is
      record
         Monkey_Number : Natural;
         Items_Inspected : Natural := 0;
         Items_Worry_Level : Monkey_Items.Vector;
         Divisible_By : Positive;
         On_True_Throw_To : Positive;
         On_False_Throw_To : Positive;

         case Operation is
            when Add => Addend : Positive;
            when Multiply => Multiplicand : Positive;
            when Square => null;
         end case;
      end record;

   package Monkey_Sequence is
      new Ada.Containers.Indefinite_Vectors (Index_Type => Positive,
                                             Element_Type => Monkey_Type);

   function Load_Monkeys (File_Name : String) 
      return Monkey_Sequence.Vector is
      function To_Monkey_Type (Monkey_Number : Natural;
                               Op : Character; 
                               Operand : SU.Unbounded_String;
                               Items : Monkey_Items.Vector;
                               Divisible_By : Positive;
                               On_True : Positive;
                               On_False : Positive)
         return Monkey_Type is
      begin
         if Op = '+' then
            return (Monkey_Number => Monkey_Number, Items_Inspected => 0, Operation => Add, 
                    Items_Worry_Level => Items, Divisible_By => Divisible_By, On_True_Throw_To => On_True,
                    On_False_Throw_To => On_False, Addend => Positive'Value (SU.To_String (Operand)));
         elsif Op = '*' then
            return (Monkey_Number => Monkey_Number, Items_Inspected => 0, Operation => Multiply, 
                    Items_Worry_Level => Items, Divisible_By => Divisible_By, On_True_Throw_To => On_True,
                    On_False_Throw_To => On_False, Multiplicand => Positive'Value (SU.To_String (Operand)));
         else
            return (Monkey_Number => Monkey_Number, Items_Inspected => 0, Operation => Square, 
                    Items_Worry_Level => Items, Divisible_By => Divisible_By, On_True_Throw_To => On_True,
                    On_False_Throw_To => On_False);
         end if;
      end To_Monkey_Type;

      function To_Items_Vector (S : String)
         return Monkey_Items.Vector is
         package SF renames Ada.Strings.Fixed;
         M : Monkey_Items.Vector;
         Item_Number : Integer;
         Start_Index, Match_Index : Positive;
      begin
         Start_Index := S'First;
         Match_Index := SF.Index (Source => S,
                                  Pattern => ",",
                                  From => Start_Index);
         while Match_Index /= 0 loop
            Item_Number := Integer'Value (S (Start_Index .. Match_Index - 1));
            M.Append (Item_Number);
            Start_Index := Match_Index + 1;
            Match_Index := SF.Index (Source => S,
                                     Pattern => ",",
                                     From => Start_Index);
         end loop;
         Item_Number := Integer'Value (S (Start_Index .. S'Last));
         M.Append (Item_Number);
         return M;
      end To_Items_Vector;

      function To_Monkey (F : TIO.File_Type)
         return Monkey_Type is
         package Re renames GNAT.Regpat;
         use Re;
         Monkey_Number_Pattern : constant Re.Pattern_Matcher 
            := Re.Compile ("Monkey (\d):");
         Starting_Items_Pattern : constant Re.Pattern_Matcher
            := Re.Compile ("Starting items: (.*)$");
         Operation_Pattern : constant Re.Pattern_Matcher
            := Re.Compile ("Operation: new = old\s+([\+|\*])\s+(.*)$");
         Test_Pattern : constant Re.Pattern_Matcher
            := Re.Compile (".*Test: divisible by (\d+)$");
         If_True_Pattern : constant Re.Pattern_Matcher
            := Re.Compile ("If true: throw to monkey\s(\d+)$");
         If_False_Pattern : constant Re.Pattern_Matcher
            := Re.Compile ("If false: throw to monkey\s(\d+)$");
         Matches : Re.Match_Array (0 .. 4);

         Monkey_Number : Positive;
         Items : Monkey_Items.Vector;
         Operation : Character;
         Operation_Arg : SU.Unbounded_String;
         Divisible_By : Natural;
         If_True, If_False : Positive;
         Input_Line : String (1 .. 1024);
         Input_Line_Last : Natural;
      begin
         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (Monkey_Number_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         Monkey_Number := Positive'Value (Input_Line (Matches (1).First .. Matches (1).Last));
         IIO.Put (Monkey_Number);
         TIO.Put_Line (" " & Input_Line (Matches (1).First .. Matches (1).Last));

         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (Starting_Items_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         Items := To_Items_Vector (Input_Line (Matches (1).First .. Matches (1).Last));

         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (Operation_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         Operation := Input_Line (Matches (1).First .. Matches (1).Last) (1);
         Operation_Arg := SU.To_Unbounded_String (Input_Line (Matches (2).First .. Matches (2).Last));

         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (Test_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         Divisible_By := Natural'Value (Input_Line (Matches (1).First .. Matches (1).Last));

         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (If_True_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         If_True := Positive'Value (Input_Line (Matches (1).First .. Matches (1).Last));
         
         TIO.Get_Line (File => F, 
                       Item => Input_Line, 
                       Last => Input_Line_Last);
         Re.Match (If_False_Pattern, 
                   Input_Line (1 .. Input_Line_Last), 
                   Matches);
         If_False := Positive'Value (Input_Line (Matches (1).First .. Matches (1).Last));
         
         return To_Monkey_Type (Monkey_Number => Monkey_Number,
                                Op => Operation,
                                Operand => Operation_Arg,
                                Items => Items,
                                Divisible_By => Divisible_By,
                                On_True => If_True,
                                On_False => If_False);
      end To_Monkey;

      F : TIO.File_Type;
      The_Monkeys : Monkey_Sequence.Vector;
   begin
      TIO.Open (F,
                TIO.In_File,
                File_Name);
      loop
         The_Monkeys.Append (To_Monkey (F));
         exit when  TIO.End_Of_File (F);
         TIO.Skip_Line (F);
      end loop;

      TIO.Close (F);

      return The_Monkeys;
   end Load_Monkeys;

   procedure Day_11_Part_1 (File_Name : String) is
      The_Monkeys : Monkey_Sequence.Vector := Load_Monkeys (File_Name);
   begin
      TIO.Put_Line (The_Monkeys.Length'Image);
      TIO.New_Line;
   end Day_11_Part_1;

begin
   Day_11_Part_1 ("files/Test_Day_11.txt");
   Day_11_Part_1 ("files/Day_11.txt");
end Day_11;
