with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_2 is
   type Winner is (Rock, Paper, Scissors, Draw);
   subtype Hand_Shape is Winner range Rock .. Scissors;

   type Elf_Input_Character is (A, B, C);
   type My_Input_Character is (X, Y, Z);

   function Elf_Choice (Input_Character : Elf_Input_Character) return Hand_Shape is
   begin
      case Input_Character is
         when A => return Rock;
         when B => return Paper;
         when C => return Scissors;
      end case;
   end Elf_Choice;

   function My_Choice (Input_Character : My_Input_Character) return Hand_Shape is
   begin
      case Input_Character is
         when X => return Rock;
         when Y => return Paper;
         when Z => return Scissors;
      end case;
   end My_Choice;

   function My_Choice_Based_On_Outcome (Input_Character : My_Input_Character; Elf_Choice : Hand_Shape)
      return Hand_Shape is
   begin
      case Input_Character is
         when X =>
            case Elf_Choice is
               when Rock => return Scissors;
               when Paper => return Rock;
               when Scissors => return Paper; 
            end case;
         when Y => 
            return Elf_Choice;
         when Z =>
            case Elf_Choice is
               when Rock => return Paper;
               when Paper => return Scissors;
               when Scissors => return Rock;
            end case;
      end case;
   end My_Choice_Based_On_Outcome;

   function Who_Wins (A_Shape : Hand_Shape; B_Shape : Hand_Shape) 
      return Winner is
   begin
      case A_Shape is
         when Rock =>
            case B_Shape is
               when Rock => return Draw;
               when Paper => return Paper;
               when Scissors => return Rock;
            end case;
         when Paper =>
            case B_Shape is
               when Rock => return Paper;
               when Paper => return Draw;
               when Scissors => return Scissors;
            end case;
         when Scissors =>
            case B_Shape is
               when Rock => return Rock;
               when Paper => return Scissors;
               when Scissors => return Draw;
            end case;
      end case;
   end Who_Wins;

   function Score_Shape (Shape : Hand_Shape) return Natural is
   begin
      case Shape is
         when Rock => return 1;
         when Paper => return 2;
         when Scissors => return 3;
      end case;
   end Score_Shape;

   function Score (Elf_Chose : Hand_Shape; I_Chose : Hand_Shape)
      return Natural is
      Points_For_Choice : Natural := Score_Shape (I_Chose);
      Outcome_Points : Natural;
      Winning_Shape : Winner := Who_Wins (Elf_Chose, I_Chose);
   begin
      if Winning_Shape = I_Chose then
         Outcome_Points := 6;
      elsif Winning_Shape = Draw then
         Outcome_Points := 3;
      else
         Outcome_Points := 0;
      end if;
      return Outcome_Points + Points_For_Choice;
   end Score;

   procedure Part_1 is
      Round_Score : Natural := 0;
      Total_Score : Natural := 0;
      F : Ada.Text_IO.File_Type;
      File_Name : constant String := "files/Day_2.txt";
      Input_Line : String (1 .. 3);
      Elf_Input : Elf_Input_Character;
      My_Input : My_Input_Character;
      Elf_Hand_Shape : Hand_Shape;
      My_Hand_Shape : Hand_Shape;
   begin
      Ada.Text_IO.Open (F, 
                        Ada.Text_IO.In_File,
                        File_Name);
      while not Ada.Text_IO.End_Of_File (F) loop
         Input_Line := Ada.Text_IO.Get_Line (F);
         Elf_Input := Elf_Input_Character'Value (Input_Line (1 .. 1));
         My_Input := My_Input_Character'Value (Input_Line (3 .. 3));
         Elf_Hand_Shape := Elf_Choice (Elf_Input);
         My_Hand_Shape := My_Choice (My_Input);
         Round_Score := Score (Elf_Hand_Shape, My_Hand_Shape);
         Total_Score := Total_Score + Round_Score;
      end loop;

      Ada.Text_IO.Close (F);
      Ada.Text_IO.Put ("Part 1: ");
      Ada.Integer_Text_IO.Put (Total_Score);
      Ada.Text_IO.New_Line;
   end Part_1;

   procedure Part_2 is
      Round_Score : Natural := 0;
      Total_Score : Natural := 0;
      F : Ada.Text_IO.File_Type;
      File_Name : constant String := "files/Day_2.txt";
      Input_Line : String (1 .. 3);
      Elf_Input : Elf_Input_Character;
      My_Input : My_Input_Character;
      Elf_Hand_Shape : Hand_Shape;
      My_Hand_Shape : Hand_Shape;
   begin
      Ada.Text_IO.Open (F, 
                        Ada.Text_IO.In_File,
                        File_Name);
      while not Ada.Text_IO.End_Of_File (F) loop
         Input_Line := Ada.Text_IO.Get_Line (F);
         Elf_Input := Elf_Input_Character'Value (Input_Line (1 .. 1));
         My_Input := My_Input_Character'Value (Input_Line (3 .. 3));
         Elf_Hand_Shape := Elf_Choice (Elf_Input);
         My_Hand_Shape := My_Choice_Based_On_Outcome (My_Input, Elf_Hand_Shape);
         Round_Score := Score (Elf_Hand_Shape, My_Hand_Shape);
         Total_Score := Total_Score + Round_Score;
      end loop;

      Ada.Text_IO.Close (F);
      Ada.Text_IO.Put ("Part 2: ");
      Ada.Integer_Text_IO.Put (Total_Score);
      Ada.Text_IO.New_Line;
   end Part_2;

begin
   Part_1;
   Part_2;
end Day_2;
