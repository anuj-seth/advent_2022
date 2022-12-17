with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Day_7 is
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package SU renames Ada.Strings.Unbounded;

   type File_Types is (File, Directory);
   type File_Info_Type is 
      record
         File_Type : File_Types;
         Name : SU.Unbounded_String;
         Size : Natural;
      end record;
   
   package File_Info_Vector is 
      new Ada.Containers.Vectors (Index_Type => Positive,
                                  Element_Type => File_Info_Type);

   function Less (F_1, F_2 : File_Info_Type) return Boolean is
      (F_1.Size < F_2.Size);

   package File_Info_Vector_Sorter is
      new File_Info_Vector.Generic_Sorting (Less);

   package Directory_Trees is new Ada.Containers.Indefinite_Multiway_Trees
      (Element_Type => File_Info_Type);
   use type Directory_Trees.Cursor;

   function Directories (T : Directory_Trees.Tree) return File_Info_Vector.Vector is
      V : File_Info_Vector.Vector;
   begin
      for E of T loop
         if E.File_Type = Directory then
            V.Append (E);
         end if;
      end loop;
      return V;
   end Directories;

   procedure Update_Directory_Size (T : in out Directory_Trees.Tree; C : Directory_Trees.Cursor) is
   begin
      declare
         Child : Directory_Trees.Cursor := Directory_Trees.First_Child (C);
         E : File_Info_Type;
         Directory_Size : Natural := 0;
      begin
         while Child /= Directory_Trees.No_Element loop
            E := Directory_Trees.Element (Child);
            case E.File_Type is
               when File =>
                  Directory_Size := Directory_Size + E.Size;
               when Directory =>
                  Update_Directory_Size (T, Child);
                  Directory_Size := Directory_Size + Directory_Trees.Element (Child).Size;
            end case;
            Child := Directory_Trees.Next_Sibling (Child);
         end loop;
         E := (File_Type => Directory, 
               Name => Directory_Trees.Element (C).Name,
               Size => Directory_Size);
         Directory_Trees.Replace_Element (Container => T,
                                          Position => C,
                                          New_Item => E);
      end;
   end Update_Directory_Size;

   procedure Show_Directory_Tree (C : Directory_Trees.Cursor; Depth : Positive := 1) is
      Current : Directory_Trees.Cursor := Directory_Trees.First_Child (C);
   begin
      while Current /= Directory_Trees.No_Element loop
         for I in 1 .. Depth loop
            TIO.Put ("-");
         end loop;
         TIO.Put (" ");
         TIO.Put (SU.To_String (Directory_Trees.Element (Current).Name) & " ");
         TIO.Put (Directory_Trees.Element (Current).File_Type'Image);
         IIO.Put (Directory_Trees.Element (Current).Size);
         TIO.New_Line;
         case Directory_Trees.Element (Current).File_Type is
            when File =>
               null;
            when Directory =>
               Show_Directory_Tree (Current, Depth + 1);
         end case;
         Current := Directory_Trees.Next_Sibling (Current);
      end loop;
   end Show_Directory_Tree;

   function Load_Directory_Tree (File_Name : String) 
      return Directory_Trees.Tree is
      package SF renames Ada.Strings.Fixed;
      T : Directory_Trees.Tree;
      C : Directory_Trees.Cursor := T.Root;
      A_Node : File_Info_Type;
      F : TIO.File_Type;
   begin
      A_Node := (File_Type => Directory,
                 Name => SU.To_Unbounded_String ("/"),
                 Size => 0);
      T.Append_Child (Parent => C,
                      New_Item => A_Node);
      TIO.Open (F, TIO.In_File, File_Name);
      while not TIO.End_Of_File (F) loop
         declare
            Line : constant String := TIO.Get_Line (F);
         begin
            if Line (1 .. 4) = "$ cd" then
               if Line (6) = '/' then
                  C := Directory_Trees.First_Child (T.Root);
               elsif Line (6 .. 7) = ".." then
                  C := Directory_Trees.Parent (C);
               else
                  A_Node := (File_Type => Directory, 
                             Name => SU.To_Unbounded_String (Line (6 .. Line'Last)), 
                             Size => 0);
                  C := Directory_Trees.First_Child (C);
                  while Directory_Trees.Element (C) /= A_Node loop
                     C := Directory_Trees.Next_Sibling (C);
                  end loop;
               end if;
            elsif Line (1 .. 4) = "$ ls" then
               null;
            elsif Line (1 .. 3) = "dir" then
               A_Node := (File_Type => Directory, 
                          Name => SU.To_Unbounded_String (Line (5 .. Line'Last)), 
                          Size => 0);
               T.Append_Child (Parent => C, New_Item => A_Node);
            else
               declare 
                  Separator_Index : Natural := SF.Index (Line, " ");
                  File_Size : Natural := Natural'Value (Line (Line'First .. Separator_Index - 1));
                  F_Name : SU.Unbounded_String := SU.To_Unbounded_String (Line (Separator_Index + 1 .. Line'Last));
               begin
                  A_Node := (File, F_Name, File_Size);
                  T.Append_Child (Parent => C, New_Item => A_Node); --  A_File);
               end;
            end if;
         end;
      end loop;

      TIO.Close (F);
      return T;
   end Load_Directory_Tree;

   procedure Day_7_Part_1 (File_Name : String) is
      T : Directory_Trees.Tree := Load_Directory_Tree (File_Name);
      V : File_Info_Vector.Vector;
      Size_Sum : Natural := 0;
   begin
      --  Show_Directory_Tree (T.Root);
      Update_Directory_Size (T, Directory_Trees.First_Child (T.Root));
      Show_Directory_Tree (T.Root);
      V := Directories (T);
      for E of V loop
         if E.Size <= 100000 then
            Size_Sum := Size_Sum + E.Size;
         end if;
      end loop;
      IIO.Put (Size_Sum);
      TIO.New_Line;
   end Day_7_Part_1;

   procedure Day_7_Part_2 (File_Name : String) is
      T : Directory_Trees.Tree := Load_Directory_Tree (File_Name);
      V : File_Info_Vector.Vector;
      Size_Sum : Natural := 0;
      Size_Required : Natural;
   begin
      --  Show_Directory_Tree (T.Root);
      Update_Directory_Size (T, Directory_Trees.First_Child (T.Root));
      Show_Directory_Tree (T.Root);
      V := Directories (T);
      Size_Required := 30000000 - (70000000 - V (V.First).Size);
      TIO.Put ("Size Required: ");
      IIO.Put (Size_Required);
      TIO.New_Line;

      File_Info_Vector_Sorter.Sort (Container => V);

      for E of V loop
         if E.Size >= Size_Required then
            IIO.Put (E.Size);
         end if;
      end loop;
      TIO.New_Line;
   end Day_7_Part_2;

begin
   Day_7_Part_1 ("files/Test_Day_7.txt");
   Day_7_Part_1 ("files/Day_7.txt");
   Day_7_Part_2 ("files/Test_Day_7.txt");
   Day_7_Part_2 ("files/Day_7.txt");
end Day_7;
