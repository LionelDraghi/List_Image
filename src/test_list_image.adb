with List_Format;
with List_Image;
with List_Image_2;
with Bulleted_List_Format;
with Bracketed_List_Format;
with Markdown_List_Format;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash_Case_Insensitive;

with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Command_Line;

procedure Test_List_Image is

   Failure_Count : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Check (That : Boolean;
                    Text : String;
                    Text_If_Failed : String := "") is
   begin
      if That then
         Put_Line ("OK : " & "Expected """ & Text & """");
      else
         Put_Line ("**Failed** : " & "Expected """ & Text
                   & """, but """ & Text_If_Failed & """");
         Failure_Count := Failure_Count + 1;
      end if;
   end Check;

   function Identity (S : String) return String is (S);

   -- container 1
   package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Integer);

   -- container 2
   package Id_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash_Case_Insensitive, "=");

   -- container 3
   package Tests_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

begin
   -- --------------------------------------------------------------------------
   Put_Line ("1. Bracketed_List_Format instanciation test on a List");

   declare
      Int_List : Integer_Lists.List;

      -- use Bracketed_List_Format;
      function Integer_List_Image_1 is new List_Image
        (Integer,
         Integer'Image,
         Integer_Lists.Cursor,
         Int_List.First,
         Integer_Lists.Next,
         Int_List.Length,
         Integer_Lists.Element,
         Bracketed_List_Format);
   begin
      Int_List.Clear;
      Check (That => Integer_List_Image_1 = "[]",
             Text => "List = []",
             Text_If_Failed => Integer_List_Image_1);

      Int_List.Append (1);
      Check (That => Integer_List_Image_1 = "[ 1]",
             Text => "List = [ 1]");

      Int_List.Append (2);
      Check (That => Integer_List_Image_1 = "[ 1,  2]",
             Text => "List = [ 1,  2]");

      Int_List.Append (3);
      Check (That => Integer_List_Image_1 = "[ 1,  2,  3]",
             Text => "List = [ 1,  2,  3]");

   end;

   -- --------------------------------------------------------------------------
   Put_Line ("2. Bracketed_List_Format instantiation test on a Set");

   declare
      Id_Set : Id_Sets.Set;

      function Id_Set_Image_1 is new List_Image
        (String,
         Identity,
         Cursor  => Id_Sets.Cursor,
         First   => Id_Set.First,
         Next    => Id_Sets.Next,
         Length  => Id_Set.Length,
         Element => Id_Sets.Element,
         Format  => Bracketed_List_Format);

   begin
      Id_Set.Clear;
      Check (That => Id_Set_Image_1 = "[]",
             Text => "Set = []",
             Text_If_Failed => Id_Set_Image_1);

      Id_Set.Insert ("Hyperion");
      Check (That => Id_Set_Image_1 = "[Hyperion]",
             Text => "Set = [Hyperion]");

      Id_Set.Insert ("Endymion");
      Check (That => Id_Set_Image_1 = "[Hyperion, Endymion]",
             Text => "Set = [Hyperion, Endymion]");

      Id_Set.Insert ("TechnoCore");
      Check (That => Id_Set_Image_1 = "[TechnoCore, Hyperion, Endymion]",
             Text => "Set = [TechnoCore, Hyperion, Endymion]");

   end;


   -- --------------------------------------------------------------------------
   Put_Line ("3. Minimal default format instantiation test on a List");

   declare
      Int_List : Integer_Lists.List;

      package Default_List_Format is new List_Format;

      function Integer_List_Image_2 is new List_Image
        (Integer,
         Integer'Image,
         Integer_Lists.Cursor,
         Int_List.First,
         Integer_Lists.Next,
         Int_List.Length,
         Integer_Lists.Element,
         Default_List_Format);
   begin
      Int_List.Clear;
      Check (That => Integer_List_Image_2 = "",
             Text => "List = """"");

      Int_List.Append (1);
      Check (That => Integer_List_Image_2 = " 1",
             Text => "List = 1",
             Text_If_Failed => Integer_List_Image_2);

      Int_List.Append (2);
      Check (That => Integer_List_Image_2 = " 1 2",
             Text => "List = 1 2",
             Text_If_Failed => Integer_List_Image_2);

      Int_List.Append (3);
      Check (That => Integer_List_Image_2 = " 1 2 3",
             Text => "List = 1 2 3");

   end;

   -- --------------------------------------------------------------------------
   Put_Line ("4. Sentences");

   declare
      package Failed_List_Format is new List_Format
        (Prefix            => "Tests ",
         Separator         => ", ",
         Last_Separator    => " and ",
         Postfix           => " fail",
         Prefix_If_Empty   => "No test failed",
         Postfix_If_Empty  => "",
         Prefix_If_Single  => "Test ",
         Postfix_If_Single => " fails");

      Tests_List : Tests_Lists.List;
      use Tests_Lists;

      function Test_List_Image_1 is new List_Image
        (String,
         Identity,
         Tests_Lists.Cursor,
         Tests_List.First,
         Tests_Lists.Next,
         Tests_List.Length,
         Tests_Lists.Element,
         Failed_List_Format);
   begin
      Tests_List.Clear;
      Check (That => Test_List_Image_1 = "No test failed",
             Text => "No test failed",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_1");
      Check (That => Test_List_Image_1 = "Test test_1 fails",
             Text => "Test test_1 fails",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_2");
      Check (That => Test_List_Image_1 = "Tests test_1 and test_2 fail",
             Text => "Tests test_1 and test_2 fail",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_3");
      Check (That => Test_List_Image_1 = "Tests test_1, test_2 and test_3 fail",
             Text => "Tests test_1, test_2 and test_3 fail",
             Text_If_Failed => Test_List_Image_1);

   end;

   -- --------------------------------------------------------------------------
   Put_Line ("5. Simple bulleted list");

   declare
      Tests_List : Tests_Lists.List;
      use Tests_Lists;

      function Test_List_Image_1 is new List_Image
        (String,
         Identity,
         Tests_Lists.Cursor,
         Tests_List.First,
         Tests_Lists.Next,
         Tests_List.Length,
         Tests_Lists.Element,
         Bulleted_List_Format);
   begin
      Tests_List.Clear;
      Check (That           => Test_List_Image_1 = "",
             Text           => "Test_List_Image_1 = """,
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_1");
      Check (That           => Test_List_Image_1 = ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF,
             Text           => "- test_1",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_2");
      Check (That           => Test_List_Image_1 = ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF
             & "- test_2" & ASCII.CR & ASCII.LF,
             Text           => "LF - test_1 LF - test_2 LF",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_3");
      Check (That           => Test_List_Image_1 = ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF
             & "- test_2" & ASCII.CR & ASCII.LF
             & "- test_3" & ASCII.CR & ASCII.LF,
             Text           => "LF - test_1 LF - test_2 LF - test_3 LF",
             Text_If_Failed => Test_List_Image_1);
   end;

   -- --------------------------------------------------------------------------
   Put_Line ("6. Markdown bulleted list");

   declare
      Tests_List : Tests_Lists.List;
      use Tests_Lists;

      function Test_List_Image_1 is new List_Image
        (String,
         Identity,
         Tests_Lists.Cursor,
         Tests_List.First,
         Tests_Lists.Next,
         Tests_List.Length,
         Tests_Lists.Element,
         Markdown_List_Format);
   begin
      Tests_List.Clear;
      Check (That           => Test_List_Image_1 = ASCII.CR & ASCII.LF,
             Text           => "Test_List_Image_1 = """,
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_1");
      Check (That           => Test_List_Image_1 =
               ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF,
             Text           => "- test_1",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_2");
      Check (That           => Test_List_Image_1 =
               ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF
             & "- test_2" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF,
             Text           => "LF - test_1 LF - test_2 LF",
             Text_If_Failed => Test_List_Image_1);

      Tests_List.Append ("test_3");
      Check (That           => Test_List_Image_1 =
               ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF
             & "- test_1" & ASCII.CR & ASCII.LF
             & "- test_2" & ASCII.CR & ASCII.LF
             & "- test_3" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF,
             Text           => "LF - test_1 LF - test_2 LF - test_3 LF",
             Text_If_Failed => Test_List_Image_1);

   end;

   -- --------------------------------------------------------------------------
   Put_Line ("7. Markdown table line");

   declare
      L1, L2, L3, L4, L5 : Tests_Lists.List;
      use Tests_Lists;

      package Markdown_Table_Line is new List_Format
        (Prefix           => "|",
         Separator        => "|",
         Postfix          => "|",
         Prefix_If_Empty  => "",
         Postfix_If_Empty => "");
      -- Should be named Github Flavored Markdown, as Markdown
      -- don't define tables.
      function Image (C : Tests_Lists.Cursor) return String is
      begin
         return Element (C);
      end Image;

      function Iterator (L : Tests_Lists.List) return
        Tests_Lists.List_Iterator_Interfaces.Forward_Iterator'Class is
      begin
         return Tests_Lists.List_Iterator_Interfaces.Forward_Iterator'Class
           (Tests_Lists.Iterate (L));
      end Iterator;

      function Line_Image is new List_Image_2
        (Cursor              => Tests_Lists.Cursor,
         Image               => Image,
         Container           => Tests_Lists.List,
         Iterator_Interfaces => Tests_Lists.List_Iterator_Interfaces,
         Iterator            => Iterator,
         Format              => Markdown_Table_Line);

   begin
      Put_Line ("Exemple From http://www.tablesgenerator.com/markdown_tables");
      L1.Append ("Tables");
      L1.Append ("Are");
      L1.Append ("Cool");
      L2.Append ("----------");
      L2.Append (":-------------:");
      L2.Append ("------:");
      L3.Append ("col 1 is");
      L3.Append ("left-aligned");
      L3.Append ("$1600");
      L4.Append ("col 2 is");
      L4.Append ("centered");
      L4.Append (" $12");
      L5.Append ("col 3 is");
      L5.Append ("right - aligned");
      L5.Append ("$1");


      Put_Line (Line_Image (L1));
      Put_Line ("*******************************************");
      Put_Line (Line_Image (L2));
      Put_Line (Line_Image (L3));
      Put_Line (Line_Image (L4));
      Put_Line (Line_Image (L5));

   end;

   -- --------------------------------------------------------------------------
   if Failure_Count /= 0 then
      Put_Line (Natural'Image (Failure_Count) & " tests fails.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Test_List_Image;
