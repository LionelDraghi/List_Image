with List_Format;
with List_Image;
with Bracketed_List_Format;

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

      -- use Integer_Lists;
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
   if Failure_Count /= 0 then
      Put_Line (Natural'Image (Failure_Count) & " tests fails.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Test_List_Image;
