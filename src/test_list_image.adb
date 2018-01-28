-- -----------------------------------------------------------------------------
-- Copyright 2018 Lionel Draghi
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------
-- This file is part of the List_Image project
-- available here https://github.com/LionelDraghi/List_Image
-- -----------------------------------------------------------------------------

with List_Image;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Command_Line;
with Ada.Strings.Fixed;                             use Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Text_IO;                                   use Ada.Text_IO;

procedure Test_List_Image is

   Failure_Count : Natural   := 0;
   Check_Idx     : Positive  := 1;
   Prefix        : Character := Character'Pred ('A');
   Rule          : constant String := 80 * '-';

   -- --------------------------------------------------------------------------
   procedure New_Test (Title : String) is
      -- Print a test separator of the form :
      --    A. $Title test ---------
      -- and increment A at each call
   begin
      Prefix    := Character'Succ (Prefix);
      Check_Idx := 1;
      New_Line;
      Put_Line (Overwrite (Source   => Rule,
                           Position => Rule'First,
                           New_Item => Prefix & ". " & Title & " test "));
   end New_Test;

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    Image    : String;
                    Expected : String) is
      -- Print a test sequence of the form :
      --   A.1. $Title
      --   Expected "$Expected"
      -- then,
      --   OK
      -- if Image = Expected, or
      --   ** Failed **
      -- if not.
      --
      Tmp : constant String := Positive'Image (Check_Idx);
      Idx : constant String := Tmp (2 .. Tmp'Last);
   begin
      New_Line;
      Put_Line (Prefix & '.' & Idx & ". " & Title);
      Put_Line ("Expected :");
      Put_Line ("""" & Expected & """");
      if Image = Expected then
         Put_Line ("OK");
      else
         Put_Line ("**Failed**,  got """ & Image & """");
         Failure_Count := Failure_Count + 1;
      end if;
      Check_Idx := Check_Idx + 1;
   end Check;

   -- container 1
   package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Integer);

   -- container 2
   package Id_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash_Case_Insensitive, "=");

   -- container 3
   package Tests_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   EOL : constant String := ASCII.CR & ASCII.LF;

begin
   -- --------------------------------------------------------------------------
   New_Test ("Bracketed_List_Style instanciation test on a List");
   declare
      Int_List : Integer_Lists.List;

      use Integer_Lists;
      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Image (C : Cursor) return String is
           (Integer'Image (Element (C)));

      function Integer_List_Image_1 is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Image,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => List_Image.Bracketed_List_Style);

--        function Integer_List_Image_1 is new List_Image.List_Image_1
--          (Integer,
--           Integer'Image,
--           Integer_Lists.Cursor,
--           Int_List.First,
--           Integer_Lists.Next,
--           Int_List.Length,
--           Integer_Lists.Element,
--           List_Image.Bracketed_List_Style);
   begin
      Int_List.Clear;
      Check (Title    => "Empty list",
             Image    => Integer_List_Image_1 (Int_List),
             Expected => "[]");

      Int_List.Append (1);
      Check (Title    => "One item",
             Image    => Integer_List_Image_1 (Int_List),
             Expected => "[ 1]");

      Int_List.Append (2);
      Check (Title    => "Two items",
             Image    => Integer_List_Image_1 (Int_List),
             Expected => "[ 1,  2]");

      Int_List.Append (3);
      Check (Title    => "More items",
             Image    => Integer_List_Image_1 (Int_List),
             Expected => "[ 1,  2,  3]");
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Bracketed_List_Style instantiation test on a Set");
   declare
      Id_Set : Id_Sets.Set;

      use Id_Sets;

      function Id_Set_Image_1 is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => Set_Iterator_Interfaces,
         Container   => Set,
         Iterator    => Iterate,
         Style      => List_Image.Bracketed_List_Style);

   begin
      Id_Set.Clear;
      Check (Title    => "Empty list",
             Image    => Id_Set_Image_1 (Id_Set),
             Expected => "[]");

      Id_Set.Insert ("Hyperion");
      Check (Title    => "One item",
             Image    => Id_Set_Image_1 (Id_Set),
             Expected => "[Hyperion]");

      Id_Set.Insert ("Endymion");
      Check (Title    => "Two items",
             Image    => Id_Set_Image_1 (Id_Set),
             Expected => "[Hyperion, Endymion]");

      Id_Set.Insert ("TechnoCore");
      Check (Title    => "More items",
             Image    => Id_Set_Image_1 (Id_Set),
             Expected => "[TechnoCore, Hyperion, Endymion]");
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Minimal default style instantiation test on a List");
   declare
      Int_List : Integer_Lists.List;

      use Integer_Lists;
      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Image (C : Cursor) return String is
        (Integer'Image (Element (C)));

      function Integer_List_Image_2 is new List_Image.Image
         (Cursor      => Cursor,
          Image       => Image,
          Iterator_If => List_Iterator_Interfaces,
          Container   => List,
          Iterator    => Iterator,
          Style      => List_Image.Default_Style);
   begin
      Int_List.Clear;
      Check (Title    => "Empty list",
             Image    => Integer_List_Image_2 (Int_List),
             Expected => "");

      Int_List.Append (1);
      Check (Title    => "One item",
             Image    => Integer_List_Image_2 (Int_List),
             Expected => " 1");

      Int_List.Append (2);
      Check (Title    => "Two items",
             Image    => Integer_List_Image_2 (Int_List),
             Expected => " 1,  2");

      Int_List.Append (3);
      Check (Title           => "More items",
             Image           => Integer_List_Image_2 (Int_List),
             Expected        => " 1,  2,  3");
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Sentences");
   declare
      package Failed_List_Style is new List_Image.List_Style
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

      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Test_List_Image_1 is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => Failed_List_Style);

   begin
      Tests_List.Clear;
      Check (Title    => "Empty list",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => "No test failed");

      Tests_List.Append ("test_1");
      Check (Title    => "One item",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => "Test test_1 fails");

      Tests_List.Append ("test_2");
      Check (Title    => "Two items",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => "Tests test_1 and test_2 fail");

      Tests_List.Append ("test_3");
      Check (Title           => "More items",
             Image           => Test_List_Image_1 (Tests_List),
             Expected        => "Tests test_1, test_2 and test_3 fail");
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Simple bulleted list");
   declare
      Tests_List : Tests_Lists.List;
      use Tests_Lists;

      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Test_List_Image_1 is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => List_Image.Bulleted_List_Style);

      EOL : constant String := ASCII.CR & ASCII.LF;

   begin
      Tests_List.Clear;
      Check (Title           => "Empty list",
             Image           => Test_List_Image_1 (Tests_List),
             Expected        => "");

      Tests_List.Append ("test_1");
      Check (Title           => "One item",
             Image           => Test_List_Image_1 (Tests_List),
             Expected        => EOL & "- test_1" & EOL);

      Tests_List.Append ("test_2");
      Check (Title           => "Two items",
             Image           => Test_List_Image_1 (Tests_List),
             Expected        => EOL &
               "- test_1" & EOL &
               "- test_2" & EOL);

      Tests_List.Append ("test_3");
      Check (Title           => "More items",
             Image           => Test_List_Image_1 (Tests_List),
             Expected        => EOL &
               "- test_1" & EOL &
               "- test_2" & EOL &
               "- test_3" & EOL);
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Markdown bulleted list");
   declare
      Tests_List : Tests_Lists.List;

      use Tests_Lists;
      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Test_List_Image_1 is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => List_Image.Markdown_Bulleted_List_Style);

      EOL : constant String := ASCII.CR & ASCII.LF;

   begin
      Tests_List.Clear;
      Check (Title    => "Empty list",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => EOL);

      Tests_List.Append ("test_1");
      Check (Title    => "One item",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => EOL & EOL &
               "- test_1" & EOL & EOL);

      Tests_List.Append ("test_2");
      Check (Title    => "Two items",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => EOL & EOL &
               "- test_1" & EOL &
               "- test_2" & EOL & EOL);

      Tests_List.Append ("test_3");
      Check (Title    => "More items",
             Image    => Test_List_Image_1 (Tests_List),
             Expected => EOL & EOL &
               "- test_1" & EOL &
               "- test_2" & EOL &
               "- test_3" & EOL & EOL);
   end;

   -- --------------------------------------------------------------------------
   New_Test ("Markdown table lines");
   declare
      L1, L2, L3, L4, L5 : Tests_Lists.List;

      package Markdown_Table_Style is new List_Image.List_Style
        (Prefix           => "|",
         Separator        => "|",
         Postfix          => "|",
         Prefix_If_Empty  => "",
         Postfix_If_Empty => "");
      -- Should be named Github Flavored Markdown, as Markdown
      -- don't define tables.

      use Tests_Lists;
      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Image is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => Markdown_Table_Style);

   begin
      Put_Line ("Example From http://www.tablesgenerator.com/markdown_tables");

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
      L4.Append ("$12");
      L5.Append ("col 3 is");
      L5.Append ("right - aligned");
      L5.Append ("$1");

      Check (Title    => "Line 1",
             Image    => Image (L1),
             Expected => "|Tables|Are|Cool|");
      Check (Title    => "Line 2",
             Image    => Image (L2),
             Expected => "|----------|:-------------:|------:|");
      Check (Title    => "Line 3",
             Image    => Image (L3),
             Expected => "|col 1 is|left-aligned|$1600|");
      Check (Title    => "Line 4",
             Image    => Image (L4),
             Expected => "|col 2 is|centered|$12|");
      Check (Title    => "Line 5",
             Image    => Image (L5),
             Expected => "|col 3 is|right - aligned|$1|");
   end;

   -- --------------------------------------------------------------------------
   New_Test ("html bulleted list");
   declare
      L : Tests_Lists.List;

      use Tests_Lists;
      function Iterator (L : List) return
        List_Iterator_Interfaces.Forward_Iterator'Class is (Iterate (L));

      function Image is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => List_Iterator_Interfaces,
         Container   => List,
         Iterator    => Iterator,
         Style      => List_Image.HTML_Bulleted_List_Style);

   begin
      Check (Title    => "Empty list",
             Image    => Image (L),
             Expected => "");

      L.Append ("salt");
      declare
         Expected : constant String :=
                      "<ul>" & EOL &
                      "  <li>salt</li>" & EOL &
                      "</ul>";
      begin
         Check (Title    => "One item",
                Image    => Image (L),
                Expected => Expected);
      end;

      L.Append ("pepper");
      declare
         Expected : constant String :=
                      "<ul>" & EOL &
                      "  <li>salt</li>" & EOL &
                      "  <li>pepper</li>" & EOL &
                      "</ul>";
      begin
         Check (Title    => "Two items",
                Image    => Image (L),
                Expected => Expected);
      end;

      L.Append ("sugar");
      declare
         Expected : constant String :=
                      "<ul>" & EOL &
                      "  <li>salt</li>" & EOL &
                      "  <li>pepper</li>" & EOL &
                      "  <li>sugar</li>" & EOL &
                      "</ul>";
      begin
         Check (Title    => "More items",
                Image    => Image (L),
                Expected => Expected);
      end;

   end;

   -- --------------------------------------------------------------------------
   New_Line;
   if Failure_Count /= 0 then
      Put_Line (Natural'Image (Failure_Count) & " tests fails.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line (Rule);
      Put_Line ("All tests OK.");
   end if;

end Test_List_Image;
