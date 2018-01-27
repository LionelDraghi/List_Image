with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function List_Image_2 (Cont : in Container) return String is
   I : constant Iterator_Interfaces.Forward_Iterator'class := Iterator (Cont);
   Tmp : Unbounded_String;
   -- Tmp : Unbounded_String := Null_Unbounded_String;
   C   : Cursor;
   use Iterator_Interfaces;
begin
   C := Iterator_Interfaces.First (I);

   if not Iterator_Interfaces.Has_Element (C) then
      -- empty data structure
      return Format.Prefix_If_Empty & Format.Postfix_If_Empty;

   else
      Tmp := To_Unbounded_String (Format.Prefix);
      loop
         Tmp := To_Unbounded_String (Image (C));
         C := Iterator_Interfaces.Next (I, C);
         exit when not Iterator_Interfaces.Has_Element (C);
      end loop;
   end if;
   return (To_String (Tmp & Format.Postfix));

end List_Image_2;
