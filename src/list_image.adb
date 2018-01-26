with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function List_Image return String is
   Tmp : Unbounded_String;
   C   : Cursor;

   use type Ada.Containers.Count_Type;

begin
   case Length is
      when 0 =>
         return Format.Prefix_If_Empty & Format.Postfix_If_Empty;

      when 1 =>
         return Format.Prefix_If_Single & Image (Element (First))
           & Format.Postfix_If_Single;

      when others =>
         C := First;
         Tmp := To_Unbounded_String (Format.Prefix) & Image (Element (C));
         for I in 2 .. Length - 1 loop
            C := Next (C);
            Tmp := Tmp  & Format.Separator & Image (Element (C));
         end loop;
         C := Next (C);
         Tmp := Tmp  & Format.Last_Separator & Image (Element (C));

         return To_String (Tmp & Format.Postfix);

   end case;
end List_Image;
