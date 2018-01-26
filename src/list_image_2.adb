with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function List_Image return String is
   Tmp : Unbounded_String;
   C   : Cursor;
begin
   case Length is
      when 0 => return "";
      when 1 => return Format.Opening & Image (Element (First)) & Format.Postfix;
      when others =>
         C := First;
         Tmp := To_Unbounded_String (Format.Opening) & Image (Element (C));
         for I in 2 .. Length loop
            C := Next (C);
            Tmp := Tmp  & Format.Separator & Image (Element (C));
         end loop;
         Tmp := Tmp & Format.Postfix;
   end case;
   return To_String (Tmp);
end List_Image;
