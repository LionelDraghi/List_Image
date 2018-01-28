with Ada.Strings.Unbounded;

package body List_Image is

   function Image (Cont : in Container) return String is
      I      : constant Iterator_If.Forward_Iterator'Class := Iterator (Cont);
      C1, C2 : Cursor;

      use Ada.Strings.Unbounded;
      Tmp    : Unbounded_String;

   begin
      C1 := Iterator_If.First (I);

      if not Iterator_If.Has_Element (C1) then
         -- empty data structure
         return Format.Prefix_If_Empty & Format.Postfix_If_Empty;

      else
         -- before using the first list item, we need to know if there a second
         -- item.
         C2 := Iterator_If.Next (I, C1);

         if not Iterator_If.Has_Element (C2) then
            -- single item list
            return Format.Prefix_If_Single & Image (C1)
              & Format.Postfix_If_Single;

         else
            -- at least two item in the list
            Tmp := To_Unbounded_String (Format.Prefix)
              & To_Unbounded_String (Image (C1));
            loop
               C1 := C2;
               C2 := Iterator_If.Next (I, C2);
               if Iterator_If.Has_Element (C2) then
                  Tmp := Tmp & Format.Separator & To_Unbounded_String (Image (C1));

               else
                  Tmp := Tmp & Format.Last_Separator & To_Unbounded_String (Image (C1));
                  exit;

               end if;

            end loop;

         end if;

      end if;
      return (To_String (Tmp & Format.Postfix));

   end Image;

end List_Image;
