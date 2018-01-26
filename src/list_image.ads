with List_Format;

with Ada.Containers;

generic
   type Element_Type (<>) is Private;
   with function Image (Element : Element_Type) return String is <>;

   type Cursor is private;
   with function First return Cursor is <>;
   with function Next (Position : Cursor) return Cursor is <>;
   with function Length return Ada.Containers.Count_Type is <>;

   with function Element (Position : Cursor) return Element_Type is <>;

   with package Format is new List_Format (<>);

function List_Image return String;
