with List_Format;

-- with Ada.Containers;
with Ada.Iterator_Interfaces;

generic
   type Cursor is private;
   with function Image (C : Cursor) return String;

   type Container (<>) is private;
   with package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, others => <>);
   with function Iterator (C : Container) return Iterator_Interfaces.Forward_Iterator'Class;

   with package Format is new List_Format (<>);

function List_Image_2 (Cont : in Container) return String;
