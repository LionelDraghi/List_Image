with List_Format;

package Markdown_List_Format is new List_Format
  (Prefix           => ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF & "- ",
   Separator        => ASCII.CR & ASCII.LF & "- ",
   Postfix          => ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF,
   Prefix_If_Empty  => ASCII.CR & ASCII.LF,
   Postfix_If_Empty => "");
-- Markdown list is like the simple bulleted list, but surrounded by
-- two empty lines : in some Markdown implementation, if the first bullet
-- is not preceded by an empty line, the list is not recognized.

