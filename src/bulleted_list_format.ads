with List_Format;

package Bulleted_List_Format is new List_Format
  (Prefix           => ASCII.CR & ASCII.LF & "- ",
   Separator        => ASCII.CR & ASCII.LF & "- ",
   Postfix          => ASCII.CR & ASCII.LF,
   Prefix_If_Empty  => "",
   Postfix_If_Empty => "");

