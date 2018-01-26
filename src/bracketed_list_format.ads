with List_Format;

package Bracketed_List_Format is new List_Format (Prefix    => "[",
                                                  Postfix   => "]",
                                                  Separator => ", ");
