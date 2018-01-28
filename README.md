# List_Image
Ada generic helper to print iterable containers content, with customizable styles.

## Why?

This started with a simple idea in mind : removing duplicate code in [ArchiCheck](http://lionel.draghi.free.fr/Archicheck/index.html). 
The same loop that print containers content with a presentation more or less close to

```
[A, B, C]
```

was duplicated more time in the code.

Writing a generic function for a specific container is simple, but if I want that generic to be instanciated with whatever [iterable containers](http://www.ada-auth.org/standards/12rm/html/RM-5-5-1.html#I3224) from the Ada standard containers lib (List, Maps and Sets, etc.), it becomes fairly more complex, because of Ada Containers design.

## Usage

The `List_Image` package provides :

1. The Image generic function, that is to be instantiated for each user defined Container.  
   The function has a simple profile :
   ```Ada
   function Image (Cont : in Container) return String;
   ```
   The function simply walk through the Container, and gather the image of each element.  
   
2. The `List_Style` generic package, that is one of the `Image` generic parameters.  
   The style of the presentation may be customized in a large way, see examples behind.

3. A collection of predifined `List_Style` instantiation.
      
### use example

```Ada
with List_Image;

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash_Case_Insensitive;
...
   -- Example here with a set of identifier
   package Id_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash_Case_Insensitive, "=");
   Id_Set : Id_Sets.Set;

   use Id_Sets;
   function Id_Set_Image is new List_Image.Image
        (Cursor      => Cursor,
         Image       => Element,
         Iterator_If => Set_Iterator_Interfaces,
         Container   => Set,
         Iterator    => Iterate,
         Style      => List_Image.Bracketed_List_Style);
   ...
   Id_Set.Insert ("Salt");
   Id_Set.Insert ("Pepper");
   Put_Line (Id_Set_Image (Id_Set));

   -- Note the use of the predefined style Bracketed_List_Style.
   -- Image will be [Salt, Pepper]
```
### Predefined styles

- Simple style :
  ```
  A, B, C
  ```

- Bracketed style :
  ```
  [A, B, C]
  ```

- Bulleted :
  ```
  - A
  - B
  - C
  ```

 - Markdown bulleted list : like the bulleted list, but surrounded by
   two empty lines (in some Markdown implementation, if the first bullet
   is not preceded by an empty line, the list is not recognized)

- HTML bulleted list :
  ```
  <ul>
  <li>A</li>
  <li>B</li>
  <li>C</li>
  </ul>
  ```
  Note : an empty list `<ul></ul>` is recognized by most navigator,
         but seems to be illegal html.
         No problem here, thanks to the _If_Empty parameters nothing will
         be generated if the list is emty.
   
- Markdown_Table_Style
  ```
  | A | B | C |
  ```
  Note : Markdown don't define tables, but it's a common extension,
         like in Github Flavored Markdown for example.

### Defining your own style

This signature package defines the style used by the Image function to
print the list.  
```Ada
   generic
      Prefix    : String;
      Postfix   : String;
      Separator : String;

      Last_Separator    : String := Separator;
      Prefix_If_Empty   : String := Prefix;
      Postfix_If_Empty  : String := Postfix;
      Prefix_If_Single  : String := Prefix;
      Postfix_If_Single : String := Postfix;

      EOL               : String := List_Image.EOL;

   package List_Style is end List_Style;
```

`Prefix`, `Postfix` and `Separator` parameters are self explaining.
If `Prefix` is set to '(', `Postfix` to ')', and `Separator` to ',', the returned Image will be of this kind : 
```
(A,B,C,D)
```
If all parameter are set to "", the image will be :
```
ABCD
```
Special Prefix and Postfix are possible for null list, and for list with
a single element.
This is usefull when you want to want `[A,B,C]` as an image, but you don't
want `[]` when the list is empty.

An interresting application of this feature is to have well written comments
regarding singular and plural.  
If you want your image to be
```
A item found
``` 
but 
```
A, B, C items found
```
just set `Postfix` to `" items found"`, and `Postfix_If_Single` to
`" item found"`.

And by the way, if you want the Image to be `"No item found"` when the
list is emtpy, `Prefix_If_Empty` or `Postfix_If_Empty` are here for you.

Note that Separator may be whatever String.  
You may want to insert an End
of Line sequence to split the list on several line, the `EOL` String and
parameters are provided for that purpose.
## About

The initial discussion on "iterable containers" is [on comp.lang.ada](https://groups.google.com/d/msg/comp.lang.ada/El_hKSV5SVA/GkyFb27SAAAJ), but it's certainly not the first one.

Released under [Apache License v2.0](LICENSE-2.0.md).

## Building

Get the sources [on the GitHub project page](https://github.com/LionelDraghi/List_Image)  

To build and run the tests, just :
> make

## Portability

Only tested on my Linux box, but the sources and tests should run nice on most platform, including windows.  
The utilities itself deals with line terminator. Default value is CR/LF, but you can change it, it's a generic parameter.

Lionel
