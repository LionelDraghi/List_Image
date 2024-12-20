<!-- omit from toc -->
List_Image
==========

Ada generic helper to print iterable containers content, with customizable styles.

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

------------------------------------------------------------------------

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Why?](#why)
  - [Ada 2022](#ada-2022)
- [Versions](#versions)
- [Usage](#usage)
  - [use example](#use-example)
  - [Predefined styles](#predefined-styles)
    - [Single Line styles](#single-line-styles)
    - [Multi Line styles](#multi-line-styles)
  - [Defining your own style](#defining-your-own-style)
- [About](#about)
- [References](#references)
- [Building](#building)
- [Portability](#portability)
- [Help and comments](#help-and-comments)

------------------------------------------------------------------------

## Why?

This started with a simple idea in mind : removing duplicate code in [ArchiCheck](http://lionel.draghi.free.fr/Archicheck/index.html). 
The same loop that print containers content with a presentation more or less close to

```
[A, B, C]
```

was duplicated more times in the code.

Writing a generic function for a specific container is simple, but if I want that generic to be instantiated with whatever [iterable containers](http://www.ada-auth.org/standards/12rm/html/RM-5-5-1.html#I3224) from the Ada standard containers lib (List, Maps and Sets, etc.), it becomes fairly more complex, because of Ada Containers design.

To quote [Emmanuel Briot](http://blog.adacore.com/traits-based-containers) :  

> [Ada predefined] ... containers have a lot of similarity in their APIs. As a result, it is relatively easy to use any of the containers when we are familiar with one of them.  
> But this does not make it easy to write algorithms that are container agnostic.

### Ada 2022
[Ada 2022 generalized `'Image` for all types](http://ada-auth.org/standards/22over/html/Ov22-7-1.html). This is a very fortunate evolution, that removes completely one of the main use-case for `List_Image`, that is just debugging : if you're OK with the default Bracketed style of `'Image`, just use it.  

As a consequence, `List_Image` no more provide the `Bracketed_List_Style` generic instantiation in the [versions following 2.0.0](https://github.com/LionelDraghi/List_Image/tree/2.0.0).  

## Versions

- [Version 1.x.x](https://github.com/LionelDraghi/List_Image/tree/1.0.0) is stable, and the last compatible with Ada2012. 

- [Version 2.x.x](https://github.com/LionelDraghi/List_Image/tree/2.0.0) may change in incompatible ways to explore Ada 2022 consequences. Removing the `Bracketed_List_Style` generic instantiation is an example.

## Usage

The `List_Image` package provides :

1. The Image generic function, that is to be instantiated for each user defined Container.  
   The function has a simple profile :
   ```Ada
   function Image (Cont : in Container) return String;
   ```
   The function simply walk through the Container, and gather the image of each element.  
   
2. The `Image_Style` generic package, that is one of the `Image` generic parameters.  
   The style of the presentation may be customized in a large way, see examples behind.

3. A collection of predefined `Image_Style` instantiation.
   
      
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

   -- Start of List_Image related declarations ----------------------------------

   use Id_Sets;
   package Id_Sets_Cursors is new List_Image.Cursors_Signature
     (Container => Id_Sets.Set,
      Cursor    => Id_Sets.Cursor);

   function Image (C : Cursor) return String is (Element (C));

   function Id_Set_Image is new List_Image.Image
     (Cursors => Id_Sets_Cursors,
      Style   => List_Image.Bracketed_List_Style);

   -- End of List_Image related declarations ------------------------------------

   ...
   Id_Set.Insert ("Salt");
   Id_Set.Insert ("Pepper");
   Put_Line (Id_Set_Image (Id_Set));

   -- Note the use of the predefined style Bracketed_List_Style.
   -- Image will be [Salt, Pepper]
```

### Predefined styles

#### Single Line styles

Those instantiations are provided in the `List_Image` package.

- Simple style :
  ```
  A, B, C
  ```

- Bracketed style :
  ```
  [A, B, C]
  ```

- Markdown table style :
  ```
  | A | B | C |
  ```
  Note : Markdown don't define tables, but it's a common extension,
         like in Github Flavored Markdown for example.

#### Multi Line styles

Those instantiations are dependent on the platform definition of an End of Line.
There is no standard way in Ada to know at run time what is the standard EOL.
Therefore, Styles depending on this EOL definition are provided in platform 
specific packages, currently `List_Image.Unix_Predefined_Styles` and `List_Image.Windows_Predefined_Styles`.

- Bulleted :
  ```
  - A
  - B
  - C
  ```

 - Markdown bulleted list :  
   like the bulleted list, but surrounded by
   two empty lines (in some Markdown implementations, if the first bullet
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
         be generated if the list is empty.

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

      EOL               : String := "";

   package Image_Style is end Image_Style;
```

`Prefix`, `Postfix` and `Separator` parameters are self explaining.  
If `Prefix` is set to '(', `Postfix` to ')', and `Separator` to ',', the returned Image will be of this kind : 
```
(A,B,C,D)
```
If all parameters are set to "", the image will be :
```
ABCD
```
Special Prefix and Postfix are possible for null list, and for list with
a single element.  
This is useful when you want `[A,B,C]` as an image, but you
want an empty string (and not `[]`) when the list is empty.

An interesting application of this feature is to have well written comments
regarding singular and plural!  
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
list is empty, `Prefix_If_Empty` or `Postfix_If_Empty` are here for you.

`Last_Separator` allows to have a different last separator, as in :
```
A, B, C and D
```

Both separators may be whatever String.  
You may want to insert an End of Line sequence to split the list on several line, the `EOL` default String and formal
parameter are provided for that purpose.

## About

This package was created by Lionel Draghi, and is released under [Apache License v2.0](LICENSE-2.0.md).

Special thanks to Emmanuel Briot and Randy Bruckardt for their help.

## References 

- [The initial discussion on "iterable containers" is on comp.lang.ada](https://usenet.ada-lang.io/comp.lang.ada/61ba3677-0041-4dba-af9b-a5df48f3ce8a@googlegroups.com/),
- [Traits-Based Containers](http://blog.adacore.com/traits-based-containers)
- [the Generic Ada Library for Algorithms and Containers](https://github.com/AdaCore/ada-traits-containers)
- [The EOL miss and mess in Ada](https://usenet.ada-lang.io/comp.lang.ada/575826a1-c983-49aa-95e2-54048f6b7b5b@googlegroups.com/)

## Building

Get the sources [on the GitHub project page](https://github.com/LionelDraghi/List_Image)  

To build and run the tests, just :
> make

## Portability

Only tested on my Linux box, but the sources and tests should run nice on most platform, including windows.

Outputs on several lines comes with the question of what is (or even "is there") a End Of Line marker on the platform.
(search for `EOL` above for more details). 
The provided `Windows_Predefined_Styles` and `Unix_Predefined_Styles` packages should do the job on most todays platform.

> [!NOTE]
> Version 1.x.x are compatible with Ada 2012, and version 2.x.x are only compatible with Ada 2022.  

---------------------------------------------------------------------
## Help and comments

- Discussions are welcome [here](https://github.com/LionelDraghi/List_Image/discussions)
