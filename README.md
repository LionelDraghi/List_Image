# List_Image
Ada generic helper to print iterable containers content, with customizable formats.

This started with a simple idea in mind : removing duplicate code in [ArchiCheck](http://lionel.draghi.free.fr/Archicheck/index.html). 
The same loop that print containers content with, for example, this format: [A, B, C] was present more time in the code.

Writing a generic function for a specific container is simple, but if I want a generic that can be used 
with List, Maps and Sets from the Ada standard containers lib, it becomes fairly more complex.

The interresting initial discussion on "iterable containers" is [here](https://groups.google.com/d/msg/comp.lang.ada/El_hKSV5SVA/GkyFb27SAAAJ)

Released under GPL v3 with the "instantiation" exception.

To build and run the tests, just :
> make

Lionel
