* Better pretty-printing
* Use pretty-printing library instead of manually concatenating strings
* Allow more flexible syntax in the parser ("multiple argument" binders)
* Allow adding assumptions (probably using let definitions and a term for assuming)
* Make the normalization more conservative (so you can still understand what you've proved)
* Add tests for nat and eq
* More informative error messages for type checking and parsing
* Add uninhabited type, unit, (dependent) product, coproduct, lists, vectors of fixed size, etc.
* Maybe switch to bidirectional type checking to allow less type annotations, and to allow more documentation by allowing type annotations in arbitrary places
