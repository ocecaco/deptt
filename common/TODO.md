* Potentially get rid of the explicit context by having free variables
  store their own type. This should be very easy with the way I handle
  binding now.
* Handle local definitions more sensibly: don't just inline
  everything. Local definitions in the bidirectional language should
  be turned into local definitions in the core language. Definitions
  should not be expanded when clearly not necessary, although I don't
  think it's necessary to be very clever about the expansion.
* Use eta-equality as well to get a stronger judgmental equality.
* Finish the bidirectional translation so all the data types I've
  implemented can be used.
* Write tests for everything.
* Do some examples using setoids (maybe working with some mathematical
  structures such as groups).
* Improve type error messages (This should be easier with the new
  approach to binding).
* Make the parser/pretty printer work again once the language is
  essentially finished.
* Make it easy to add axioms/assumptions in order to develop
  incomplete programs.
