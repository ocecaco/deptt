# Deptt
Deptt is a type checker for a dependent type theory with a hierarchy of predicative, non-cumulative universes and explicit universe polymorphism à la Agda. It is written in Haskell, and can be run in the browser using GHCJS, with a simple frontend based on `reflex-frp`.

It is mostly based on a [tutorial](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/) by Andrej Bauer, but it extend the type theory with some built-in data types, such as data types for the natural numbers and equality. Furthermore, it adds explicit universe polymorphism, allowing one to write definitions which can be used in multiple universes. This implementation of universe polymorphism is based on Agda, although Agda makes things more convenient by letting the universes be implicit most of the time.

# Building
You can build it using
```bash
nix-build -o frontend-result -A ghcjs.frontend
```
assuming you have `ghcjs` and `reflex-platform` set up properly using Nix.

# Example
The following is an example of a program written in `deptt`, showing what the syntax looks like:
```
let lone : level = lsucc lzero

and type0 : type lone = type lzero

and type1 : type (lsucc lone) = type lone

and binaryrelation (A : type0) : type1 = A -> A -> type0

and reflexive (A : type0) (rel : binaryrelation A) : type0 = forall x : A, rel x x

and symmetric (A : type0) (rel : binaryrelation A) : type0 = forall x y : A, rel x y -> rel y x

and transitive (A : type0) (rel : binaryrelation A) : type0 = forall x y z : A, rel x y -> rel y z -> rel x z

and equivrelation (A : type0) (rel : binaryrelation A) : type0 = prod lzero lzero (reflexive A rel) (prod lzero lzero (symmetric A rel) (transitive A rel))

and setoid : type1 = ex lone lone type0 (fun A : type0 => ex lone lzero (binaryrelation A) (fun rel : binaryrelation A => equivrelation A rel))

and relationby (A B : type0) (f : A -> B) : binaryrelation A = fun (x y : A) => eq lzero B (f x) (f y)

in

let plus (n m : nat) : nat = natelim lzero (fun p : nat => nat) m (fun p : nat => succ) n

in

natelim lzero

(fun q : nat => eq lzero nat (plus q zero) q)

(refl lzero nat zero)

(
fun (k : nat) (IH : eq lzero nat (plus k zero) k) =>
eqelim lzero lzero nat (plus k zero) (fun t : nat => eq lzero nat (plus (succ k) zero) (succ t)) (refl lzero nat (plus (succ k) zero)) k IH
)

: forall n : nat, eq lzero nat (plus n zero) n
```
