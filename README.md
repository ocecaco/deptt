# deptt
Deptt is a type checker for a dependent type theory with a predicative, non-cumulative universes and explicit universe polymorphism à la Agda. It is written in Haskell, and can be run in the browser using GHCJS, with a simple frontend based on `reflex-frp`.

# Building
You can build it using
```bash
nix-build -o frontend-result -A ghcjs.frontend
```
assuming you have `ghcjs` and `reflex-platform` set up properly using Nix.
