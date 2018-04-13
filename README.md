# template-haskell-lecture
To be used for the lecture about metaprogramming in Haskell.
The goal is to write two functions, one for automatically generating definitions like `n1`, `n2`, ... that refer to `S Z`, `S (S Z)` and such.
The other is to automatically generate tuple projection functions for n-tuples. Function names should look like `proj2of5`, meaning the second projection of a 5-tuple.

## Usage

To run the REPL for the library:

```
stack repl
```
