- Three main components of a programming language
- Deep embedding
- Shallow embedding
- Circuit language
    - `identity`
    - `beside`
    - `above`
    - `fan`
    - `stretch`
    - Deep embedding

- Deep embedding multiple interpretations/semantics
- Shallow embedding multiple interpretations/semantics
    - Problems with this
    - Advantages of this

- Expression problem
    - Extend syntax and semantics in a modular fashion
- Catamorphism
- `ListF`
    - Functor instance
- `Fix`
- `toList`
- `fromList`
- Algebra
- Catamorphism diagram
- `cata`

- `Functor` instance for `ListF`
- Peano numbers
- `PeanoF` definition
- `Functor` instance for `PeanoF`
- `toInt` for `PeanoF`

# Composing languages

- Coproduct of functors
- `MulF`
- Creating languages with both `ExprF` and `MulF` using `(:+:)`
- Junction of algebras with `\/`
- Solving the expression problem!

# Parsers

- Backus-Naur Form (BNF)
- BNF extended
- Paull's modified algorithm
