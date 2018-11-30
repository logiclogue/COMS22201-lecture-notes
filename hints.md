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
- `Parser` defintion
- `parse` function
- `fail` parser
- `look` parser
- `Functor` instance for `Parser`
- `(<$>)` operator
- `(<$)` operator
- `skip`
- `(<*>)` operator
- `(<**>)` operator
- `(<*)` operator
- `(*>)` operator
- `Monoidal` class equivalence to `Applicative`
- `Alternative`
- `(<|>)` operator
- `choice`
- `(<:>)`
- `some`
- `many`
- `Monad` instance for a parser
- `satisfy`
- `char`

- Paull's algorithm

- `chainl1`

# The Free Monad

- `Free`
- `Functor` instance for `Free f`
- `extract`
- `eval`
- Environment
- `vars` example
- `DivF` example
- Evaluation function for the coproduct of `AddF` and `DivF`
- Safe division with `Maybe`
- `Fail` datatype
- Functor instance of `Fail`
- Safe division with `Fail`
- Substitution
- Non-determinism with `Or`
