# 01/10/2018

# Language Engineering 2018

- "The price of reliability is the pursuit of the utmost simplicity" - Tony
  Hoare

A programming language consists of three main components. They are:
    - Syntax - the shape of grammar/words/vocab
    - Semantics - the meaning, a function from syntax to some domain
    - Pragmatics - the purpose of a language

A domain specific language (DSL) is a language that has been crafted with a
specific purpose in mind. They are not necessarily Turing-complete.

Some DSLs come equipped with all the features of general purpose languages:
    - Parser
    - Syntax-highlighting
    - IDEs
    - Compiler
    - Documentation

An embedded domain-specific language (EDSL) is defined within another (host)
language. The advantage is that there is less word to perform. This is at the
cost of restricted flexibility.

An EDSL can be either a deep or shallow embedding.

- Deep embedding - syntax is concrete datatypes, semantics given by evaluation.
- Shallow embedding - syntax is borrowed from host, semantics directly given.

Example, consider:

```
"3 + 5" is a string

v denotational brackets
[|3 + 5|] :: Int
```

We use denotational brackets to ascribe a semantics:

```
[|3 + 5|] = [|3|] + [|5|] = 3 + 5 = 8
```

## Deep Embedding

We can model this using a deep embedding in Haskell with the following code:

```
> data Expr = Var Int
>           | Add Expr Expr
```

Concrete syntax for `3 + 5` is then given by:

```
Add (Var 3) (Var 5) :: Expr
```

A parser is a function that takes a string and produces such a concrete
representation.

The semantics is given by an evaluation function:

```
> eval :: Expr -> Int
> eval (Var n)   = n
> eval (Add x y) = eval x + eval y
```

Notice that instead of `[|3 + 5|]`, we can now write:

```
eval (Add (Var 3) (Var 5))
```

## Shallow Embedding

The shallow embedding is given directly by functions: (we will redefine `Expr`
here)

```
> type Expr = Int
> var :: Int -> Expr
> var n = n
>
> add :: Expr -> Expr -> Expr
> add x y = x + y
```

Our example is now written as:

```
add (var 3) (var 5)
```
