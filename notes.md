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
data Expr = Var Int
          | Add Expr Expr
```

Concrete syntax for `3 + 5` is then given by:

```
Add (Var 3) (Var 5) :: Expr
```

A parser is a function that takes a string and produces such a concrete
representation.

The semantics is given by an evaluation function:

```
eval :: Expr -> Int
eval (Var n)   = n
eval (Add x y) = eval x + eval y
```

Notice that instead of `[|3 + 5|]`, we can now write:

```
eval (Add (Var 3) (Var 5))
```

## Shallow Embedding

The shallow embedding is given directly by functions: (we will redefine `Expr`
here)

```
type Expr = Int
var :: Int -> Expr
var n = n

add :: Expr -> Expr -> Expr
add x y = x + y
```

Our example is now written as:

```
add (var 3) (var 5)
```

# 2.1

A compiler is code that transforms a source language to a target language
through some intermediate representation.

```
          eval
source ----------------> target
    |                       ^
    |---> intermediate  ----|
 compile  representation     exec
           (IR)
```

Typical examples of this diagram include the C compiler `gcc`, which takes a C
source file and compiles this to assembly. That assembly is then executed in the
terminal.

JavaScript tends to ignore the compile stage since it is an interpreted
language. The web browser performs eval, which turns JS into rendered output.

Haskell has two modes. When using `ghc`, it compiles `.hs` files into assembly
which can then be executed in the terminal. However, when using `ghci` it takes
source and interprets it directly by evaluating in the terminal.

## Case Study : Circuit Language

[Read: "Folding Domain-Specific Languages" by Gibbons & Wu]

The circuit language is a DSL for describing circuits. It consists of several
operations:

- `identity :: Int -> Circuit`
    - e.g. `identity 3 = | | |` } 3
- `beside :: Circuit -> Circuit -> Circuit`
    - e.g. `beside (| | |) (| |) = | | | | |`
    - `beside (|\|) (| |\|) = |\| | |\|`
- `above :: Circuit -> Circuit -> Circuit`
    - e.g. `above (|\| |) (| |\|) = |\| |`
    - `                             | |\|`
    - This assumes that the width of the two circuits are equal
    - This assumes that the width of the two circuits are equal
- `fan :: Int -> Circuit`
    - e.g. `fan 4 = (|\|-|-|)`
    - `fan 6 = (|\|-|-|-|-|)`
- `stretch :: [Int] -> Circuit -> Circuit`
    - e.g. `stretch [1, 2, 3, 2, 2] (|\| |\|-|)`
    - `= |-|\| | | |-|\|-|\|`
    - e.g. `stretch [3, 2, 4] (|-|\|)`
    - `= | | |-|\|-|-|-|\|`

This language is used to describe how circuits work. Information starts at the
top of each line and flows downwards.

Information is combined by joining lines, and applying the operation `(*)` "blob"!

e.g. `y = x[0] * x[1] * x[2]`

Information is duplicated when lines separate.

e.g. `x = y[0] = y[1] = y[2]`

## Deep Embedding

There are many different ways of interpreting this circuit language.

e.g.
    - Find the width of circuit
    - Find the height of circuit
    - Evaluate the output of circuit given input and `(*)`

We will start with a deep embedding. This is achieved in two steps:

- Create a data structure for the abstract syntax
- Define a semantics with an evaluation function

```
> data Circuit
>     = Identity Int
>     | Beside Circuit Circuit
>     | Above Circuit Circuit
>     | Fan Int
>     | Stretch [Int] Circuit
```
