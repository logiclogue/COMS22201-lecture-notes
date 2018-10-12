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

We can interpret the circuit language by stipulating that the semantics of a
term is the width of the circuit generated.

```
> type Width = Int
> width :: Circuit -> Width
> width (Identity n)   = n
> width (Beside c1 c2) = width c1 + width c2
> width (Above c1 c2)  = width c1 -- Choice of semantics here!
> width (Fan n)        = n
> width (Stretch ws c) = sum ws
```

We can give *multiple semantics* easily by supplying more evaluation functions.

For instance the height of the circuit is:

```
> type Height = Int
> height :: Circuit -> Height
> height (Identity n) = 1
> ;
> height (Above c1 c2) = height c1 + height c2
```

Sometimes the semantics are intertwined in a dependent way. For instance,
calculating if a circuit is well connected requires us to calculate the width
even though all we are interested in is a `Bool`.

```
> type Connected = Bool
```

But for this we need the width as well, we can use the `width` function for
this.

```
> connected :: Circuit -> Connected
> connected (Identity n)   = True
> connected (Beside c1 c2) = connected c1 && connected c2
> connected (Above c1 c2)  = connected c1 && connected c2
>     && width c1 == width c2
> connected (Fan n)        = True
> connected (Stretch ws c) = connected c && width c == length ws
>     && ((not . elem 0) ws)
```

## Shallow Embedding

In a shallow embedding we simply have to give a semantics in terms of the
operations directly.

```
type Width = Int

type Circuit = Width

identity :: Int -> Circuit -- NB the type synonyms make this an Int.
identity n = n

beside :: Circuit -> Circuit -> Circuit
beside c1 c2 = c1 + c2

above :: Circuit -> Circuit -> Circuit
above c1 c2 = c1

fan :: Int -> Circuit
fan n = n

stretch :: [Int] -> Circuit -> Circuit
stretch ws c = sum ws
```

Shallow is problematic because it is hard to add a different semantics. In order
to do so, we must redefine `Circuit`, but this might break any code that depends
on the old definition. Additionally, a dependent semantics requires all of the
interpretations to be considered at once. This is not compositional.

However in a shallow semantics it is easy to extend the language with new
operations since this involves adding new functions: nothing breaks. With a deep
embedding a new constructor must be added so all semantics must be extended
accordingly.

# The Expression Problem

The expression problem concerns itself with finding a solution to the following:

- Is it possible to extend the syntax and semantics of a language in a modular
  fashion.

For instance, consider the data type `Expr` from before:

```
data Expr = Val Int | Add Expr Expr
```

Here we want to extend the syntax by adding a new operation for multiplication,
but we do not want to modify any existing code. I.e. we cannot simply add a new
`Mul` constructor.

Similarly consider the semantics:

```
eval :: Expr -> Int
```

Again we want to extend the semantics without modifying the code.

(In this case, adding semantics is easy because we simply write another
function of type `Expr -> b`).

To solve the expression problem we will study the generalisation of folds called
a catamorphism. We do this because folds are a way of reducing data structures
in a composable way, and syntax trees are just data structures.

# Catamorphisms

Consider the fold for a list.

```
> data [a] = []
>          | a : [a]

> foldr :: b -> (a -> b -> b) -> [a] -> b
> foldr k f []     = k
> foldr k f (x:xs) = f x (foldr k f xs)
```

So the first step is to deconstruct the type of lists to expose its generic
structure.

The definition of lists is the same as the following where we remove the
syntactic sugar:

```
> data List a = Empty
>             | Cons a (List a)
```

We remove recursion from this datatype, and mark it with a parameter `k` for
`k`ontinuation.

```
> data ListF a k = EmptyF
>                | ConsF a k -- This shows us where List a was recursive
```

We now make the recursive parameter something we can change programatically by
giving a functor instance to `ListF a`

```
> instance Functor (ListF a) where
>     fmap :: (x -> y) -> ListF a x -> ListF a y
```

We now need to define a type that gives us the fixed point of data. This is
defined as follows:

```
> data Fix f = In (f (Fix f))
```

This datatype allows us to generalise all recursive datatypes (except neutrally
recursive).

For example, instead of `List a`, we can write `Fix (ListF a)`. To demonstrate
this, we show that `List a` and `Fix (ListF a)` are isomorphic:

```
> toList   :: Fix (ListF a) -> List a
> fromLIst :: List a -> Fix (ListF a)
```

We say that `List a` and `Fix (ListF a)` are isomorphic when:

`(toList . fromList) = id` and `(fromList . toList) = id`.

In other notation we write: `List a =~ Fix (ListF a)`

Let's define these functions:

```
fromList :: List a -> Fix (ListF a)
fromList Empty = In EmptyF
```
