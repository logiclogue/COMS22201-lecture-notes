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

# 04/10/2018

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

# 08/10/2018

"Perfection is achieved, not when there is nothing more to add, but when there
is nothing left to take away" - Antoine de Saint-Exupery

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

# 11/10/2018

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
> fromList :: List a -> Fix (ListF a)
```

We say that `List a` and `Fix (ListF a)` are isomorphic when:

`(toList . fromList) = id` and `(fromList . toList) = id`.

In other notation we write: `List a =~ Fix (ListF a)`

Let's define these functions:

```
fromList :: List a -> Fix (ListF a)
fromList Empty = In EmptyF
```

# 15/10/2018

Some examples of values of type `Fix (ListF a)` are:

```
In EmptyF :: Fix (ListF a) -- This works because EmptyF :: ListF a k for any a
                           -- or k
    -- ListF a (Fix (ListF a))
```

Note that the type of `In` is:

```
In :: f (Fix f) -> Fix f
```

So the example above is where `f = ListF a`

```
In (ConsF 5 (In EmptyF))
            -- Fix (ListF a)
-- Fix (ListF a)
```

Fix two elements we have:

```
In (ConsF 6 (In (ConsF 7 (In EmptyF))))
```

So now we have enough to finish a definition of `fromList`:

```
> fromList :: List a -> Fix (ListF a)
> fromList Empty       = In EmptyF
> fromList (Cons x xs) = In (ConsF x (fromList xs))
                 -- a
                   -- List a
```

We are now ready to generalise fold to be a catamorphism:

Consider functions off type `ListF a b -> b`

```
h :: ListF a b -> b
h EmptyF      = k
h (ConsF a y) = f a y
    where
        k :: b
        f :: a -> b -> b
```

Functions of that type correspond to replacing the constructors of `ListF a`
with functions `k` and `f` just like in `foldr`.

A catamorphism arises from this diagram:

```
           fmap (cata alg)
        f (Fix f) ------> f b 
     In | ^ inop           | alg
        v |                v
      Fix f -------------> b
               cata alg
```

The function `inop` is the opposite of `In`. We definite it by the following:

```
> inop :: Fix f -> f (Fix f)
> inop (In x) = x
           -- f (Fix f)
```

So finally we can write the function `cata` by chasing the arrows of this
square.

```
> cata :: Functor f => (f b -> b) -> Fix f -> b
> cata alg x = (alg . fmap (cata alg) . inop) x
```

An alternative and equivalent definition is:

```
> cata :: Functor f => (f b -> b) -> Fix f -> b
> cata alg (In x) = alg (fmap (cata alg) x)
```

To use this, we only need to supply the `alg`.

We will define the function `toList` using a `cata`:

```
> toList :: Fix (ListF a) -> List a
> toList = cata alg
>     where
>         alg :: ListF a (List a) -> List a
>         alg EmptyF       = Empty
>         alg (ConsF x xs) = Cons x xs
                     -- a
                       -- List a
```

Here is another function:

```
> toList' :: Fix (ListF a) -> [a]
> toList' = cata alg
>     where
>         alg :: ListF a [a] -> [a]
>         alg EmptyF       = []
>         alg (ConsF x xs) = x:xs
                     -- a
                       -- [a]
```

# 18/10/2018

We can also define a function that returns the length of a `Fix (ListF a)`:

```
> length :: Fix (ListF a) -> Int
> length = cata alg where
>     alg :: ListF a Int -> Int
>     alg EmptyF      = 0
>     alg (ConsF x y) = 1 + y
                   -- Int
```

Here is an example of evaluation:

```
length (In (ConsF 7 (In (ConsF 9 (In EmptyF)))))
= {length}
cata alg (In (ConsF 7 (In (ConsF 9 (In EmptyF)))))
= {cata}
alg (fmap (cata alg) (ConsF 7 (In (ConsF 9 (In EmptyF)))))
= {fmap}
alg (ConsF 7 (cata alg (In (ConsF 9 (In EmptyF)))))
= {alg}
1 + cata alg (In (ConsF 9 (In EmptyF)))
= {cata}
1 + alg (fmap (cata alg) (ConsF 9 (In EmptyF)))
= {fmap}
1 + alg (ConsF 9 (cata alg (In EmptyF)))
= {alg}
1 + 1 + cata alg (In EmptyF)
= {cata}
1 + 1 + alg (fmap (cata alg) EmptyF)
= {fmap}
1 + 1 + alg EmptyF
= {alg}
1 + 1 + 0
= {(+)}
2
```

```
instance Functor (ListF a) where
    fmap :: (x -> y) -> ListF a x -> ListF a y
    fmap f EmptyF      = EmptyF
    fmap f (ConsF a x) = ConsF a (f x)
```

Now another example, of summing a list:

```
> sum :: Fix (ListF Int) -> Int
> sum = cata alg where
>     alg :: ListF Int Int -> Int
>     alg EmptyF      = 0
>     alg (ConsF x y) = x + y
                   -- Int
```

# Peano Numbers

A peano number is either zero, or the successor of another peano number.

```
> data Peano = Z
>            | S Peano
```

So the number 3 is written `S (S (S Z))`

* Step 1. Make a signature functor for `Peano`.

```
> data PeanoF k = Z
>               | S k -- we identified the recursive call in Peano
```

* Step 2. Define a functor instance for `PeanoF`.

```
> instance Functor Peano where
>     fmap :: (a -> b) -> Peano a -> Peano b
>     fmap f Z     = Z
>     fmap f (S x) = S (f x)
```

* Step 3. Profit, write functions using `cata`.

```
toInt :: Fix PeanoF -> Int
toInt = cata alg where
    alg :: PeanoF Int -> Int
    alg Z     = 0
    alg (S x) = 1 + x
```

Now we can define a doubling function:

```
> double :: Fix PeanoF -> Fix PeanoF
> double = cata alg
>     alg :: PeanoF -> Fix Peano
>     alg Z     = In Z
>     alg (S x) = In (S (In (S x))) -- We have doubled the number of S
             -- Fix PeanoF
```

# 22/10/2018

# Composing Languages

Previously, we looked at the following datatype as the language for addition:

```
> data Expr = Val Int
>           | Add Expr Expr
```

We learnt to extract the signature functor for this by locating recursive calls.

```
> data ExprF k = ValF Int
>              | AddF k k
```

The `Fix ExprF` datatype is essentially `Expr`:

```
Fix ExprF = Expr
```

Suppose we want to add multiplication to this language, we need a way to extend
`ExprF` with more constructors. This is achieved by the coproduct of functors.

The coproduct functor is defined as:

```
> data (f :+: g) a = L (f a)
>                  | R (g a)
```

This takes two functors `f` and `g` and makes the functor `(f :+: g)`. It
introduces these constructors:

```
L :: f a -> (f :+: g) a
R :: g a -> (f :+: g) a
```

The `Functor` instance is defined as follows:

```
> instance (Functor f, Functor g) => Functor (f :+: g) where
>     fmap :: (a -> b) -> (f :+: g) a -> (f :+: g) b
>     fmap f (L x) = L (fmap f x)
           -- a -> b   ---------- f b
                -- f a
>     fmap f (R y) = R (fmap f y)
                -- g a ---------- g b
```

Now we are ready to define the signature functor for multiplication:

```
> data MulF k = MulF k k
```

This introduces the datatype constructor:

```
MulF :: k -> k -> MulF k
```

We define its `Functor` instance:

```
> instance Functor MulF where
>     fmap f (MulF x y) = MulF (f x) (f y)
```

Finally, we can put the `ExprF` and `MulF` languages together to have a language
with both addition and multiplication:

```
Fix (ExprF :+: MulF)
```

This is essentially the same as describing the following datatype, but in a
compositional way:

```
> data Expr' = Val' Int         --
>            | Add' Expr' Expr' -- from ExprF
>            | Mul' Expr' Expr' -- from MulF
```

For practical purposes, we do not work with `Expr'`, but with `Fix (ExprF :+:
MulF)`.

We need to write algebras of the form:

```
ExprF :+: MulF b -> b
```

to reduce a `Fix (ExprF :+: MulF)` tree to a `b`.

To do this in a compositional way, we define a way of combining `ExprF` algebras
and `MulF` algebras: we call this the *junction* of algebras:

```
> (\/) :: (f a -> a) -> (g a -> a) -> ((f :+: g) a -> a)
> (falg \/ galg) (L x) = falg x
   ---- f a -> a    -- f a
                 ----- f :+: g a
           ---- g a -> a ------ a
> (falg \/ galg) (R y) = falg y
                    -- g a
```

So now we can give a semantics to the language `Fix (ExprF :+: MulF)` by
defining algebras.

```
> add :: ExprF Int -> Int
> add (ValF x)   = x
> add (AddF x y) = x + y
```

```
> mul :: MulF Int -> Int
> mul (MulF x y) = x * y
```

To evaluation, we write

```
> eval :: Fix (ExprF :+: MulF) -> Int
> eval = cata (add \/ mul)
```

Hurrah, we have solved the expression problem!

In fact, we can decompose the `ExprF` into constituent parts:

```
> data ValF k = ValF Int
> data AddF k = AddF k k
```

After defining `Functor` instances, we can define algebras for:

```
Fix (ValF :+: AddF :+: MulF)
```

# 25/10/2018

# Parsers

A parser of things is a function of strings
To lists of pairs of things and strings
    - Graham Hutton

# Backus-Naur Form

Backus-Naur Form (BNF) is a language used to express the shape of grammars. It
was invented in around 1958 for the expression of the Algol programming
language.

A BNF statement is made up of:

```
ε     represents empty strings
<n>   represents a non-terminal
"x"   represents a terminal
p | q represents the choice between p, q.
```

An example of BNF is the following:

```
<digit> ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
        -- "is defined to be".
```

We can approximate numbers by this:

```
<num> ::= <digit>
        | <digit><number>
```

The core language of BNF is usually extended with some constructs:

```
[e]  optional e
(e)  grouping e
e*   0 or more repetitions of e
e+   1 or more repetitions of e
```

For a more complex example, consider expressions:

```
<expr> ::= <num>
         | <num>"+"<expr>
```

This corresponds to the following type:

```
> data Expr = Val Num
>           | Add Num Expr
```

In principle, we do the same to convert `<num>` into a `Num` datatype. However,
we will approximate this by the type `Int`.

```
> type Num = Int
```

Grammars can sometimes be ambiguous: a single string can be accepted by the
grammar in multiple ways:

```
<expr> ::= <num>
         | <expr>"+"<expr>
```

The problem here is also that `<expr>` is left-recursive: there is branch which
has an `<expr>` before any terminal.

This causes problems in recursive-descent parsers, which we will study later.

The solution is to refactor the grammar.

# Paull's Modified Algorithm

We can remove recursion as follows. Suppose we have the following grammar:

```
A ::= Aα₁|...|Aαn|β₁|...|βm
```

Where `A` is a non-terminal and `αi`, `βj` are BNF expressions.

To apply the algorithm, we rewrite the term above to be the following:

```
A  ::= β₁A'|...|βmA'α
A' ::= α₁A'|...|αnA'|ε
```

In practice have is how we convert the following:

```
           ----- β
<expr> ::= <num>
         | <expr>"+"<expr>
    |      --- A --------- α
    v
<expr>  ::= <num><expr'>
<expr'> ::= "+"<expr><expr'>|ε
```

# 29/10/2018

# Parsers

A parser is a function that takes in a list of characters, and returns an item
that was parsed, along with the unconsumed string.

We can define it by:

```
> newtype Parser a = Parser (String -> [(a, String)])
```

We can use a parser by defining a function `parse`:

```
> parse :: Parser a -> String -> [(a, String)]
> parse (Parser px) = px
                -- String -> [(a, String)]
```

Now we define combinators that allow us to build parsers:

```
> fail :: Parser a
> fail = Parser (\ts -> [])
```

This parser always fails to parse:

```
parse (fail) "Hello" = []
```

```
> item :: Parser Char
> item = Parser (\ts -> case ts of
>       []      -> []
>       (t:ts') -> [(t, ts')])
                   ---------- [(Char, String)]
```

Here we have:

```
parse (item) "Hello" = [('H', "ello")]
```

Sometimes it is useful to look at the input stream without consuming anything:

```
> look :: Parser String
> look = Parser (\ts = [(ts, ts)])
```

```
parse (look) "Hello" = [("Hello", "Hello")]
```

Often, we want to transform our parsers from producing values of one type to
another.

For instance we might transform a parser for a single `Char` into producing the
corresponding `Int`.

This is achieved by giving a functor instance for parsers:

```
> instance Functor Parser where
>     --fmap :: (a -> b) -> Parser a -> Parser b
>     fmap f (Parser px) =
                     -- String -> [(a, String)]
>         Parser (\ts -> [(f x, ts')
>                        | (x, ts') <- px ts])
```

Here is a diagram of what is happening:

```
|-------------------------------| ts
--------- |---------------------| ts'
    x
    |
    v
   f x
```

We can use this to define a parser for `Int`s from our `item` parser.

Now we introduce some combinators:

```
(<$>) :: (a -> b) -> Parser a -> Parser b

f <$> px = fmap f px
```

The following variation is often useful:

```
(<$) :: a -> Parser b -> Parser a
x <$ py = fmap (const x) py
```

We can use this to build a function called `skip` that parses input, but outputs
nothing useful.

```
> skip :: Parser a -> Parser ()
> skip px = () <$ px
```

Now we want to apply a function to the different outputs of the parse:

```
|------------------------------------------------------| ts
----------------------- |------------------------------| ts'
f :: (a -> b -> c -> d)  ------ |----------------------| ts''
:                         x :: a ------ |--------------| ts'''
:                                y :: b  ------ |------| ts''''
:                                        z :: c :
:-----------------------------------------------:
            f x y z :: d
```

To make something like this we use a combination of `<*>` and `<$>` like this:

```
pf <*> px <*> py <*> pz
```

This uses the `(<*>)` operation, which we will define shortly.

The `Applicative` class introduces pure and `(<*>)`:

```
> class Functor f => Applicative f where
>     pure  :: a -> f a
>     (<*>) :: f (a -> b) -> f a -> f b
```

# 01/11/2018

These have the following definitions:

```
> instance Applicative Parser where
>     --pure :: a -> Parser a
>     pure x = Parser (\ts -> [(x, ts)])
```

The `pure x` parser will not consume any input, but always generate the value `x`.

```
parse (pure 5) "Hello" = [(5, "Hello")]
```

Now we define `<*>`, pronounced "ap" for "apply".

```
>     --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
>     Parser pf <*> Parser px = Parser (\ts -> 
>                       [(f x, ts'')
>                       | (f, ts')  <- pf ts
>                       , (x, ts'') <- px ts'])
```

```
|----------------------------------------------| ts
:-------------:|-------------------------------| ts'
:      f        :-------:|---------------------| ts''
:                   x   :
:-----------------------:
           f x
```

The operation we defined first parses a function, then a value, and then applies
the function to the value.

This can be done the other way around:

```
> (<**>) :: Parser a -> Parser (a -> b) -> Parser b
> Parser px <**> Parser pf = Parser (\ts -> 
>               [(f x, ts'')
>               | (x, ts')  <- px ts
>               , (f, ts'') <- pf ts'])
```

```
|--------------------------------| ts
:------:|------------------------| ts'
:  x    :---------:|-------------| ts''
:            f     :
:------------------:
        f x
```

Other derived operators are `(<*)` and `(*>)`. Their types are:

```
> (<*) :: Parser a -> Parser b -> Parser a
> (*>) :: Parser a -> Parser b -> Parser b
```

The `Monoidal` class is equivalent to `Applicative`:


# Monoidal

```
> class Monoidal f where
>     unit :: f ()
>     mult :: f a -> f b -> f (a, b)
```

For parsers the `Monoidal` instance is as follows:

```
instance Monoidal Parser where
    --unit :: Parser ()
    unit = Parser (\ts -> [((), ts)])

    --mult :: Parser a -> Parser b -> Parser (a, b)
    mult (Parser px) (Parser py) = Parser (\ts ->
                    [((x, y), ts'')
                    | (x, ts')  <- px ts
                    , (y, ts'') <- py ts'])
```

```
|-------------------------------| ts
:---------:|--------------------| ts'
:    x     :------:|------------| ts''
:             y   :
:-----------------:
      (x, y)
```

It is useful to make `mult` a binary operation, so we introduce one:

```
> (<~>) :: Monoidal f => f a -> f b -> f (a, b)
> px <~> py = mult px py
```

We derive these useful combinators:

```
> (<~) :: Monoidal f => f a -> f b -> f a
> px <~ py = fst <$> (px <~> py)
```

```
> (~>) :: Monoidal f => f a -> f b -> f b
> px ~> py = snd <$> (px <~> py)
```

Where `fst (x, y) = x`
      `snd (x, y) = y`

Note that we have this equivalence:

```
(<~) = (<*)
(~>) = (*>)
```

# Alternatives

Now we produce parser that can deal with choice in a grammar.

```
> class Alternative f where
>     empty :: f a
>     (<|>) :: f a -> f a -> f a
```

Here is the instance for parsers:

```
instance Alternative Parser where
    --empty :: Parser a
    empty = fail -- from before.

    --(<|>) :: Parser a -> Parser a -> Parser a
    Parser px <|> Parser py = Parser (\ts ->
                        px ts ++ py ts)
```

```
|-----------------------| ts
:------:|---------------| ts'
  x, y
```

# 05/11/2018

```
parse px ts ++ parse py ts = parse (px <|> py) ts
```

Sometimes we want to extend `<|>` to many input parsers.

```
> choice :: [Parser a] -> Parser a
> choice pxs = foldr (<|>) empty pxs
```

We can define a combinator that appends the result of a parse onto others:

```
> (<:>) :: Parser a -> Parser [a] -> Parser [a]
> px <:> pxs = (:) <$> px <*> pxs
```

To understand this, first recall that most parser combinators are
left-associative:

```
(:) <$> px <*> pxs        | Note:
=                         | (<$>) :: (a -> b) ->
((:) <$> px) <*> pxs      |         f a -> f b
:--- a -> [a] -> [a]      | (<*>) :: f (a -> b)
:        -- Parser a      |      -> f a -> f b
:  = (a -> [a]) -> [a]    |
:---------:      : :      |
Parser ([a] -> [a]):      |
:                :-:
:               Parser [a]
:-----------------:
    Parser [a]
```

Now we are ready to define combinators that correspond to `+` and `*` from BNF:

```
e+ is written some e
e* is written many e
```

```
> some :: Parser a -> Parser [a]
> some px = px <:> many px
```

This parsers one `px` and appends it to the result of `many px`.

```
> many :: Parser a -> Parser [a]
> many px = some px <|> empty
```

# Monadic Parsing

Sometimes we want the control flow of a parser to depend on what was parsed.

Suppose we have `px :: Parser a`, we can define a function `f :: a -> Parser b`.
The function `f` inspects the value `x` which came from `px`, and produces a new
parser accordingly. The result should be a parser of type `Parser b`.

```
> instance Monad Parser where
>     --return :: a -> Parser a
>     return = pure -- from applicative
>     --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
>     Parser px >>= f = Parser (\ts ->
>             concat [parse (f x) ts'
>                         | (x, ts') <- px ts])

|----------------------------| ts
:-------:|-------------------| ts'
    x    :-------------------:
                  f x
```

To use this combinator we combine a parser with a function:

The satisfy parser takes in a function that is a predicate on `Char`s and
returns the parse value if it satisfies the predicate:

```
> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = item >>= \t -> if p t
>                             then pure t
>                             else empty
```

This is perhaps the most useful combinator. Rather than the monadic definition,
we can write one directly:

```
> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = Parser (\ts -> case ts of
>                 []      -> []
>                 (t:ts') -> [(t, ts') | p t])
                             ----------------
                 this is equivalent to:
                    if p t then [(t, ts')]
                           else []
```

We can now parse a single character as follows:

```
> char :: Char -> Parser Char
> char c = satisfy (c ==)
     or in other words:
> char c = satisfy (\c' -> c == c')
```

Example: `parse (char 'x') "xyz" = [('x', "yz")]`

# 08/11/2018

```
> module Lecture12 where

> import Text.Yoda
> import Data.Char
```

Let's start with a left-recursive grammar:

```
    expr = term | expr "+" term
    term = fact | term "*" fact
    fact = numb | "(" expr ")"
    numb = "0" | "1" | "2"
```

We must remove the left recursion by applying Paull's modified
algorithm:

```
    expr  = term expr'
    expr' = "+" term expr' 
          | epsilon

    term  = fact term'
    term' = "*" fact term' | epsilon

    fact = numb | "(" expr ")"
    numb = "0" | "1" | "2"
```

The other rules remain the same.

We now make datatypes that correspond to these rules.
Each nonterminal corresponds to a new datatype. The alternations
correspond to constructors for that type.

```
> data Expr  = ExprTerm Term Expr'
>   deriving Show

> expr :: Parser Expr
> expr = ExprTerm <$> term <*> expr'

    expr' = "+" term expr' 
          | epsilon

> data Expr' = Expr'Add Term Expr'
>            | Expr'Emp
>   deriving Show

> expr' :: Parser Expr'
> expr' = Expr'Add <$ char '+' <*> term <*> expr'
>     <|> Expr'Emp <$ unit

    term  = fact term'

> data Term  = TermFact Fact Term'
>   deriving Show

> term :: Parser Term
> term = TermFact <$> fact <*> term'

    term' = "*" fact term' 
          | epsilon

> data Term' = Term'Mul Fact Term'
>            | Term'Emp
>   deriving Show

> term' :: Parser Term'
> term' = Term'Mul <$ char '*' <*> fact <*> term'
>     <|> Term'Emp <$ unit

    fact = numb 
         | "(" expr ")"

> data Fact = FactNumb Int
>           | FactPar Expr
>   deriving Show

> fact = FactNumb <$> numb
>    <|> FactPar <$ char '(' <*> expr <* char ')'

    numb = "0" | "1" | "2"

> numb :: Parser Int
> numb  = read <$> digits

> digit :: Parser Char
> digit = satisfy isDigit

> digits :: Parser String
> digits = cull (some digit)
```

Now we can test this with statements such as:

```
    parse (expr) "(3+2)*45"
```

It is possible to remove spurious results by applying `eof` at the
end:

```
   parse (expr <* eof) "(3+2)*45"
```
