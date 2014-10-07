(To run the code, look at [test.sh](test.sh))

stupid-game
===========

A program that plays the game 'stupid', which also explores using a Free
Monad for abstracting over access to the outside world.

I started out not knowing how to use a Free Monad at all, only having a
vague understanding of what it is used *for*. Now that I've completed the
task, I think it's fair to say that it was complete overkill. :) But it was
a fun ride, and I have a stronger understanding of *when* and *how* it
could be useful.

Now let me break down the code for you.

Introduction: Types
-------------------

Let's start by defining the types relevant to a game of 'stupid'. For that
we have [Types.hs](Types.hs), with definitions like

```haskell
data Suit = Heart | Diamond | Club | Spade
    deriving (Bounded, Enum, Eq, Ord)
```

Introduction: Main
------------------

One thing I find common in Haskell is the ability to "pseudocode" the high
level logic, and end up with the actual code required. `main` is a great
example. Besides `quietInterp` and `attackStage`, this is a straightforward
high-level description of reading input, parsing it, and processing it,
which needed very little change throughout its lifetime. Those two mystery
functions are expanded below.

```haskell
main = do
    inFile <- fmap head getArgs
    input <- T.readFile inFile
    mapM_ runProg (parseInput input)
    putStr "\r\n"

runProg :: GameData -> IO ()
runProg = flip quietInterp attackStage
```

Parsing
-------

The first part of this challenge is parsing the data out of [data.txt](data.txt). For
that task I used the attoparsec library. The parsers are found in
[ParseInput.hs](ParseInput.hs). They're outside the scope of this document, but here is
one example:


```haskell
-- "Two players get their cards which are represented by a line in the
-- file: ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9"
parseHandPair = (,)
    <$> sepBy parseCard " " <* parseSep
    <*> sepBy parseCard " "
```

I relied on a lot of standard Haskell patterns. Applicative stands out in
in this example.

Game Logic
----------

The majority of [stupid.hs](stupid.hs) defines the logic for playing 'stupid'. The
individual stages are broken into distinct definitions. Each one is written
as a meta-program using "keywords" from a domain-specific language (DSL),
developed below. Here's one example:

```haskell
passStage rank = do
    t <- getTrump
    nOff <- length <$> getHand Offense
    nTable <- tableSize
    card <- (minCard t . cardsRanked rank) <$> getHand Defense
    case (compare nOff nTable, card) of
        -- offense has enough cards && defense has one to play
        (GT, Just card') -> do
            passWith card'
            passStage rank
        -- else...
        _ -> defendStage

  where

    cardsRanked r = filter ((r ==) . cRank)
```

This once again uses a collection of standard Haskell patterns (Functor,
Monad, etc.) to describe the actions taken. You can see some of the the DSL
"keywords", like `getHand` and `passWith`.

The first stage of the game is naturally called `attackStage`. This is one
of the keywords found in `main`, where it is passed as the first step to the
meta-program's interpreter.

DSL
---

The DSL is defined in [DSL.hs](DSL.hs). Naturally.

While the code in [stupid.hs](stupid.hs) defines how to play 'stupid', this code
defines what actually happens to the state of the game during each player
action. Here's an example. Glaze your eyes a bit until I've explained a few
more things.

```haskell
winTurn role = do
    case role of
        Offense -> do
            allCards <- playedCards
            lift $ GS.updateHand Defense (++ allCards)
        Defense -> swapRoles
    lift $ GS.clearTable
    liftF $ WinTurn role ()
```

Now, hold on to your butts, because I want to point out that this
'stupid' DSL is actually implemented in terms of *another* DSL that
manipulates game state. That lower-level DSL is found in [GameState.hs](GameState.hs).
You can see it used above; for instance, `GS.clearTable`, which has this
definition:

```haskell
clearTable :: GameState ()
clearTable = do
  st <- get
  put st { table = [] }
```

Confusingly, besides describing the *what* of 'stupid', the code in
[DSL.hs](DSL.hs) also defines a generic interface to the outside world. The
interface is summarized in the `GameAction` type:

```haskell
data GameAction nxt
    = AttackWith Card nxt
    | PassWith Card nxt
    | Defend PlayedCard Card nxt
    | ReinforceWith [Card] nxt
    | WinTurn Role nxt
    | Result Int nxt
    deriving (Functor)
```

This type takes its shape from the requirements of using the Free Monad. I
won't say much more about that at present.

Interpreters
------------

Once the meta-program describing the game is fully written, it must be
interpreted to generate output. This is where `GameAction` and the Free
Monad come in to play. By using the Free machinery, intepreters are simply
written as a function keyed on `GameAction` values. Two interpreters exist in
[Interpreters.hs](Interpreters.hs): one that is noisy and one that is quiet. This is where
the second mystery keyword from `main` is defined: `quietInterp` is the
quiet interpreter. :)

Since this Free machinery is precisely what I set out to understand when
beginning this project, I will declare that it is "outside the scope of
this document." :) I will refer you to my
[main reference](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html),
and I promise I'll say a bit more about it at the end.

Oh wait, I'm already at the end.

The End
-------

Like I said in the beginning, some of this Free machinery was total
overkill for this challenge. There are benefits that would make it very
useful for other projects, however. For instance, the connection between
game logic and the outside world is entirely abstract. I could write
another interpreter that beams messages to space without changing the game
logic at all. Perhaps more importantly, I can *test the game logic in
isolation*.

Plus, it's kinda fun, once you learn to ignore the white noise.
