/**
 * ## Stupid ##
 *
 * There's a Lithuanian/Russian card game called 'Stupid'.
 *
 * It has these (simplified) rules:
 *
 * Suites: Hearts, Diamonds, Clubs, Spades.
 * Card ranks: 2, 3, 4, 5, 6, 7, 8, 9, T, Jack, Queen, King, Ace.
 *
 * Cards are ordered by their rank and then their suite.
 */

I assume suites are ordered as listed above? H < D < C < S

/*
 * One suite is called the trump suite. Any trump card is always higher than
 * any non-trump card.
 *
 * The trump suite is represented as the first line in data file by a letter:
 * H|D|C|S.
 *
 * Two players get their cards which are represented by a line in the file:
 * ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9
 *
 * First player starts as offense. Offense always starts.
 *
 * 1) He plays smallest non-trump card. If he does not have non-trump card,
 * he plays smallest trump card then.
 * 2) If defense has cards of the same rank, he plays smallest of them
 * and passes the turn to offense. This can only be done if offense has enough
 * cards to defend all of them. Roles of the attacker/defender switch then.
 */

Do you mean, "This can only be done if *defense* has enough cards to defend
all of them"?

[edit] I understand now -- offense only becomes defense if his hand is
 >= # of uncovered cards.

/*
 * 3) Whenever there is no card to be passed,
 */

What does it mean to "pass" a card? Above, it is stated that the defense
"passes the turn to offense", which sounds like a different use of the
word.

[edit] I understand better now.

/*
 * defender must use
 * smallest non-trump cards that are bigger than the each card on the table
 * to cover them, starting with the smallest uncovered cards currently on
 * table. If he doesn't have a non-trump card to cover any card, he
 * can use a trump card to do that. If he doesn't have any card to cover current
 * card, he shouldn't play any more cards in this turn.
 */

Do I correctly understand that the defender must cover as many cards as
possible, whether or not he can cover all of them?

/*
 * 4) Attacker then can add more cards that are of the same rank that
 * currently exist on the table for defender to current. Maximum number of
 * non-covered cards on the table cannot exceed defenders current hand size.
 * If such scenario arises, attacker then plays the smallest card from their
 * hand.
 */

"current" is used as a verb here. Is that a typo? Should it be "cover"?

/*
 * 5a) If all cards are covered - defender wins this round, cards are
 * discarded and defense becomes offense for the next turn.
 * 5b) If not all cards are covered - go to 3). If defender can't cover all cards
 * it loses the round and takes all cards to his hand. It's offense turn.
 *
 * Repeat this until one player does not have cards in his hand after a turn.
 * That player wins.
 *
 * For each given line, print 0 if there is a tie (both players have no cards
 * in their hands), 1 if p1 wins, 2 if p2 wins.
 */

Upon rereading, I think I understand the game now. Now to check my
understanding with the example run...

It looks like you can only cover with cards of the same suite (or trump
cards). Remaining questions: Can you cover a higher ranked card with a
lower ranked trump card? I'll have check my results against those given, I
suppose.

---------------------

To do this problem, I want to experiment with a new design methodology,
using 'free monads'. This will let me easily write different interpreters
so I can both output something similar to the example runs, as well as
simply output a 1, 0, or 2 to test against the main results.

The challenge for me is to understand the concepts well enough to know the
right way in which to proceed. I know I'll want to create a "language" of
possible actions, and then write a program in that language that follows
the game's rules. Wait, is that it? Did I just grok the situation?

Possible actions include:

attack
pass
defend
reattack
end

There's one level up, too, where the program must read data, create game
state, and pass it to the game runner. Finally, there's the various
interpreters for the game logic program.

> import System.Environment (getArgs)
> import Control.Monad ((<=<))
> import qualified Data.Text.IO as T

> import Types
> import ParseInput

-- > main = runProg . makeInitialState =<< (readFile . head) =<< getArgs

Ah. Oh. This is wrong. The input gives the trump card and then it gives
multiple initial states. Thus the program flow is readTrump ->
readInitState -> runGame -> readInitState -> ...

For a lot of data, this would need a streaming library, but I don't want to
go there yet. I think I will be content to parse the trump card and the
list of input states, and then process each input state.

> main = do
>     inFile <- fmap head getArgs
>     input <- T.readFile inFile
>     mapM_ runProg (parseInput input)

> runProg :: GameState -> IO ()
> runProg = putStrLn . show

Great, now we can build our game state.

(See Types.hs)

Now a little parser for the data file.

(See ParseInput.hs)

Now to define the game actions.

(See Game.hs)
