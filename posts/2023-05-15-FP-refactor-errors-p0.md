---
title: Refactoring error messages in Haskell programs. Who cares about K9 composers?
featured: true
summary: Concerns about the quality of error output in Haskell programs, one more time.
toc: true
tags: patterns-of-erroneous-code, Haskell, error-messages
---

> *Great programming is mathematics. ... Except, all falsehoods are the same and error messages are not. Otherwise, great programming is mathematics.*   
>  &emsp; &emsp;   A quote that I wish someone whom I could quote had said


**DRAFT version:** _Awaiting Initial Feedback._

In this post I will talk about improving error messages.  We will also discuss dogs which compose music. 

I wrote about error messages in Haskell before, I decided to give it one more go. 
I am working on a slowly progressing task: rewriting code to improve the quality of error messages across projects I contribute to (... and log messages too, but I will focus on the error outputs here). I try to dedicate a few hours every sprint to it. 
This work often includes rethinking parts of _aeson_ or _Parsec_ code that use `MonadPlus` / `Alternative` when the resulting error message is likely to throw anyone for a loop, or re-implementing code that uses `Maybe` where something like `Either` would be a better choice, or where errors were never caught...  This work also involves adding a decent amount of context to the messages.  I have been trying to fix up the errors for several years now and I am starting to believe that this work may never end.  You roll this rock uphill and it rolls back down. Can Functional Programming create quality error outputs?  Of course it can! 
But, for this to happen on the level of projects ... I think that the community needs to talk about it more.  

The (low) quality of error messages I witness in functional code is something that has puzzled me for a very long time. I wrote about [_add_blank_target Maybe Overuse](2021-01-17-maybe-overuse.html) and [_add_blank_target Alternative Overuse](2021-02-13-alternative.html) in the past. The first received a very mixed response (including very positive and very negative), the response to the second was flat negative. I decided that the reasons for what I am observing are probably mostly not technical. This (at least partially) motivated me to look into cognitive psychology ([_add_blank_target Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html)), and I came up with "a theory" about [_add_blank_target Theorists vs Pragmatists](2022-11-07-empirical-programming.html).  I cannot claim that I understand what is happening, I can only claim spending literally years thinking about it. 

I want to try one more time to talk about my experience with errors and troubleshooting with some examples and thoughts. My current plan is that with this series (or with this post) I will end my blogging. 

This post is also about conveniences.  It shows a few examples where established parts of the Haskell ecosystem make it easy to be careless about errors or where providing decent error messages is simply hard.  

I will mostly focus on _aeson_ (the premier Haskell package for working with JSON) with short mentions outside of this library.  This is because code that uses _aeson_ has been my more recent refactoring effort and is fresh on my mind. 

## Historical notes

These are my (Haskell user) observations about the history of error messages in the Haskell ecosystem. 
If you have been using Haskell for a long time, you probably remember that _aeson_ did not have `eitherDecode` at the beginning. `eitherDecode` was added in `0.6.1.0` (about two years after the initial release).  What it did have (and unfortunately still does) is a more nicely named

```Haskell
decode :: FromJSON a => ByteString -> Maybe a
```

If I did my hackage archaeology correctly, an ability to output error messages was added in `0.2.0.0` with the 
introduction of `parse :: (a -> Parser b) -> a -> Result b` which has been hiding in [_add_blank_target `Data.Aeson.Types`](https://hackage.haskell.org/package/aeson-0.2.0.0/docs/Data-Aeson-Types.html).
The commonly imported `Data.Aeson` module did not have an error message producing combinator until `0.6.1.0`.

If you look over the documentation of the older versions of _aeson_ you will see the following code as the suggested implementation for [_add_blank_target `FromJSON`](https://hackage.haskell.org/package/aeson-0.6.0.0/docs/Data-Aeson.html#t:FromJSON):

```Haskell
-- A non-Object value is of the wrong type, so use mzero to fail.
   parseJSON _          = mzero
```

I am still finding (and fixing) similar code despite a past effort to eradicate these.  It is not easy to troubleshoot a bug if the message handed to you says only "mzero".  

With respect to error messages, _aeson_ clearly went a long way since the old days.
If you look at aeson's Haddock [_add_blank_target today](https://hackage.haskell.org/package/aeson-2.1.2.1/docs/Data-Aeson.html#t:FromJSON) you will find the use of `mzero` discouraged!:

> "The basic ways to signal a failed conversion are as follows:   
> 
> * `fail` yields a custom error message: it is the recommended way of reporting a failure;
> *  `empty` (or `mzero`) is uninformative: use it when the error is meant to be caught by some `(<|>)`;
>  * `typeMismatch` can be used to report a failure when the encountered value is not of the expected JSON type; unexpected is an appropriate alternative when more than one type may be expected, or to keep the expected type implicit.
>
> `prependFailure` (or `modifyFailure`) add more information to a parser's error messages."

However, I still find the recommended use of `<|>` for working with errors an odd design choice. I will explain shortly why.

There are other libraries where an ability to get or provide crucial error information has been added only recently (e.g _servant-multipart_).  At the same time, there are many examples where `Maybe` has been overused in the past and still is. My [_add_blank_target Maybe overuse](2021-01-17-maybe-overuse.html) post has a few examples like these[^parsec]. 

[^someex]: I am sure some readers are going to point out the sophisticated open union approach that went into the design of Haskell exceptions. I agree.  


## Criticism outlined

**`Maybe` criticism:**  Legacy `Maybe` combinators should be causing some concern. In programming, legacy is inertia. `Maybe` is not the correct type to represent something like a parsing failure, it can be useful to describe missing data but not for situations where _we care_ about what went wrong (like parsing errors). 
_A decoding function that returns `Maybe` should be marked deprecated and eventually removed._ 
Functions like these are found in many libraries, not just _aeson_, and this is not just about parsing. 
One can even see it as a pattern across the whole Haskell ecosystem. 

Anyone in a desperate need of dropping the error information can do that with an easy to create natural transformation like:

```Haskell
errInfoDon'tCare :: Either e a -> Maybe a
```
 
I am not trying to be sarcastic, IMO _"who cares?"_ is a fair question to ask. It would be loud enough and useful in PR reviews.   

**Hyrum's Law and friends:** 
If you believe there is some truth to the [_add_blank_target Hyrum's Law](https://www.hyrumslaw.com/) (the law which states that 
all, even the unintended ways to use a library will be exploited by its users) you will probably agree with my stance on this. 
I like to think about Hyrum's Law using words that end with "use": use, overuse, misuse, and abuse. Programming concepts often end up being misused and abused, it is enough for a library like _aeson_ to provide an opening. 

I do believe in (or rather, have been observing) something similar to Hyrum's Law, namely:

> &emsp;  _Developers are likely to choose convenience over correctness_

I call it _The Law of Convenience_ and note that `Maybe` is much more convenient to use then `Either`.   

I also believe that writing code has a significant habitual factor. Ignoring error messages is a concerning habit to have. 

And, finally, I believe that major libraries lead the ecosystem by example.

Haskell is converting from a research language to a language that is used commercially and topics like efficient ability to troubleshoot production issues are becoming important.
The changes I am observing are good, I only hope that the community will get more aggressive on this front.   
In particular, it would be nice to see more error types that are semantically richer than `String`. We do not want `String` to become the type of choice for errors and I am happy to see when this is not the case (Megaparsec, yaml, amqp, ...). I would also love to see more of standard type level consideration for errors (e.g. standard typeclasses for working with them that go beyond the `Exception` typeclass)[^someex]. 

[^parsec]:
Of course, _aeson_ historical record cannot be generalized to all libraries (e.g. Parsec was clearly concerned about error outputs from day one.). 


## MonadPlus error laws

I am fixing a lot of code that uses `Alternative` / `MonadPlus` abstractions.  The next section will show a code that produces wrong error messages by misusing these abstractions.  In this section I will discuss `MonadPlus` in more general terms. 

`MonadPlus` is a very convenient and easy to use `Monoid` like abstraction.  It comes with `mzero` which 
is often used to represent a failure without any error information.  It is supposed to be a principled abstraction 
that needs to follow certain monoid-like laws (see [_add_blank_target `MonadPlus`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad.html#t:MonadPlus), [_add_blank_target Laws](https://wiki.haskell.org/Typeclassopedia#Failure_and_choice:_Alternative.2C_MonadPlus.2C_ArrowPlus)).  Does this abstraction play well with computations that also can emit nontrivial errors?

To dig this rabbit hole a little deeper, let's try to test the second law for _mzero_ (`v >> mzero = mzero`) polymorphically by adding `MonadFail` constraint: 

```Haskell
tst :: (MonadFail m, MonadPlus m) => m b
tst = fail "not mzero" >> mzero
```
  
Now I can try it with different monads to see if its error output is the same as `mzero`'s. E.g.:

```Haskell
{-# LANGUAGE TypeApplications #-}

import qualified Text.Megaparsec as MP
import Data.Void

-- |
-- >>> verifyMP
-- False
verifyMP :: Bool
verifyMP = runTest tst == runTest mzero 
  where 
    runTest p = MP.parse @Void @String @Int p "test" ""
```

_side_note_start
Side note: [_add_blank_target Megaparsec haddock](https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec.html#t:ParsecT) on the `MonadPlus` instance of `ParsecT` states: 

>  _"strictly speaking, this instance is unlawful. The right identity law does not hold, e.g. in general this is not true:
   `v >> mzero = mero`. However the following holds: `try v >> mzero = mzero`"_

Obviously, there is no magic here, backtracking or not the error message from `try v >> mzero` may be different than `mzero`, making a simple change to the above test verifies this as well. 
_side_note_end

Examples that fail  _"`tst` output is the same as `mzero` output"_ tests:

* `IO`
* `Parser` from `Data.Aeson.Types`
* `Parec` from say `Text.Megaparsec`
* `Parser` from _attoparsec_

Examples that pass such test:

* `Maybe`
* `ReadP` and `ReadPrec` from `Text.ParserCombinators`

`Maybe` has no error information, `Text.ParserCombinators` implement `mzero` as a [_add_blank_target no-message failure](https://hackage.haskell.org/package/base-4.18.0.0/docs/src/Text.ParserCombinators.ReadP.html#P).  

**Question:** Can we find an example where a monadic computation allows for nontrivial error messages
and passes this test?   

**Answer:** For a failing computation `v`, we would expect[^faillaw] `v >> anything = v`. This, combined with 
the second mzero law (`v >> mzero = mzero`) implies that any failing computation is equivalent to `mzero`. 
So, we either need to think about the second mzero law "modulo errors" or we have to accept that 
any lawful `MonadPlus` computation will suppress error information.   

[^faillaw]: e.g. [_add_blank_target Monad Fail Law](https://wiki.haskell.org/Typeclassopedia#MonadFail)


I believe developers are divided into these 2 camps:  those that think about and implement laws, and those who do not, but are nevertheless surprised when computations behave in unlawful ways.  We consciously or subconsciously assume various computational properties when we reason about the computations. Partially lawful is concerning. If you care about error output, "lawful modulo errors" should be concerning, having such limitation undocumented is concerning too.

Principled computations give us abstractions to work with, like theorems are tools to a mathematician.  We do not need to think about the details, just apply them to create new code.  When we do that with `MonadPlus` error messages can fall through the cracks.  I am dealing with quite a bit of code that has fallen into this trap.  Next section will show one such example. 

**_Who cares_** about errors?:  I hope we will come up with principled abstractions that are error message friendly. 
I am looking forward to a day where _aeson_ will stop recommending the use of `<|>` as an error message signaling abstraction.   

_side_note_start
On a more positive note: A somewhat more principled (and interesting) approach would seem to be to
require monoidal structure on the underlying error messages.  We would require that `mzero` results in `mempty` error, and `f1 <|> f2` results in `e1 <> e2` if both fail.  Alternative can be viewed as a "higher order monoid", it only makes sense that its errors should be a Monoid as well.  Note `(Monad m, Monoid e) => MonadPlus (ExceptT e m)`. 
However, _appending error messages tends to produce not very user friendly results.
_side_note_end

_side_note_start
Side note: (Mostly for grins.) Polymorphic implementation of the proposed `errInfoDon'tCare` combinator.

```Haskell
errInfoDon'tCare :: Alternative f => Either e a -> f a
errInfoDon'tCare = either (const empty) pure
```

It dumps any error information you might have. 
_side_note_end

## Alternative dog music. A use of `<|>` is considered harmful

Let's sketch a contrived code to illustrate a use of `<|>`:

```Haskell
data Pet = MkPet {
  breed :: Breed
  , species :: Species
  , petname :: Text
}

data Composer = MkComposer {
    genre :: Genre 
    , composername :: Text
}

data Favorite = 
  MkFavoritePet Pet
  ... -- there are other favorite things 
  | MkFavoriteComposer Composer

-- Constituent types (Pet, Composer) intances are not shown
-- Assume these types have unique (different) JSON representations

instance FromJSON Favorite where 
  parseJSON v = 
     MkFavoritePet <$> (A.parseJSON v) 
     <|> ... -- parse other things
     <|> MkFavoriteComposer <$> (A.parseJSON v)   
```

Note the above _Law of Convenience_ applies here: this code reuses existing JSON parsers to create the parser for `Favorite`, and this parser is very easy to implement.  This also looks elegant, and seems to principally fit the `Alternative` very well. There is no special JSON representation of `Favorite`, 
rather we use JSON representations of the constituent types `Pet`, `Composer`, etc.  This approach does not fuss with data constructor tags eliminating some JSON size overhead and
looks ideal for structurally typed callers (e.g. TypeScript). But, this approach has issues. 

Assume this has some frontend UI. Assume that a user enters information about her favorite four legged friend and that does not parse for some reason (e.g. frontend JSON encoding of `Pet` is incorrect). 
The error message from the parser will say something like 

```
"Composer needs a genre"
```   

(or whatever error `FromJSON` for `Composer` returns if it is given an unexpected JSON object). 

We see a couple of problems: the message is misleading and it lacks context (there is nothing in this message to indicate that it came from the JSON parser for `Favorite`). 
I will focus on it being misleading because, believe me, this coding pattern can produce very confusing errors in real life. Code like this is something I am slowly working to fix in projects I contribute to. Fixing such code is often not easy. 

_side_note_start
Common solution to the above parsing issue is to add a tag to JSON data to disambiguate the data constructor. 
In fact, this is what happens if you use default generic JSON instances. You no longer use `<|>` if you know
which constructor is being parsed.   
However, fixing such code gets more tricky if you have to consider backward compatibility, or when parsing into an extensibly defined (e.g. using something like _vinyl_) coproduct type (basically when adding tags to JSON representation is harder). In worst cases returning error messages from all alternatives may need to be considered (not a user frienly option but better than lying).  

**Exercise:** Try to implement JSON boilerplate for [_add_blank_target Data.Functor.Sum](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Sum.html) that would be friendly for non-Haskellers and provide clear error messages ("InL" and "InR" tags would not be very friendly). (I do not have a good solution.)

Adding tags to JSON representation of constituent types can also be considered[^ts].
_side_note_end

[^ts]: E.g. in structurally typed environments there are no data constructors. Adding a type disambiguating property to all objects in the union types is a programming pattern in TypeScript. 

_side_note_start
The alternative game is somewhat different when using Parsec or Megaparsec (the differences are related to backtracking and input consumption). 
Interestingly,  the `a <|> b` phenomenology we are discussing is very similar to Parsec's `try a <|> b`.  
This post does a great job explaining the complexity: 
[_add_blank_target Parsec: “try a <|> b” considered harmful](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/). 
Fixing `try a <|> b` anti-pattern can be not trivial. 
_side_note_end

_side_note_start
Side note: The scenario I described in this example is what I call:  _Unexpected input test_. 
Such tests can pin-point problems with error message response to a programming bug.  Some readers will argue that improving error messages caused by development time issues is an overkill.  However, this should be a case by case decision, e.g. such tests can be very relevant when programmers are the users (implementing a new programming language, a low-code infrastructure, etc) or when bugs are observed frequently.
_side_note_end

_side_note_start
Side note:  Another danger of the described approach: consider constituent (`Pet`, `Composer`) type JSON specs that do not tag type information and have partially overlapping data definitions (e.g. think about not overlapping fields being nullable).
_side_note_end




Are developers aware of this `<|>` issue?  Probably some are and some are not.
Code like this is probably written because JSON parser errors are unlikely to be viewed by the end user, _aeson_ makes code like this easy to implement, the code looks elegant, and error messages are the last thing on people's minds. 

Which leads to another question:   
Q: How would we guard against issues like this?  A common practice for avoiding program issues is writing tests.  How do I write a non-brittle test that checks the quality of _aeson_ error messages?  Do I write message parsers?   


Let’s forget about `<|>` for a moment and try to formalize what a parser error message is:  Consider the input document specification as a collection of sets of detailed specs _S<sub>T</sub>_, one for each parsed type _T_ (e.g. “Composer has a not-nullable ‘genre’  field of type Genere” is an element of _S<sub>Composer</sub>_) .  An error message pin-points an[^one] element in one of these sets marking it as failed (e.g. “Composer needs a genre”).  

To return a user-friendly error message, the parser needs to choose _S<sub>T</sub>_ wisely by matching the data the user is working on.  Parser needs to have access to enough information about this context to compute which _S<sub>T</sub>_ to use (data constructor tags is an example of how such context is provided to the parser). 
Thus, 

> &emsp; _thinking about user friendly error messages needs to be a part of software design and input specification_.  

[^one]: this assumes, for simplicity, that we are listing only one (e.g. first encountered) violation of the spec.

   
The point I have been trying to make is that using `Alternative` / `MonadPlus` in computations where error information is important (like parsing) can be very tricky. It requires thinking about and testing error outputs, not something developers typically do. 

>  &emsp;  _Hmm, I think Snuffy's genre would be hard rock. But what if the dog's name is Beethoven?_

## Overloaded errors 

This section will be more subtle. _Programs sometimes need to be selective about which error condition is handled_.

We will try to write a program that checks if the local config file _".my.yaml"_ exists and if not, uses _"~/.my.yaml"_, and returns an error if there is an issue with any of the files.

_side_note_start
Side note: Making issues "loud" helps in troubleshooting. 
Returning an error if there is a problem with the local file instead of alternating to a backup file is one way to be loud about configuration problems.
_side_note_end

We will use `MonadPlus` instance of `IO`. 
Here is standard library [_add_blank_target implementation](https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.IO.html#mplusIO) of `mplus` or `<|>` for the `IO` Monad:

```Haskell
mplusIO :: IO a -> IO a -> IO a
mplusIO m n = m `catchException` \ (_ :: IOError) -> n
```

_side_note_start
Side note: this code silences the first error which could be not ideal if you care about what went wrong. It becomes the responsibility of the caller do deal with this (e.g. at least log the error in the computation passed to `mplus`).   
Also, this `MonadPlus` instance is unlawful:  `launchMissiles >> mzero` is not `mzero`. 
_side_note_end

Let's take a journey trying to do implement this and see some nuances and how complex using `IO` with `<|>` can be:

```Haskell
import qualified Data.Yaml as Y -- yaml package dep
import Control.Applicative ((<|>))
import System.FilePath ((</>))  -- filepath package dep
import Data.ByteString as BS    -- bytestring package dep
import Control.Exception ( throwIO )

-- MyConfig and its instances not shown, home directory is passed as argument for simplicity

-- will not alternate to home directory file no matter what the issue with the local file is
-- because Y.decodeFileThrow is not throwing IOError, it throws Y.ParseException  
won'tWork :: FilePath -> IO MyConfig
won'tWork homedir = 
    Y.decodeFileThrow ".my.yaml" 
    <|> Y.decodeFileThrow (homedir </> ".my.yaml")

-- Y.decodeFileEither :: FromJSON a => FilePath -> IO (Either ParseException a) 
-- uses ParseException to also signal readFile issues like missing file
-- this puts all problems in one bucket and alternates to home directory on any issue with the local file
conflateAllIssues :: FilePath -> IO MyConfig
conflateAllIssues homedir = decode ".my.yaml"
    <|> decode (homedir </> ".my.yaml")
  where 
    decode :: FilePath -> IO MyConfig
    decode file = Y.decodeFileEither file >>= either (ioError . parseErrToIOError) pure  
    parseErrToIOError :: Y.ParseException -> IOError
    parseErrToIOError = userError . show -- for illustration only


-- still not ideal, it conflates any IOError issued from BS.readFile and alternates on any of them
-- however invalid syntax in local file will now cause an error
isolateIOErrors :: FilePath -> IO MyConfig
isolateIOErrors homedir = 
    decodeFileIsolateIOErrors ".my.yaml" 
    <|> decodeFileIsolateIOErrors (homedir </> ".my.yaml")

-- override what yaml package provides
decodeFileIsolateIOErrors :: FilePath -> IO MyConfig
decodeFileIsolateIOErrors file = do 
    bytes <- BS.readFile file      -- possible IOError
    either throwIO pure $ Y.decodeEither' bytes -- not IOError
```

If you dislike this code, then I am with you.
This example's goal is to illustrate a thought process that goes into handling errors, so let's focus on that process only. 

`conflateAllIssues` example conflates (and silences) all of these things:

* local file is missing
* invalid yaml syntax in the local file
* local file yaml has valid syntactically but does not represent `MyConfig`
* other IO issues related to the local file, e.g. file access problems, file corruption ...
 
The requirement is to alternate to the home directory file only when the local file is missing and output an error message otherwise. `isolateIOErrors` moves in this direction, but is still not right (it will alternate if there is anything wrong with `readFile`). 
Obviously there are ways to move forward, e.g. explore `Y.ParseException` constructors (there is more than one!) and make decisions whether to convert to `IOError` to alternate or not, or explore the content of the `IOError` returned from `readFile` and flip some of it outside of `IOError`.   

I hope this shows that things can get complex.  

My hidden goal behind this exercise was to have us notice something that applies to a wider range of `MonadPlus` / `Alternative` instances. 
In particular, it is related to the previous example. 
The impression I probably left on you in the previous section was: a naive use of _Alternative_ results in bad error messages. 

I look at the "Who cares about K9 composers" as a deeper issue of 2 conflated errors. 
The code in the previous section conflates errors from parsing JSON data representing one of the possible constituent types (parsing wrong branch), with errors from parsing JSON data that does not represent any of the constituent types (parsing unexpected data).  This code cannot distinguish between these errors and alternates on both. Ideally we would only alternate on the first but there is no obvious way to do that (_aeson_ errors are `String`s). 

Overloaded errors is a concern when programming parsers using `MonadPlus` instances. 
This is subtle and, probably, I have not explained it clearly enough.  Please give it some thought before dismissing it.


## Topics to discuss

In this post I wrote about things that irk me at the present moment.  I think that the overall situation with error messages is getting better and better, but IMO we are far from being where we should be. Haskell does not have expressive stack traces or convenient debuggers. 
One would assume the community will try to compensate with clear error messages and great log outputs to make up for these limitations.  I believe this topic needs more attention.

Here is a broader list of engineering topics that are IMO worth discussing:

* Overuse of `String` / `Text` as the error type.  
* Programming approach where `Either` Monad / `MonadError`-like computations augment error outputs with additional context at every opportunity. Strategies for compounding error information. 
* More about code that incorrectly uses wide ranging instead of specific errors and how abstractions fit into this.
* I dislike the non-termination `throw` `catch` games.  Throwing errors effectively bypasses the type checker. If you think of types as propositions and programs as proofs, you can prove any nonsense by throwing an error.  IMO, explicit `Either` type (or its close friends `ExceptT`/`MonadError`) are a better way to write code.  To me, throwing errors is not FP (think about Idris or even Rust for alternative ideas).  IMO, the same goes for effect systems: I prefer no `throw` `catch` games.  I would like to see the use of `error :: String -> a`, or even things like `IOError` eradicated from the ecosystem, (e.g. 
`readFile :: FilePath -> IO (Either IOFileErr ByteString)`).  (I unloaded a lot from my chest here 🙂)
* Type level consideration for errors.
* Strategies for dealing with non termination caused by use of `error :: String -> a` (a pure function, I call it &#128521; "pure evil").
* More about `Maybe`, `MonadPlus`, `Alternative` when they are, in addition to being very convenient, completely OK to use. 
* More about `MonadPlus`, `Alternative` when their use is concerning (e.g. are you using `guard :: Alternative f => Bool -> f ()` in parsers?  If so, how?).
* Strategies for refactoring code overusing `Alternative` in parsers. Writing parsers without using `<|>`.
* Monadic vs Applicative parsers comparison from the error messages standpoint.


It would be nice to know if I am alone in my views and if these topics are of interest for anyone out there. 
If not, I will probably make this my last blog post, blogging is costing a little bit too much energy.  If yes, I will select one of these topics and try to write more over the summer. 

Was this post negative?  IMO, there is a difference between negativity and frustration. 
Frustration can result in something positive, negativity cannot. 
Frustration seeks understanding, negativity does not. 
Frustration can unite, negativity can only divide.  

If you agree with some of the things I wrote here, please try to focus on these and let me know!
Thank you for reading and for your feedback. 

I am a concerned Haskeller who loves and adores this language.

To all my readers: thank you for reading my posts and for your constructive comments and for your encouragement.

