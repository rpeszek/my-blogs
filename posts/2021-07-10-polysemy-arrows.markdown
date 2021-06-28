---
title: Polysemy with Semantic Arrow Effects
author: Robert Peszek
lastmodified: Jun 27, 2021
featured: true
summary: How to add semantic arrows (monadic effects in arrow's clothing) to polysemy
toc: true
tags: Haskell
---

Code for this project can be found in my [_add_blank_target experiments](https://github.com/rpeszek/experiments) repo ([_add_blank_target polysemy-arrows](https://github.com/rpeszek/experiments/tree/master/polysemy-arrows) folder).   
It is based on [_add_blank_target _polysemy 1.3.0.0_](https://hackage.haskell.org/package/polysemy-1.3.0.0) 

**Motivation:**  At work I use a proprietary effect system which is based on [_add_blank_target arrows](https://www.haskell.org/arrows).  I like it quite a lot, but... 
For something like an effect system, proprietary is not ideal.  Effect system has a huge syntactic and semantic impact on the code.  IMO it is a worthy goal to try limit the fragmentation of the ecosystem and focus (in the industrial context) on using only few most established effect libraries.  
This task became my pet project in recent days, and I have done some proof of concept work to add arrows to [_add_blank_target _polysemy_](https://hackage.haskell.org/package/polysemy), this post summarizes my effort.
I believe similar approach can be used with other monadic effect libraries (like [_add_blank_target _fused-effects_](https://hackage.haskell.org/package/fused-effects)) but I have not done the exercise. _polysemy_ seems much closer to the design of the library we use and was my first choice.

**Nutshell:**  There are two reasons for using arrows: syntactic and semantic.  This blog, I am afraid, is about the first one.  It is mostly about syntax sugar. 
Polysemy's `Sem r` enjoys unconstrained `Monad` instance, and that translates to arrow effects getting the equivalent `ArrowApply` for free.  
The code I am about to present is trivial (at least in the mathematical sense). Monads and arrows loaded with power of `ArrowApply` are equivalent.  Semantic arrows I am about to present are just monads in arrow's clothing.  There is still some coding that needs to 
happen to implement the transformation and this code is the main subject of this post. 

_Algebraic effects for Arrows_ that are less expressive (e.g. are not `ArrowApply` or even `ArrowChoice`) are meaningfully different and will be briefly discussed in the next section.  

The code I write at work uses semantic fully loaded `ArrowApply` arrows.  We have build a lot of functionality that is directly concerned with inputs and outputs and arrows are a perfect syntactic match.  DSL expressiveness is also what we want.

My coding goal in this post is to:

*   be able to consume monadic polysemy effects using arrows
*   write arrow-like effects and consume them in monadic code

Another words, a programmer should be able to create monadic effects using arrow effects and vice-versa.  
This is largely accomplished by using the [_add_blank_target Kleisli](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:Kleisli) type
and an effect construction that mimics [_add_blank_target ArrowMonad](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad).

Most of my coworkers have, at some point, done a presentation or a tutorial about arrows. ([_add_blank_target Jake's talk](https://www.youtube.com/watch?v=msQiLyExM3w), [_add_blank_target Jason's talk](https://www.youtube.com/watch?v=YqVTCZFPoyQ)).   
I guess this post qualifies as one and, thus, I am joining the club.  

This post assumes the reader has at least basic familiarity with the concepts of arrows and algebraic effects. 


## Intuitions about Arrow Effects

Since this will mostly be about syntax sugar, I need to talk about the semantics a little bit, ... just to feel less dirty.  
In this section I want to explore the realm of theoretical possibilities for what arrow algebraic effects could possibly look like and how could they differ from monadic algebraic effects.  

It is interesting to think about arrows from the point of view of code expressiveness.  Syntax aside, you can view arrow code as a subset
of monadic code.  Monadic code is the most expressive, arrows are much more restrictive ([_add_blank_target Idioms are oblivious, arrows are meticulous, monads are promiscuous](https://www.cl.cam.ac.uk/~jdy22/papers/idioms-are-oblivious-arrows-are-meticulous-monads-are-promiscuous.pdf) by Lindley, Wadler, Yallop).  In Haskell, additional `ArrowChoice` instance is needed to be able to write conditional statements,  `ArrowApply` is needed to partially apply things.  To focus this fantasy exploration, we forget about `ArrowChoice` and `ArrowApply` for a moment and consider what arrow effect system would look like for DSLs based on the `Arrow` typeclass only.

If arrow code as a subset of monadic code then monadic handlers (interpreters) have to also work on arrow DSLs.  (With some adjustments of course, this is exactly what this post implements.)  The question is what other kind of interesting interpreters we could do that would not work on promiscuous monads but will work on meticulous arrows?

For one thing, arrows should be able to interpret to other arrows.  Basically, the handlers should be able live outside of the standard function space.  There are examples of open source code out there that actually does that.
One example is tweag's [_add_blank_target funflow](https://hackage.haskell.org/package/funflow-1.3.2/docs/Control-Arrow-Free.html).

That sounds interesting but it is good to see a more practical benefit.  To do that lets think about what it means to have lack of conditional statements.

A great reading (somewhat on the theoretical side) is the Lindley's paper [_add_blank_target Algebraic Effects and Effect Handlers for Idioms and Arrows](https://homepages.inf.ed.ac.uk/slindley/papers/aeia.pdf).

Lindley uses the terms _static control flow_ and _dynamic control flow_.  Arrows allow for _static control flow_ only.

Consider this monadic code:

``` Haskell
countTo10 = do
  i <- get
  if i < 10
  then put (i + 1)
  else pure ()
```
that uses some State effect and increments it until it reaches 10.  
This code uses dynamic control flow (`if` condition is on the result of the previous computation) and cannot be implemented with plain arrows.  

This leads to the following 2 interesting observations:

* Arrow effects need to be more precise
* Arrow effect handlers could conceivably do crazy stuff

**Arrow Effects need to be more precise:**  Monadic State effect algebra GADT is likely to look like this (actually copied it from polysemy):

``` Haskell
data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()
```

to implement `countTo10` without conditional statements I need more precise instructions! For example, I could add:

``` Haskell
  Modify :: (s -> s) -> State s m ()
```

This forced precision could be perceived as a benefit in some situations.  It gives more power to the interpreter to do different things when
interpreting `Modify` than just `Get`/`Put` (for example, do something that is more performant or use different logging).


**Arrow effect handlers could conceivably do crazy stuff:**  Static control flow makes for a much more predictable syntax tree.  
For example, you can imagine interpreter that determines statically if `put` operation is used or not!  
Again, this translates to more power given to interpreters, no `put`-s could, for example, mean more aggressive optimization that somehow caches the state.  


### Imagining ArrowMinus

A very interesting [_add_blank_target Berlin's FP Group](https://www.youtube.com/channel/UCNp-DVb8cQRIOo32sZhWgNg) presentation by Chris Penner [_add_blank_target Deconstructing Lambdas—An Awkward Guide to Programming Without Functions](https://www.youtube.com/watch?v=xZmPuz9m2t0) envisions a world where `Arrow` does not have [_add_blank_target `arr`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#v:arr).   
From a DSLs prospective, `Arrow`'s `arr` translates to ability to use any standard `a -> b` function inside the DSL.  
Removing this ability is very interesting, but is also a hard stop when trying to reuse any monadic effects library.  

It seems that Chris Penner's vision can exist with some Arrow-Minus like constructions that care about inputs and outputs
and algebraic effect system that will be completely divorced from monadic effects.

The point this section tried to make is that Arrow-like effect systems that are not convertible to monadic effects are very interesting is semantically important.  The rest of this post is about interpreting arrows using polysemy and is more a syntax sugar thing.


## Monadic polysemy, Working example

I will use the classic `Teletype` effect as my driving example.  This is taken (almost) straight from the polysemy's github readme:

``` Haskell
data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> writeTTY "Need some input"
    _  -> writeTTY $ "You said " <> i

-- * interpreter

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

interpreter :: r ~ '[Teletype, Embed IO] => Sem r a -> IO a
interpreter = runM . teletypeToIO

test :: IO ()
test = interpreter echo
```

The goal is to replace monadic computations with arrows.

## SemArr type

``` Haskell
import qualified Control.Arrow as Arr

-- | Semantic arrows in polysemy
type SemArr r a b = Arr.Kleisli (Sem r) a b

-- | Used to define atomic effect arrows
constSemArr :: (Sem r) b -> SemArr r () b
constSemArr c = Arr.Kleisli (const c)

-- | Used to define atomic effect arrows
semArr :: (a -> (Sem r) b)-> SemArr r a b
semArr = Arr.Kleisli

-- | transforms @Sem@ compilation stacks to @SemArr@ compilation stacks.
semArrCompl :: (Sem r1 b1 -> Sem r2 b2) -> SemArr r1 a b1 -> SemArr r2 a b2 
semArrCompl comp (Arr.Kleisli fn) = Arr.Kleisli (comp . fn)
```

`Kleisli m a b` wraps `a -> m b` function and is the standard way to express monadic computations as arrows.

It is probably obvious, but it is good to remember that `Kleisli` is just a type and it does not imply Monad payload by itself. 
In particular it has these instances:

```
Functor m => Functor (Kleisli m a)
Applicative m => Applicative (Kleisli m a)
```
`SemArr r a b` comes fully loaded with all `Arrow` typeclass instances (like `ArrowApply`) because `(Sem r)` has unconstrained `Monad` instance.  
`SemArr r a b` is a monad in arrow's clothing. 


## Arrow consumption

This is section will not be interesting.  We can tranform monads to arrows using Kleisli, duh:
``` Haskell
readTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r () String
readTTYA = constSemArr readTTY

writeTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r String ()
writeTTYA = semArr writeTTY

echoA :: Member Teletype r => SemArr r () ()
echoA = proc _ -> do
    i <- readTTYA -< ()
    case i of 
       "" -> 
           writeTTYA -< "Need some input"
       _ ->  
           writeTTYA -< "You said " <> i

-- Notice interpreter is input - output based, the last bit @a -> IO b@ is effectively Kleisli arrow

interpreter ::  r ~ '[Teletype, Embed IO] => SemArr r a b -> a -> IO b
interpreter arr a = runM . teletypeToIO $ Arr.runKleisli arr a

testA :: IO ()
testA = interpreter echoA ()
```

Notice the change in type signature for `interpreter`.  Interpreter now threads the input data into the computation and spells out the output at the end.
You can also view it as compiling the `SemArr r a b` arrow down to `Kleisli IO a b` (only with `Kleisli` unwrapped for extra convenience).

Lets move onto some more interesting stuff:

## Arrow Effects, The Plumbing

One of the syntactic advantages of using arrow effects, at least for me, is the simplified construction of effect algebra GADTs.  This definition nicely spells out the input and the output:

``` Haskell
data Teletype2 a b where
  ReadTTY2  :: Teletype2 () String
  WriteTTY2 :: Teletype2 String ()
```

I want to be able to add `Teletype2` to the list of polysemy effects.  To do that I need to do some plumbing:

``` Haskell
data Eff2 arr r b where
  MkEff2 :: arr () b -> Eff2 arr r b

interpretEff2 :: (forall x . m x -> Sem r x) -> (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
interpretEff2 comp fn = interpret \case
  MkEff2 arr   -> comp (Arr.runKleisli (fn arr) ())
```

`MkEff2` is the bit which mimics [_add_blank_target ArrowMonad](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad)!  This is based on the fact `B = B ^ 1`: one can represent elements in a set B by using fuctions from a one element set (here `()`) ending in B that pick that element.  That observation nicely generalizes to categorical constructions that use terminal objects and this principled idea works great for what I need.

`interpretEff2` is actually quite powerful.  It allows me to implement an equivalent to polysemy's `embed`:

``` Haskell
embedEff2 :: Member (Embed m) r => (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
embedEff2 = interpretEff2 embed
```

it will also allow me to reinterpret an arrow effect using other (possibly arrow) effects, as we will see in the [Nested Effects](#effects-based-on-arrow-effects) section.

However, there is a clear problem with what we have so far: `arr () b` in the definition of `Eff2` does not match with the `WriteTTY2` constructor. 
We need to embed `WriteTTY2` in a bigger instruction set that has enough expressiveness to be able use the above `() ->` trick.  To do that I will do the following, minimalist, [_add_blank_target free construction](https://bartoszmilewski.com/2015/07/21/free-monoids/):

``` Haskell
data Eff2Free eff a b where
  Pure :: (a -> b) -> Eff2Free eff a b
  Effect :: eff a b -> Eff2Free eff a b
  Seq :: Eff2Free eff a b -> Eff2Free eff b c -> Eff2Free eff a c

instance Category (Eff2Free eff) where
  id = Pure Cat.id
  (.) = flip Seq

liftCompKl :: Monad m => (forall x y . eff x y ->  Arr.Kleisli m x y) -> Eff2Free eff a b -> Arr.Kleisli m a b
liftCompKl _ (Pure f) =  Arr.Kleisli (pure Cat.. f)
liftCompKl fn (Effect eff) = fn eff
liftCompKl fn (Seq a1 a2) = liftCompKl fn a1 >>> liftCompKl fn a2
```

Combining `Eff2` with `Eff2Free` is what I need to move forward.  

`liftCompKl` allows me to expand `Kleisli` encoding of `Teletype2` to the mini-DSL `Eff2Free Teletype2`.  

**Semantic Note:** When I started working on this, I assumed my `Eff2Free` will be a free construction of an `Arrow`, and possibly `ArrowChoice` or even `ArrowApply`. 
This ended up being much more minimalist, we get arrows for free from using `SemArr r a b`. 

What is reallly needed is ability to pre-compose mapped functions before the lifted effect. This free construction needs
to be compatible with the _Contravariant Functor_ and there should be flexibility of how it is done.

## Arrow Effects

Here is the whole program that uses Arrow-like effect algebra, arrow instruction primitives, and arrow DSL program:

``` Haskell
data Teletype2 a b where
  ReadTTY2  :: Teletype2 () String
  WriteTTY2 :: Teletype2 String ()

-- type Effect2 arr r b = Eff2 (Eff2Free arr) r b
-- | arrow frienly combinator
readTTY2A :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => SemArr r () String
readTTY2A =  constSemArr readTTY2

-- | monad frienly combinator, can be consumed by monadic programs and effects
readTTY2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => Sem r String
readTTY2 =  send (MkEff2 (Effect ReadTTY2) :: Eff2 (Eff2Free Teletype2) (Sem r) String) 

writeTTY2A :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => SemArr r String ()
writeTTY2A = semArr writeTTY2

-- | mini-DLS @Eff2Free Teletype2@ allows me to encode input paramter to the @WriteTTY2@ instruction
writeTTY2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => String -> Sem r ()
writeTTY2 s =  send (MkEff2 (Pure (const s) >>> Effect WriteTTY2) :: Eff2 (Eff2Free Teletype2) (Sem r) ()) 

echo2A :: Member (Eff2 (Eff2Free Teletype2)) r => SemArr r () ()
echo2A = proc _ -> do
    i <- readTTY2A -< ()
    case i of 
       "" -> 
           writeTTY2A -< "Need some input"
       _ ->  
           writeTTY2A -< "You said " <> i

-- * interpreter

-- | Teletype2 instructions are interpreted as Kleisli
tele2ToKlIO :: Teletype2 a b -> Arr.Kleisli IO a b
tele2ToKlIO ReadTTY2 = Arr.Kleisli $ const getLine
tele2ToKlIO WriteTTY2 = Arr.Kleisli putStrLn

-- | tele2ToKlIO is extended to the the mini-DLS @Eff2Free Teletype2@
interpreter ::  r ~ '[Eff2 (Eff2Free Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreter arr a = runM . embedEff2 (liftCompKl tele2ToKlIO)  $ Arr.runKleisli arr a

testA2 :: IO ()
testA2 = interpreter echo2A ()
```

* `readTTY2` and `writeTTY2` primitives can be used by monadic polysemy code to create new effects or to use in programs where effect `Eff2 (Eff2Free Teletype2)` is available.
* `readTTY2A` and `writeTTY2A` are available for by arrow code. In particular, these can be used to create
new arrow effects that compile down to `Eff2 (Eff2Free Teletype2)`. 

This is the most interesting case of nesting and is shown next:


## Nested Effects

`DoEcho2` has a non-unit input and output making it more interesting. 
We will interpret it down to `Teletype2` effect by writing the output, reading the new input and returning it as the result.

``` Haskell
data Echoer2 a b where
  DoEcho2 :: Echoer2 String String

doEcho2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Echoer2)) r => String -> Sem r String
doEcho2 s =  send (MkEff2 (Pure (const s) >>> Effect DoEcho2) :: Eff2 (Eff2Free Echoer2) (Sem r) String) 

doEcho2A :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Echoer2)) r => SemArr r String String
doEcho2A = semArr doEcho2 

-- | nice use of arrows to create complex effects
echoer2ToKl :: Member (Eff2 (Eff2Free Teletype2)) r => Echoer2 a b -> Arr.Kleisli (Sem r) a b
echoer2ToKl DoEcho2 = proc inp -> do
  _ <- writeTTY2A -< inp
  txt <- readTTY2A -< ()
  if txt == ""
  then arr id -< "No Input"
  else arr id -< "You said " <> txt

-- | interpretEff2 allows to intepret down the effect
echoer2ToTeletype2 :: forall r a . Member (Eff2 (Eff2Free Teletype2)) r => Sem (Eff2 (Eff2Free Echoer2) ': r) a -> Sem r a
echoer2ToTeletype2 = interpretEff2 id (liftCompKl @ (Sem r) echoer2ToKl)

interpreterEchoer ::  r ~ '[Eff2 (Eff2Free Echoer2), Eff2 (Eff2Free Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreterEchoer arr a = runM . embedEff2 (liftCompKl tele2ToKlIO) . echoer2ToTeletype2 $ Arr.runKleisli arr a

testEchoerA :: String -> IO String
testEchoerA  = interpreterEchoer (doEcho2A >>> doEcho2A) 
```

`echoer2ToKl` plays the role of interpreter for the effect algebra. It is implemented using arrows!  
The `interpretEff2` combinator (defined in the [plumbing](#arrow-effects-the-plumbing) section) returns the interpreter that _polysemy_ understands.  
I view `interpreterEchoer` as a natural transformation between `SemArr r a b` arrow and `Kelisly IO a b` arrow (ignoring the `runKleisli` unwrapping).  

This provides a two way street to writing compilation stacks: 

* compose `Sem r` compilers that remove effects from the `r` record and compile monadic computation `Sem r1 a` to a simpler monadic computation  `Sem r2 a`
* compose `SemArr r` compilers that compile `SemArr r1 a b` arrow to simpler `SemArr r1 a b` arrow.

or mixing both ways.  These options are all possible (my git repo has the necessary combinators).  


## Conclusions

The goal of refactoring a large code base to change effect library will not be easy and I am not even sure it will be attempted.  
The differences are not just in the use of arrows vs monads.
The effects are declared polymorphically in both cases but in very different ways.  The collection of base effects like `State` have subtle differences...  
It will require much more footwork to be able to do it piece by piece.   
This seems to be an interesting engineering problem: effective refactoring methods for swapping effect systems.

The `Eff2` / `Eff2Free` tooling is not really arrow specific.  I think, it could be used for other things.

This was all very much a quick prototype. I am sure the presented code can be improved in many ways. I hope it was good enough to make an interesting reading.

