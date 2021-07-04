---
title: Arming polysemy with Arrows
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary: How to add semantic arrows (monadic effects in arrow's clothing) to polysemy
toc: true
changelog: <ul> 
    <li> (2021.07.02) Added Semantic Note in 
     <a href="#creating-arrow-effects">Creating Arrow Effects</a> section </li>
     <li> (2021.07.02-04) Added <a href="#tweag-workflows">Tweag Workflows</a> section </li>
     </ul>
tags: Haskell
---

Code for this project can be found in my [_add_blank_target experiments](https://github.com/rpeszek/experiments) github repo ([_add_blank_target polysemy-arrows](https://github.com/rpeszek/experiments/tree/master/polysemy-arrows) folder).   
It is based on [_add_blank_target _polysemy 1.3.0.0_](https://hackage.haskell.org/package/polysemy-1.3.0.0).

The goal of this post is to show how to use arrows when working with or creating _polysemy_ effects.

**Motivation:**  At work I use a proprietary effect system which is based on [_add_blank_target arrows](https://www.haskell.org/arrows).  I like it quite a bit, but... 
For something like an effect system, proprietary is not ideal.  The effect system has a huge syntactic and semantic impact on the code.  IMO it is a worthy goal to try limit the fragmentation of the ecosystem and (in the industrial context) focus on a small set of effect libraries.  
This task became my weekend pet project, and I have done some proof of concept work that added arrows to [_add_blank_target _polysemy_](https://hackage.haskell.org/package/polysemy). This post summarizes my effort.
I believe similar approach can be used with other monadic effect libraries (like [_add_blank_target _fused-effects_](https://hackage.haskell.org/package/fused-effects)). _polysemy_ seems much closer to the design of the library I use at work, and was my first choice for this proof of concept work.

**Nutshell:**  There are two reasons for using arrows: syntactic and semantic.  This blog, I am afraid, is about the first one.  It is mostly about syntax sugar. 
Polysemy's [_add_blank_target `Sem r`](https://hackage.haskell.org/package/polysemy-1.3.0.0/docs/Polysemy.html#t:Sem) Monad instance is unconstrained, and that translates to arrow effects getting the equivalent 
[_add_blank_target `ArrowApply`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad) typeclass instance for free.  
The code I am about to present is trivial (at least in the mathematical sense). Monads and arrows loaded with the power of `ArrowApply` are equivalent.  Semantic arrows I am going to implement here are just monads in arrow's clothing.  There is still some coding that needs to 
happen to implement the transformation and this code is the main subject of this post. 

Algebraic effects for arrows _that are less expressive_ (i.e. are not `ArrowApply` or even `ArrowChoice`) are meaningfully different and will be briefly discussed in the next section.  

The code I write at work uses DSLs with "fully loaded" `ArrowApply` semantic arrows.  We have build a lot of functionality that is directly concerned with inputs and outputs and arrows are a perfect syntactic match for us.  DSL expressiveness is also what we want.

My coding goal in this post is to:

*   be able to consume monadic polysemy effects using arrows
*   write arrow-like effects and consume them in monadic code

Another words, a programmer should be able to create monadic effects using arrow effects and vice-versa.  
This is largely accomplished by using the [_add_blank_target Control.Arrow.Kleisli](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:Kleisli) type
and an effect construction that mimics [_add_blank_target Control.Arrow.ArrowMonad](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad).

Many of my coworkers have, at some point, done a presentation or a tutorial about arrows. ([_add_blank_target Jake's talk](https://www.youtube.com/watch?v=msQiLyExM3w), [_add_blank_target Jason's talk](https://www.youtube.com/watch?v=YqVTCZFPoyQ)).   
I guess this post qualifies as one and, thus, I am joining the club.  

This post assumes the reader has at least basic familiarity with the concepts of arrows and algebraic effects. 


## Arrow Effects. Semantics

Since this will mostly be a syntax sugar, I need to talk about the semantics ... just to add some depth.  
In this section I want to explore the realm of theoretical possibilities for what arrow algebraic effects could possibly look like and how  they could differ from monadic algebraic effects.  

It is interesting to think about arrows from the point of view of code expressiveness.  Syntax aside, you can view arrow code as a subset
of monadic code.  Monadic code is the most expressive, arrows are much more restrictive ([_add_blank_target Idioms are oblivious, arrows are meticulous, monads are promiscuous](https://www.cl.cam.ac.uk/~jdy22/papers/idioms-are-oblivious-arrows-are-meticulous-monads-are-promiscuous.pdf) by Lindley, Wadler, Yallop).  In Haskell, additional [_add_blank_target `ArrowChoice`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowChoice) instance is needed to be able to write conditional (`if` and `case`) statements, 
[_add_blank_target `ArrowApply`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad) is needed to partially apply things.  To focus this fantasy exploration, I want to forget about `ArrowChoice` and `ArrowApply` for a moment and consider what arrow effect system would look like for DSLs based on the [_add_blank_target `Arrow`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:Arrow) typeclass only.

If an arrow code is a subset of a monadic code then monadic handlers (interpreters) have to also work on arrow DSLs.  (With some adjustments of course, this is exactly what this post implements.)  The question is what kind of other interesting interpreters one could come up with, that wouldn't work on promiscuous monads but would work on meticulous arrows?

For one, arrow effects should be able to interpret to other interesting arrows.  Basically, the handler (defined as the final interpreter / compilation target) should be able live outside of the standard (monadic) function space.  There are examples of open source code out there that make steps in this direction.
One example is tweag's [_add_blank_target funflow](https://hackage.haskell.org/package/funflow-1.3.2/docs/Control-Arrow-Free.html).

That sounds interesting but it is good to see a more practical benefit.  To do that let's think about what it means to have lack of conditional statements.

A great reading (somewhat on a theoretical side) is the Lindley's paper [_add_blank_target Algebraic Effects and Effect Handlers for Idioms and Arrows](https://homepages.inf.ed.ac.uk/slindley/papers/aeia.pdf).

Lindley uses the terms _static control flow_ and _dynamic control flow_.  Arrows allow for _static control flow_ only.

Consider this monadic code:

``` Haskell
countTo10 = do
  i <- get
  if i < 10
  then put (i + 1)
  else pure ()
```
that uses some _State_ effect and increments it until it reaches 10.  
This code uses dynamic control flow (`if`-s on the result of the previous computation) and cannot be implemented with plain arrows.  

This leads to the following 2 interesting observations:

* Arrow effects need to be more precise
* Arrow effect handlers could conceivably do crazy stuff

**Arrow Effects need to be more precise:**  
Monadic effect algebra GADT for _State_ is likely to look like this (actually copied it from polysemy):

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
interpreting `Modify` than just `Get`/`Put` (for example, do something that is more performant or use different log messages).


**Arrow effect handlers could conceivably do crazy stuff:**  
Static control flow makes for a much more predictable syntax tree.  In an example mentioned in the above paper, the 
interpreter statically infers when the `put` operation is used and when is not!  
Again, this translates to more power given to the interpreters, no `put`-s could, for example, mean a more aggressive optimization that somehow caches the state...  


### Tweag Workflows

_u/Ywen_ pointed out to me [_add_blank_target here](https://www.reddit.com/r/haskell/comments/o9y7re/arming_polysemy_with_arrows/h3oru2a?utm_source=share&utm_medium=web2x&context=3) that I missed a very cool presentation in the last ICFP, and indeed I did!  
References: [_add_blank_target Composing Effects into Tasks and Workflows](https://richarde.dev/papers/2020/workflows/workflows.pdf) by Parès, Bernardy, and Eisenberg; [_add_blank_target _kernmantle_](https://github.com/tweag/kernmantle) effects library;
here it is on [_add_blank_target youtube](https://www.youtube.com/watch?v=AiHOBF3BiLY&t=834s).

The paper is about creating data science workflow pipelines that decouple two runtime phases: config-time and process-time.
This allows for fail-early benefits (if config-time effects fail).

This approach uses an arrow (without _ArrowApply_) DSL that contains both applicative (config-phase) effects and monadic (process-time) effects. 
Applicative effect algebra GADTs separately defines parameters that the DSL _has to provide statically_ and parameters that can be used  in dynamic arrow invocation. Thus, the interpreters have more power to infer information about the statically used configuration.  This allows config-time interpreters to check, for example, if some statically specified model training data file exists and fail early if it does not, interpreters can pre-download needed artifacts, etc.

The presentation (see the youtube link) also shows a general arrow type that generalizes `Kleisli`, [_add_blank_target `Cokleisli`](https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html#t:Cokleisli) (from _comondad_), and
[_add_blank_target `Cayley`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Cayley.html) 
(from _profunctors_).

I am in the process of absorbing this work. I may update this comment when I understand _kernmantle_ better.

Separation between static and dynamic data is impossible when using monadic / ArrowApply computations.  I will discuss this a little bit more in the [Creating Arrow Effects](#creating-arrow-effects) section of this post.

### Chris Penner's ~~arr~~ow idea

A very interesting [_add_blank_target Berlin's FP Group](https://www.youtube.com/channel/UCNp-DVb8cQRIOo32sZhWgNg) presentation by Chris Penner [_add_blank_target Deconstructing Lambdas—An Awkward Guide to Programming Without Functions](https://www.youtube.com/watch?v=xZmPuz9m2t0) envisions a world with something `Arrow`-like that does not have the [_add_blank_target `arr`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#v:arr).   
From a DSLs prospective, `Arrow`'s `arr` translates to the ability to use any Haskell `a -> b` function inside the DSL.  
Removing this ability is very interesting. It is also a hard stop when trying to reuse any monadic effects library.  

It seems that Chris Penner's vision could be implemented with some Arrow-Minus like construction that cares about inputs and outputs
and algebraic effect system that will be completely divorced from monadic effects.

**The point** I tried to make in this section is that Arrow-like effect systems that are not convertible to monadic effects are very interesting and semantically important.  The rest of this post is about interpreting arrows using polysemy and is more a syntax sugar thing.

Now back to _polysemy_ and monads:


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

-- * interpreters

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

interpreter :: r ~ '[Teletype, Embed IO] => Sem r a -> IO a
interpreter = runM . teletypeToIO

test :: IO ()
test = interpreter echo
```

I have spelled out the effect stack in the type signature of the `interpreter` combinator
to add the clarity.  I will do that in following examples as well.  Compiler can infer these and typical _polysemy_ examples do not 
list the complete effect list.  
The goal is to replace monadic computations with arrows.

## `SemArr` type

Let me introduce our semantic arrow type:

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

It is probably obvious, but it is good to remember that `Kleisli` is just a type and it does not imply monad payload by itself. 
In particular, it has these instances:

``` Haskell
instance Functor m => Functor (Kleisli m a)
instance Applicative m => Applicative (Kleisli m a)
```

`SemArr r a b` comes fully loaded with all `Arrow` typeclass instances (like `ArrowApply`) because `(Sem r)` has the unconstrained `Monad` instance:

``` Haskell
instance Monad (Sem f)
``` 


`SemArr r a b` is a monad in arrow's clothing. 


## Consuming _polysemy_ effects using arrows

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

`readTTYA` and `writeTTYA` are arrow-ised versions of the monadic `readTTY` and `writeTTY` primitives.

Notice the change in the type signature for `interpreter`.  `interpreter` now threads the input data into the computation and spells out the output at the end.
You can also view it as compiling the `SemArr r a b` arrow down to `Kleisli IO a b` (only with `Kleisli` unwrapped for extra convenience).

Let's move onto some more interesting stuff:

## Arrow Effects. The Plumbing

One of the syntactic advantages of using arrow effects, at least for me, is the simplified construction of the effect algebra GADT.  This definition nicely spells out the input and the output:

``` Haskell
data Teletype2 a b where
  ReadTTY2  :: Teletype2 () String
  WriteTTY2 :: Teletype2 String ()
```

I want to be able to add `Teletype2` to the list of polysemy effects.  This needs some plumbing:

``` Haskell
data Eff2 arr r b where
  MkEff2 :: arr () b -> Eff2 arr r b

interpretEff2 :: (forall x . m x -> Sem r x) -> (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
interpretEff2 comp fn = interpret \case
  MkEff2 arr   -> comp (Arr.runKleisli (fn arr) ())
```

`MkEff2` is the bit which mimics [_add_blank_target Control.Arrow.ArrowMonad](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html#t:ArrowMonad)!  This trick is based on the fact `B = B ^ 1`: one can represent elements in a set `B` by using fuctions from a one element set (here `()`) ending in `B` that pick that element.  That observation nicely generalizes to categorical constructions that use terminal objects and this principled idea works great for what I need.

`interpretEff2` is actually quite powerful.  It allows me to implement an equivalent to polysemy's `embed`:

``` Haskell
embedEff2 :: Member (Embed m) r => (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
embedEff2 = interpretEff2 embed
```

it will also allow me to reinterpret an arrow effect using other (possibly arrow) effects. We will see it again in the [Nested Effects](#effects-based-on-arrow-effects) section.

However, there is a clear problem with what we have so far: `arr () b` in the definition of `Eff2` does not match with the `WriteTTY2` constructor. 
We need to embed `WriteTTY2` in a bigger instruction set that has enough expressiveness to be able use the above `() ->` trick.  To do that I will do the following, minimalist, free construction:

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

`liftCompKl` allows me to expand `Kleisli` encoding of `Teletype2` to the mini-DSL: `Eff2Free Teletype2`.  

**Semantic Note:** When I started working on this, I assumed my `Eff2Free` will be a free construction of the `Arrow`, and possibly `ArrowChoice` or even `ArrowApply`. 
This ended up being much more minimalist, we get arrows for free from using `SemArr r a b`. 

What is really needed, is the ability to pre-compose mapped (i.e. `Pure`) functions with the lifted effect. This free construction needs
only to be compatible with the `Contravariant` Functor (or `Profunctor` `lmap`) and there should be some flexibility of how it is done.

## Creating Arrow Effects

Here is the whole program that uses an Arrow-like effect algebra, arrow instruction primitives, and a DSL program that uses arrows:

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
readTTY2 = send (MkEff2 (Effect ReadTTY2)) 

writeTTY2A :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => SemArr r String ()
writeTTY2A = semArr writeTTY2

-- | mini-DLS @Eff2Free Teletype2@ allows me to encode input paramter to the @WriteTTY2@ instruction
writeTTY2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Teletype2)) r => String -> Sem r ()
writeTTY2 s = send (MkEff2 (Pure (const s) >>> Effect WriteTTY2)) 

echo2A :: Member (Eff2 (Eff2Free Teletype2)) r => SemArr r () ()
echo2A = proc _ -> do
    i <- readTTY2A -< ()
    case i of 
       "" -> 
           writeTTY2A -< "Need some input"
       _ ->  
           writeTTY2A -< "You said " <> i

-- * interpreters

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

* `readTTY2` and `writeTTY2` primitives can be used by any monadic polysemy code to create new effects or to use in programs where effect the `Eff2 (Eff2Free Teletype2)` is available.
* `readTTY2A` and `writeTTY2A` are available for any arrow code. In particular, these can be used to create
new arrow effects that compile down to `Eff2 (Eff2Free Teletype2)`. 

**Semantic Note:**  Notice that, when defining monadic version of `Teletype` GADT, we used value level `String` in the `WriteTTY`
constructor, now the `String` input is defined squarely on the type level.  
We could consider defining `WriteTTY2` as `WriteTTY2 :: String -> Teletype2 () ()` instead.   
This would yield 
`String -> SemArr r () ()` type for the above `writeTTY2A`.  In the expressive world of _monads_ / _ArrowApply_ both constructions are largely equivalent. 
Not so in the more restrictive universe of general arrows we discussed in the [Semantics](#arrow-effects.-semantics) section.  
The new construction would allow a more static handling of the `String` parameter by the interpreter but the `String` parameter would not be available for 
dynamic use at the DSL level. We would not be able to write the above `echo2A` program.
A code example of this is provided in the included repo [_add_blank_target here](https://github.com/rpeszek/experiments/blob/05e23cdca5766b947b731d9084bdfe96c7bcbaae/polysemy-arrows/src/Teletype2B.hs)

Implementing arrow effects on top of other arrows effects seems to be the most interesting case of nesting and is shown next:


## Creating Nested Effects

`DoEcho2` instruction has a non-unit input and output making it more interesting. 
We will interpret it down to `Teletype2` effect by writing its input, reading a new input and returning it as the result.

``` Haskell
data Echoer2 a b where
  DoEcho2 :: Echoer2 String String

doEcho2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Echoer2)) r => String -> Sem r String
doEcho2 s = send (MkEff2 (Pure (const s) >>> Effect DoEcho2)) 

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

interpreter ::  r ~ '[Eff2 (Eff2Free Echoer2), Eff2 (Eff2Free Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreter arr a = runM . embedEff2 (liftCompKl tele2ToKlIO) . echoer2ToTeletype2 $ Arr.runKleisli arr a

testEchoerA :: String -> IO String
testEchoerA  = interpreter (doEcho2A >>> doEcho2A) 
```

`echoer2ToKl` plays the role of an interpreter for the effect algebra. It is implemented using arrows!  
The `interpretEff2` combinator (defined in the [plumbing](#arrow-effects-the-plumbing) section) returns an interpreter that _polysemy_ understands.  
Again, I view `interpreter` as a natural transformation between `SemArr r a b` arrow and `Kelisly IO a b` arrow (ignoring the `runKleisli` unwrapping).  

We have a two way street to writing compilation stacks: 

* compose `Sem r` interpreters that remove effects from the `r` record and compile `Sem r1 a` monadic computation to a simpler monadic computation  `Sem r2 a` (the standard _polysemy_'s approach)
* compose `SemArr r` interpreters that compile `SemArr r1 a b` arrow to a simpler `SemArr r2 a b` arrow.

or mixing both ways.  These options are all possible (my git repo has the necessary combinators).  


## Final Thoughts

We have accomplished the goal.  We can program _polysemy_ effects by using interchangeably both the monadic and the arrow syntax.

This seems to be an interesting engineering problem: effective refactoring methods for swapping effect systems.  
The task of refactoring a large code base to change its effect library will not be easy and I am not even sure it will be attempted.  
The differences are not just in the use of arrows vs monads.
The effects are declared polymorphically in both cases but in very different ways.  The collections of the base (batteries included) effects has subtle differences.  There are subtle but important differences in error handling ...  
It will require much more footwork to be able to do it piece by piece.   

The `Eff2` / `Eff2Free` tooling is not really arrow specific. I believe, it could be used with other things. That is the reason behind the names (for not using something like `EffArr`).  However, whatever you come up with (e.g. a free Profunctor construction) this will be just dressing a monad in some other clothing - it will be syntactic only. The `Arrow` seems to be the only meaningful exception, because it comes with the syntax extension (the Arrows pragma).

This was all very much a quick prototype. I am sure the presented code can be improved in many ways. I hope it was good enough to allow for an interesting reading.

