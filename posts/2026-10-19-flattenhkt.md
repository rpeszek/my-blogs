---
title: Life after Haskell, HKT flattening (examples in Kotlin)
featured: true
summary: Using flattened HKTs and higher-order functions in mainstream programming
toc: true
tags: non-FP FP, Haskell
codestyle: newstyle
---

**DRAFT STATUS**

> *It's no use trying to eat a steak with a teaspoon, and a straw.*   
> &emsp; &emsp; - Anthony T. Hincks  

This quote describes my thoughts about using mainstream languages to write functional programs. But, maybe there are some weird spoons and straws that 
replace some of the functional pearls? 

I will complain about booby-traps in popular mainstream languages. 
If you are tired of such complaints, go directly to [Setter the HKT flattening case study](#setter-the-hkt-flattening-case-study) where I consider "flattening" &#128296; HKTs. I use flattening in an intuitive sense. `f a` becomes `fa` (a "big" type; `fa` is a "flattened" `f a` because it uses fewer type variables) and `a` sticks around (a "small" type). `(a -> b) -> f a -> f b` becomes a plain `(a -> b) -> fa -> fb`. 
This allows me to start using a less polymorphic but still useful `Setter` instead of a `Functor`. 
Similarly, the `Traversal` optic ends up replaced with a `Setter` that uses implicit IO.  
   
I advocate for avoiding type-based ad-hoc polymorphism (Context Oriented Programming in Kotlin) in favor of passing computations like `Setter s t a b` as first-class, direct arguments (a "dictionary passing"-like style).  This promotes programming with higher-order functions.
You may very much disagree with this approach as a good idiomatic direction. IMO, it is still worth examining.


You may also find [Using LLMs](#using-llms) interesting.

*Written for functional programmers familiar with Haskell, optics, and Rank-N who work with mainstream languages.
Examples are in Kotlin (I am new to Kotlin).*

## My frustration

I work on a large Haskell code base that has been sadly shrinking over the last few years.  The biggest piece still in use is a proprietary programming language that allows manipulating and combining web service endpoints in a type-safe way.  It also simplifies UI development.  The UI piece is our "low code" solution for fast development of mostly short-lived apps. LLMs and tooling based on LLMs are replacing the "low code". So a large part of the Haskell code base will no longer be needed. Our language uses "big" and "small" types terminology and that seems approachable. I will explore this idea here in the context of doing FP in a popular mainstream language. 

A bigger issue is finding and hiring functional programmers.  Our codebase does not do [_add_blank_target Simple Haskell](https://www.simplehaskell.org/) / [_add_blank_target Boring Haskell](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/). I am not sure if I even like these ideas. I get it. It is not easy. I have a math background and I did math research before I started software programming. Learning FP was fascinating but not easy for me. 
_It is as hard to hire developers who know FP as it is for a developer who knows FP to find an FP job._
At the same time (based on my past experience as an interviewer):
_It may be easier to find a developer who knows FP than a Java/TypeScript/... developer who understands Java/TypeScript/... booby-traps._
IMO, this is the result of needless complexity and nuance and a problem with industry priorities. Frankly, I am scared.

_side_note_start **Not my story:**  Uber's Go codebase was suffering from frequent crashes. Ignoring Turing's Halting Problem / Rice's Theorem as too theoretical, Uber hired a bunch of PhDs to create NilAway, a linter solution to null reference issues. It is written in Go. Apparently it is known to crash with a null reference panic from time to time &#128512;
_side_note_end

We briefly considered Go.  Go trades safety for performance and claims to be junior-programmer friendly, failing to see this as a contradiction.
We rejected Go because of its safety track record in concurrent programming. The industry's take on beginner friendliness is misguided. It can be compared to giving a sharp knife to a baby as the first toy. 

We probably will settle on Kotlin. It is touted as one of the safer mainstream options.  But even the `==` operator is still a booby-trap. 

```Kotlin
data class RegularId(val value: Int)
data class SpecialId(val value: Int)

list.find { it.id == foo.bar.lookupId }
```

`list` has items with `RegularId`s, the `lookupId` field in `bar`'s class is refactored from `RegularId` to `SpecialId`. This breaks this code at runtime, not at compile time.  Not to mention that `a == b` is not the same as `b == a` for some `a`s and `b`s in the standard Java library. 
We cannot stop using `==` and there is no workaround for it other than lots of diligence and testing. Any JVM PL that tries to have decent interop with Java faces such issues. Kotlin can flag some such errors but not all.
I have not used Java for a long time. I do not remember all the gotchas. I am playing with Kotlin to identify booby-traps and catalogue them. After 4 hours of doing this, I wrote down 12 gotchas. 3 more the next day...
In my experience, developers (junior or senior) are completely unaware of these issues. 

> I call it **The Booby-trap Principle**  
> _Nothing is obviously wrong except for the non-obvious._

Kotlin's surface area is very bloated.  `class` can be: `open`, `abstract`, `data`, `@JvmRecord data`, `sealed`, `value`, `enum`, `annotation`, `expect` / `actual`, `external`, `inner`..., or just `class`.  `interface` adds 3 more variants.  Clearly not very approachable. 

I did some math research, then 2 decades of mainstream programming with many battle scars, then an 8-year-long Haskell job (some math came back only different), now I am going back to mainstream.  Is some convergence possible?  Can I find some middle-ground approach to programming? 
 
## Higher-order functions 

Haskell-like FP in a non-FP language is hard to do. The biggest culprit is the lack of support for higher-kinded / second-order types.  There is a technique called _lightweight higher-kinded types_ or _defunctionalization of higher-kinded types_.  The libraries based on this technique are hard to use and probably harder to learn than Haskell itself.

There is an effort to create a more approachable FP ecosystem (e.g. arrow in Kotlin - new versions, Effect in TypeScript, returns in Python, Vavr in Java ...).  These libraries hide internal implementation of FP concepts behind a "frontend" that feels more idiomatic and familiar in a non-FP language.  Using these libraries results in safer and cleaner code.  This is a great effort. 
 
My personal preference is to work with raw computations directly.  This is what attracted me to FP and Haskell in the first place. 
To me, FP is the study of computations; I want to see the computations,
not look at some API facade. 

> *FP libraries are implemented in their corresponding languages.*   
> *Should we base functional programming on higher-order functions instead of proprietary language features[^propr]? Can we do it in a reasonably idiomatic and simple way?* (Open Portability Question)

[^propr]: This assumes a pragmatic approach to deciding what is proprietary; it is not a turtles-all-the-way-down proposal.

The answer could be a hybrid approach exposing both cozy interfaces and raw higher-order functions for hardcore users. 
You can (and probably should) disagree with me on this. We can still explore the raw capabilities of higher-order functions together, ignoring the question of how these are packaged.

 `(a -> b) -> s -> t` is the higher-order function I will start with and focus on. 


## `Setter` the HKT flattening case study

Consider the shape of `Functor` 

```Haskell
 (a -> b) -> f a -> f b
```

Introduce type variables `s` and `t` and substitute `f a` with `s` and `f b` with `t`.  We get 

```Haskell
 (a -> b) -> s -> t
```

which is the definition of `Setter s t a b`.

```Haskell
type Setter s t a b = (a -> b) -> s -> t
type Setter' s a = Setter s s a a
```

`Functor` can be viewed as `forall a b . (a -> b) -> f a -> f b` and we are losing this polymorphic aspect, but setters are still extremely useful. 

`Traversal` is an interesting computation because it does not have a simple HKT-free definition.  

```Haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

`Setter` immediately looks more mainstream-friendly. 
We can _widen_ `Traversal` to `Setter` without losing much of its power.  I justify this by thinking about `forall f. Applicative f` as a set of effects that is replaced with implicit IO. I think of this process intuitively as _flattening_ (&#128296;) HKT variables into plain old first-order type variables.  

We can think about `Setter` as an optic (as a `Traversal` &#128296; or an ordinary `Setter`) and as a computation that can replace `Functor` &#128296;. We are not taking the `Traversable`, `mapM` / `traverse` point of view. If you show the Haskell definitions of `traverse` or `mapM` to a pragmatic mainstream programmer, would they see it as something relevant to their work?  In my experience the answer is no. `Either`- and `Promise`-like types have become popular but come with isolated execution APIs and sometimes do not even expose a `mapM`/`traverse` equivalent.
To most programmers `map` and `traverse` would be the same, and this is what we are giving them here. People who have thought much more about it than I have (maintainers of Kotlin's arrow) have deprecated their [_add_blank_target `Traverse`](https://old.arrow-kt.io/docs/0.12/apidocs/arrow-core-data/arrow.typeclasses/-traverse/).

`Functor` and polymorphic `Traversal` need to satisfy certain laws. Most can be intuitively expressed as _structure preserving_. `s` and `t` need to have the same shape (e.g. a tree of `Int`s and a tree of `String`s with the same shape of branches). There is more to it[^laws], but all should be intuitively explainable IMO.

[^laws]: E.g. `Applicative` effects in `Traversal` should happen only once per traversed element.

Some readers may want to cry out: but it is not the same! We are losing functionality. In particular, `Traversal` and `Traversable traverse` are powerful computations that can recover folding capabilities!  

This gives me an opportunity to show how pearls are replaced with spoons or how type definitions are flattened in favor of implicit IO capabilities. The pearl:

```Haskell
newtype Const a b = Const a deriving Functor

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f `mappend` v)

constClassyFoldMap :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
constClassyFoldMap f = getConst . traverse (Const . f)

-- notice the implementation is identical
constFoldCombinatorMap :: (Monoid m) => Traversal s s a a -> (a -> m) -> s -> m
constFoldCombinatorMap trav f = getConst . trav (Const . f)
```

Here is the "spoon" (in Kotlin)

```Kotlin
typealias Setter<S, T, A, B> = (f: (A) -> B, s: S) -> T

// io version of foldMap based on just Setter 
// hf function is needed for polyoptic to collect A's in S, not needed if we use monooptic like in constFoldCombinatorMap. 
fun <S, T, A, B, M> ioFoldMap(
    mempty: M,
    mappend: (M, M) -> M,
    hf: (A) -> B, //allows A and B to be different
    setter: Setter<S, T, A, B>,
    f: (A) -> M,
    s: S,
): M {
    var acc = mempty
    setter({ x ->
        acc = mappend(acc, f(x))
        hf(x)
    }, s)
    return acc
}

fun <A> listMonoidEmpty(): List<A> = emptyList()

fun <A> listMonoidAppend(x: List<A>, y: List<A>): List<A> = x + y
```

You can use these with monomorphic types &#9989;

```Kotlin
data class MonoTest1(val ap1: Int, val ap2: Int, val ap3: List<Int>)

data class MonoTest2(val bp1: String, val bp2: String, val bp3: List<String>)

val testMonoTypeSetter: Setter<MonoTest1, MonoTest2, Int, String> = { f, s ->
    MonoTest2(f(s.ap1), f(s.ap2), s.ap3.map(f))
}

// == [1, 2, 3, 4], used to extract data
val testmonofold: List<Int> =
    ioFoldMap(listMonoidEmpty(), ::listMonoidAppend, Int::toString, testMonoTypeSetter, { listOf(it) }, MonoTest1(1, 2, listOf(3, 4)))


// == MonoTest2("1", "2", ["3", "4"]), used as setter optic
val testmonotraversal: MonoTest2 =
    testMonoTypeSetter(Int::toString, MonoTest1(1, 2, listOf(3, 4)))
```

You can use these with polymorphic types &#9989;

```Kotlin

data class Test<A>(val p1: A, val p2: A, val p3: List<A>)

fun <A, B> testSetter(): Setter<Test<A>, Test<B>, A, B> = { f, s ->
    Test(f(s.p1), f(s.p2), s.p3.map(f))
}

// testfold == [1,2,3,4], fold with "Setter' Test<Int> Int"
// Kotlin even infers the testSetter type
val testfold: List<Int> =
    ioFoldMap(listMonoidEmpty(), ::listMonoidAppend, { it }, testSetter(), { listOf(it) }, Test(1, 2, listOf(3, 4)))

// testfold2 == [1,2,3,4], fold with "Setter Test<Int> Test<String> Int String"
// Kotlin even infers the testSetter type
val testfold2: List<Int> =
    ioFoldMap(listMonoidEmpty(), ::listMonoidAppend, Int::toString, testSetter(), { listOf(it) }, Test(1, 2, listOf(3, 4)))

// used as setter optic
// Kotlin's local type inference does not work here, need to explicitly apply type arguments `<Int, String>`
val testpolytraversal: Test<String> =
    testSetter<Int, String>()(Int::toString, Test(1, 2, listOf(3, 4)))
```

No dreaded Kotlin contexts here!  No ad-hoc polymorphism so no context is needed. (More on this in the next section.)

Converting `Setter` to folds does not appear to be used in mainstream libraries but it could!  

We can add computational power to `Setter` by asking for more stuff. E.g., adding `s -> a` gives us an equivalent of `Lens s t a b`.  One should be somewhat careful with what is added because it is possible to add too much (e.g. Kotlin's _arrow_ `PTraversal<S,T,A,B>` expects to have an equivalent of `foldMap` defined for the polymorphic case `PTraversal<S,T,A,B>`  making it different from the standard definition where `foldMapOf` can only be defined for `Traversal s s a a`).

Also, `Setter` is almost first-class!  You may occasionally need to pass it rank-N style after the under-the-hood HKT is fixed, but this is supported in many mainstream languages (e.g. TypeScript and Kotlin; I show the Kotlin version in [Composing Setter](#composing-setter)). Also, it composes just as regular functions do.

One could argue that `(a -> b) -> s -> t` is too abstract and hard to understand for some engineers.  But we are in an environment with huge support for encapsulation.  In the simplest case the user will just see `SimpleModifier<S,A>` with private implementation and I am not discussing how this is done. 

## Scrap your ad-hoc  

Even in Haskell there was a movement [_add_blank_target Scrap your type classes](https://haskellforall.com/2012/05/scrap-your-type-classes) to avoid ad-hoc polymorphism in favor of explicit first-class computations.  I never liked this idea in Haskell but am proposing it for mainstream. 
The technique is often called explicit dictionary passing because of how Haskell type classes are implemented.  Dictionary refers to some kind of record-type argument.  One can also pass functions directly as arguments; I ignore this slight semantic difference. In this section I will explain 
why ditching ad-hoc is a good idea.  If you do not need convincing, then you only need the Kotlin definition of `Either` from this section to continue with [Composing Setter](#composing-setter).

To start, consider the ability to polymorphically create values of different types (e.g. `Monoid` `mempty`) as a programming language benchmark.  


| JVM PL   | Verdict      | Approach    | Notes     |
| :------  | :-----       |  :---       | :------------- |
| Java     | no           |             | developers use the factory pattern |
| Scala    | incoherent   | _implicits_ &emsp; &emsp; | changing imports can change runtime behavior (spooky action at a distance) |
| Kotlin  &emsp; &emsp; | cumbersome &emsp; &emsp;  | _context_   | developers write context code |

This does not look good. 

At a high level, Haskell ad-hoc polymorphism is linked to a type, OO ad-hoc polymorphism is linked to a runtime object. OO and the JVM use late binding (dynamic method dispatch), relying on runtime objects to achieve ad-hoc polymorphism.  To have type-specific implementations, there need to be runtime objects on the heap that point to a vtable (virtual method table) to surrogate the types. That feels like the wrong way to go about it, because it is.

Kotlin calls these _witness objects_.   In addition to runtime cost, this requires explicit code specifying that surrogate/witness object (`context` in Kotlin). This ends up being _cumbersome_ (especially if you try this with FP). Terminology like "context bubbling" is being used.  It feels like writing compiler code, not application code. These witnesses/surrogates need to identify concrete types (e.g. `List<String>` not `List<T>`), and this gets very redundant.  

Scala implicits accomplish type-specific ad-hoc through compile-time resolution and rewriting, removing the boilerplate. You pay with compilation time and incoherence risk. To summarize, on the JVM we are getting a very strong signal to stay away from type-based ad-hoc polymorphism. 

Things improve outside the JVM: Swift supports type-based ad-hoc using _protocols_ and _protocol extensions_. Swift uses witness tables, which function in a similar way to Haskell's dictionary passing. The compiler giving up on you when you use complex extensions is a concern in Swift though (I am a Swift newb and I could be misrepresenting things here). Monomorphization could do it (modern C++ templates, Rust) but I know even less about the cost-benefit ratio there.  I believe that avoiding ad-hoc could be beneficial outside the JVM, but I am not in a good position to claim this.

Moving forward I will use this expression to benchmark a language's ability to 
handle composable computations polymorphically on "small" types:

```Haskell
-- Benchmark:
-- use this expression to define function `Either String Int -> Either String Int` 
-- and see how cumbersome this is
computation length . computation (map length) . computation (:[]) . computation show . computation (+2)


-- e.g.: benchmark passes in Haskell (code is simple)
testcomp :: Either String Int -> Either String Int
testcomp =  computation length . computation (map length) . computation (:[]) . computation show  .  computation  (+2)
   where computation = fmap
```

 
_If I cannot express `f a` then I can as well reason about `fa`._  &#128512;
Using `Setter` as an inspiration, I am tempted to encode `Functor` like this

```Haskell
-- pragmas not shown (MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MonoLocalBinds, RankNTypes, ...)

-- a classy version of Setter specialized to Functor
class FlatFunctor fa fb a b where 
   flatmap :: (a -> b) -> fa -> fb

-- less cumbersome to use version but Kotlin does not support it and I will not use it
class FlatFunctor' fa fb a b | fa -> a, fa b -> fb where 
   flatmap' :: (a -> b) -> fa -> fb   

-- Note the same `fmap` implementation works
instance FlatFunctor (Either e a) (Either e b) a b where 
    flatmap = fmap

-- benchmark passes in Haskell, (computation still is simple)
-- it will fail in Kotlin (cumbersome context code)
testcomp :: Either String Int -> Either String Int
testcomp =  computation length . computation (map length) . computation (:[]) . computation show  .  computation  (+2)
   where 
        computation :: forall a b. FlatFunctor (Either String a) (Either String b) a b => (a -> b) -> Either String a -> Either String b
        computation = flatmap
```

This is all doable in Kotlin

```Kotlin
interface FlatFunctor<MA, MB, A, B> {
    fun flatmap(f: (A) -> B, ma: MA): MB
}

// not curried version of flatmap
context(flat: FlatFunctor<MA, MB, A, B>)
fun <MA, MB, A, B> flatmap(f: (A) -> B, ma: MA): MB = flat.flatmap(f, ma)

// curried version of flatmap
context(flat: FlatFunctor<MA, MB, A, B>)
fun <MA, MB, A, B> flatmap(f: (A) -> B): (MA) -> MB = { ma -> flat.flatmap(f, ma) }

sealed interface Either<E, A> {
    data class Left<E, A>(val left: E) : Either<E, A> // Either<E, Nothing> is more idiomatic, Nothing is a bottom, Void-like type.
    data class Right<E, A>(val right: A) : Either<E, A> // Either<Nothing, A> is more idiomatic
}

// THE CULPRIT, we will need to bring this one into scope using `context`
fun <E, A, B> eitherFlatFunctor(): FlatFunctor<Either<E, A>, Either<E, B>, A, B> =
    object : FlatFunctor<Either<E, A>, Either<E, B>, A, B> {
        override fun flatmap(f: (A) -> B, ma: Either<E, A>): Either<E, B> = when (ma) {
            is Either.Left -> Either.Left(ma.left)
            is Either.Right -> Either.Right(f(ma.right))
        }
    }
```

_side_note_start  **Warning:**  using type bounds has safety issues &#10060;

```Kotlin
interface FlatFunctor<BOUND> {
    fun <MA : BOUND, MB : BOUND, A, B> flatmap(f: (A) -> B, ma: MA): MB
}
```

OO often dilutes type safety, and it does so here: the caller chooses `MB`, so the implementation can only produce it with an unchecked cast.
_side_note_end

With some extra boilerplate not shown (e.g. `compose` implementation), I (a Kotlin newb) end up with a very cumbersome `testcomp` because of `context` compiler obligations I need to satisfy (benchmark fails &#10060;).

```Kotlin
// benchmark failed, too cumbersome
fun testcomp(e: Either<String, Int>): Either<String, Int> =
    context(
        eitherFlatFunctor<String, Int, Int>(),
        eitherFlatFunctor<String, Int, String>(),
        eitherFlatFunctor<String, String, List<String>>(),
        eitherFlatFunctor<String, List<String>, List<Int>>(),
        eitherFlatFunctor<String, List<Int>, Int>(),
    ) {
        val pipeline: (Either<String, Int>) -> Either<String, Int> =
            flatmap<_, _, List<Int>, Int> { it.size } compose
            flatmap<_, _, List<String>, List<Int>> { l -> l.map { it.length } } compose
            flatmap<_, _, String, List<String>> { listOf(it) } compose
            flatmap<_, _, Int, String> { it.toString() } compose
            flatmap<_, _, Int, Int> { it + 2 }
        pipeline(e)
    }
```

The same "instance" of `FlatFunctor` is used 5 times but we have to specify it for each use!
There are workarounds to make this less painful.  They are complex and have type safety issues.
I am advocating for avoiding ad-hoc in favor of direct "dictionary passing" style.  In Haskell, typeclasses provide better safety (coherence) and lots of convenience.  This type-based ad-hoc in Kotlin is still incoherent and very cumbersome.  The extra cost of objects on the heap is a concern too.  Let's ditch it!

## Composing `Setter`

Note that we need higher rank (ability to pass a polymorphic function as an argument) so it can be used with any `a -> b`s.

Haskell for reference: 

```Haskell
eitherSetter :: Setter (Either String a) (Either String b) a b 
eitherSetter = fmap -- it's all the same

-- benchmark passes in Haskell (requires rank-N but code remains simple)
testcomp :: (forall a b . Setter (Either String a) (Either String b) a b) -> Either String Int -> Either String Int
testcomp setter = computation length . computation (map length) . computation (:[]) . computation show . computation (+2)
   where computation = setter
```

Kotlin:

```Kotlin
// Either (a rather standard definition) can be found in the previous section
fun <E, A, B> eitherSetter(): Setter<Either<E, A>, Either<E, B>, A, B> = { f, s ->
    when (s) {
        is Either.Left -> Either.Left(s.left)
        is Either.Right -> Either.Right(f(s.right))
    }
}

// we need a wrapper to pass Setter polymorphically for "small" `A`s and `B`s 
// this is the rank-N definition, in Kotlin you do this with a wrapping interface
// comp(f) is syntax sugar for comp.invoke(f), similar to Scala apply.
interface PolyEitherSetter<E> {
    operator fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B>
}

fun <E> polyEitherSetter(): PolyEitherSetter<E> = object : PolyEitherSetter<E> {
    override fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B> = { s -> eitherSetter<E, A, B>()(f, s) }
}

// now we have a cleaner testcomp benchmark pass!
// Kotlin still has issues with inferring types but this is much better
// 1 = testcomp(polyEitherSetter<String>())(Either.Right(4))
fun <E> testcomp(smap: PolyEitherSetter<E>): (Either<E, Int>) -> Either<E, Int> =
    smap { l: List<Int> -> l.size } compose
        smap { l: List<String> -> l.map { it.length } } compose
        smap { s: String -> listOf(s) } compose
        smap { x: Int -> x.toString() } compose
        smap { x: Int -> x + 2 }
```

The `testcomp` benchmark passes &#9989;. IMO, we have a much saner coding environment.
`testcomp` is effectively a higher-order function with `interface PolyEitherSetter` used to pass the `Setter` polymorphically.
We avoid ad-hoc, we avoid `context`!   

## Using LLMs

Selecting a new programming language ought to include checking vibe-coded code quality. Kotlin did not do very well in my experiments.  I attribute this to Java bloat, conflicting paradigms, and legacy patterns in the training data (e.g. `class` has over 14 variants with sometimes subtle differences).  Code synthesis was always about a small solution space; ambiguity cannot be a good thing.  I am also concerned about the presence of Java booby-traps in the generated code.  


It is also interesting to think about the long-term effects of vibe coding on code quality.
My prompts about concurrency in Kotlin often produce examples with subtle race conditions.
I expect this situation to get worse when vibe coding replicates these bugs,
causing models to collapse on wrong solutions.
Maybe not surprisingly, if I ask for concurrency program examples using the Kotlin _arrow_ library (an FP library that tries to be Kotlin-idiomatic and approachable), I get what look like bug-free examples. We are starting to observe a decrease in overall software quality and this trend now has a self-feeding loop. In areas that are not easily testable, vibe coding could lead to quality collapse and model collapse at the same time.  
In the end, we may see a reemergence of FP to improve the situation. 

It appears that LLMs may be starting to use _Lean_ beyond math proofs[^lean]. Will Lean become the reasoning engine under the hood, the anti-hallucinatory pill for LLMs?  Extracting semantics from, say, a legal document or a poem into Lean is &#x1F92F;. Is _Lean_ the future king of prompting?  

[^lean]: [_add_blank_target Lean4Agent](https://github.com/RickySkywalker/Lean4Agent) models and verifies agent behavior, [_add_blank_target VeriBench](https://icml.cc/virtual/2025/52376): End-to-End Translation of Executable Python Tasks into Structured Lean 4 Artifacts, [_add_blank_target LeanReasoner](https://arxiv.org/html/2403.13312v1), 
[_add_blank_target LeanReasoner Training Paradigm](https://openreview.net/forum?id=maT9JAAz_u) ... 

Vibe coding is such a game changer. I am completely new to Kotlin and can 
explore its capabilities in depth. I can prompt on a conceptual level and then dive deep into generated Kotlin code, exploring its limitations. 
I can prompt by writing Haskell code snippets, prompt with PLT terminology, and even use operational semantics.  Claude Code even told me that one of my construction rules was not constructive and found a nuanced code issue based on semantic definitions. How impressive is that! 
People are vibe coding open-source Rust replacements of whole application suites like Adobe or Microsoft Office.  There is undeniable power there and finding effective and safe ways of using this tech can lead us into more FP and interesting jobs. 
No one has a crystal ball.  My heart is with Idris and Agda, but I started learning _Lean 4_, possibly the last PL I learn before unemployment...


## Closing thoughts

Why did a Kotlin newb write about Kotlin?  Answer: I do not know a lot about Kotlin; I wrote about higher-order functions.
But I am finding that Kotlin has a huge amount of nuance and cognitive bloat.  Lots of this is a result of Java interop doom.  Some is probably of Kotlin's own making. I have mixed feelings about selecting it for backend development. 

I have advocated here for ditching type-based ad-hoc (known in Kotlin as Context Oriented Programming) in favor of using higher-order functions and passing instance definitions directly ("dictionary passing" style).  Consider that Kotlin _arrow_'s own `PTraversal` is defined as an _interface_ in terms of (using Haskell syntax)

```Haskell
-- arrow's PTraversal in Haskell syntax
foldMap :: m -> ((m,m) -> m) -> s -> (a -> m) -> m
modify :: s -> (a -> b) -> t
```

Note that the `Monoid` constraint is replaced with two explicit arguments `m, (m,m) -> m`.  That is consistent with the direction I would like to see. 
I am not a Kotlin dev.  However, my reading tells me the community has largely abandoned using contexts for more polymorphic expressions.  It is still considered very beneficial for certain coding tasks.  Me doing deep sigh "yet another booby-trap complexity to know about". 

> _Not fully safe, very inconvenient to use, what are we waiting for?_   
>   &emsp; &emsp;  -  Gimli's early excitement about Kotlin contexts

As a side note on `PTraversal`: I would prefer to define it as a higher-order function `(a -> b) -> s -> t`.  These 2 approaches are not equivalent (mine is closer to the Haskell _lens_ library or van Laarhoven definition) but are close.  I like to favor raw functions.  _arrow_ maintainers, I am sure, have done a lot of thinking about it and selected an interface and a different set of underlying computations.  The `PTraversal` interface defines 41 additional methods.  My preference is to have separate combinators but this is not a widely shared preference outside of FP. IMO, the less magic the better, but I see the benefits of hiding FP as well.

I have discussed flattening HKTs using `Setter`. Here are some non-`Setter` examples that flatten HKTs

```Haskell
type FlatFold ta a = forall m . m -> ((m,m) -> m) -> (a -> m) -> ta -> m

type Embed ta a = a -> ta
type Flatten ta a = ta -> a 

-- Example: Setter, Embed, Flatten lets you express an awkward-to-use Monad e.g. `Either e`  by passing these 3 args
--
-- (forall a b . Setter (Either e a) (Either e b) a b)
-- (forall a . Embed (Either e a) a)
-- (forall a . Flatten (Either e (Either e a)) (Either e a))

flatBind :: Flatten mmb mb -> Setter ma mmb a mb ->  (a -> mb) -> ma -> mb
flatBind flatten map f = flatten . map f

-- >>> test pure join fmap (Right 1)
-- Right "1"
test :: 
    (forall a . Embed (Either e a) a) ->    
    (forall a . Flatten (Either e (Either e a)) (Either e a)) ->
    (forall a b . Setter (Either e a) (Either e b) a b) -> 
    (Either e Int) -> (Either e String)
test embed flatten setter e = flatBind flatten setter (embed . show) e 
```

`Bifunctor`, `Profunctor`, ... The list of what can be easily flattened will probably not be long.  Also, I am not suggesting using all of it.  It is important to be selective.  Otherwise it is easier to stay with Haskell. 

_Using a non-FP language makes FP coding harder and more complex rather than easier or simpler._

I like to distinguish _easy_ (to learn) from _simple_ (to understand) (see [_add_blank_target Easy vs Simple](2022-08-30-code-cognitiveload.html#easy-vs-simple)). Easy ~= knowledge, simple ~= understanding.  IMO, flattening HKTs does not make things _easier_. HKTs abstract over type construction, which is one side of the construction coin. Flattening hides the learning path to one of the cornerstones of programming (for me _construction is the foundation_).  
Probably, when designing a library we have interfaces = easy,  higher-order functions = simple.  However, my portability question is valid here: 
  
> _Should we base functional programming on higher-order functions instead of proprietary language features?_

I have not done much mainstream programming for a while now. 
But I had a brief interlude with TypeScript The Horrible, 4 years ago ([_add_blank_target TypeScript Notes](/tags/TypeScript-Notes.html)). 
I tried to write more functional code but mostly gave up as the language and ecosystem fought against me.  Creating an FP library in such an ecosystem is a nuanced task. 

Higher-order functions would force programmers to think more deeply about computations. They are not _easy_.   

> _Certainty of PR rejection, small chance of success. What are we waiting for?_

A hybrid approach providing both interfaces and raw computations?  
Your feedback is appreciated: [_add_blank_target GitHub discussion](https://github.com/rpeszek/rpeszek.github.io/discussions/7).





