---
title: Life after Haskell, HKT flattening (examples in Kotlin)
featured: true
summary: Flattened HKTs and higher-order functions in mainstream programming
toc: true
tags: Haskell
codestyle: newstyle
---

**DRAFT STATUS**

> *It's no use trying to eat a steak with a teaspoon, and a straw.*   
> &emsp; &emsp; - Anthony T. Hincks  

This quote describes my thoughts about using mainstream languages to write functional programs. But, maybe there are some weird spoons and straws that 
replace some of the functional pearls? 

I will complain about booby-traps in popular mainstream languages. 
If you are tired of such complaints, go directly to [Setter the HKT flattening case study](#setter-the-hkt-flattening-case-study) where I consider flattening HKTs. `f a` becomes `fa` (a big type) and `a` sticks around (a small type). `(a -> b) -> f a -> f b` becomes a plain `(a -> b) -> fa -> fb`. 
This way `Functor` is replaced with a plain `Setter`.  The `Traversal` optic ends up replaced with a `Setter` using implicit IO.  

I advocate for avoiding type-based ad-hoc polymorphism (Context Oriented Programming in Kotlin) in favor of passing computations like `Setter s t a b` as first-class, direct arguments (a "dictionary passing"-like style).  This promotes programming with higher-order functions.


You may also find [Using LLMs](#using-llms) interesting.

*Written for functional programmers familiar with Haskell, optics, and Rank-N who work with mainstream languages.
Examples are in Kotlin (I am new to Kotlin).*

## My story

I work on a large Haskell code base that has been sadly shrinking over the last few years.  The biggest piece still in use is a proprietary programming language that allows manipulating and combining web service endpoints in a type-safe way.  It also simplifies UI development.  The UI piece is our "low code" solution for fast development of mostly short-lived apps. LLMs and tooling based on LLMs are replacing the "low code". So a large part of the Haskell code base will no longer be needed. Our language uses "big" and "small" types terminology and that seems approachable. I will explore this idea here in the context of doing FP in a popular mainstream language. 

A bigger issue is finding and hiring functional programmers.  Our codebase does not do [_add_blank_target Simple Haskell](https://www.simplehaskell.org/) / [_add_blank_target Boring Haskell](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/). I am not sure if I even like these ideas. I get it. It is not easy. I have a math background and I did math research before I started software programming. Learning FP was fascinating but not easy for me. 
_It is as hard to hire developers who know FP as it is for a developer who knows FP to find an FP job._
At the same time (based on my past experience as an interviewer):
_It may be easier to find a developer who knows FP than a Java/TypeScript/... developer who understands Java/TypeScript/... booby-traps._
IMO, this is a problem with industry priorities and how we learn to program. Frankly, I am scared.

_side_note_start Not my story:  Uber's Go codebase was suffering from frequent crashes. Ignoring the Halting Problem / Rice's Theorem as too theoretical, Uber hired a bunch of PhDs to create NilAway, a linter solution to null reference issues. It is written in Go. Apparently it is known to crash with a null reference panic from time to time &#128512;
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
I have not used Java for a long time. I do not remember all the Java gotchas. I am using LLMs to try 
to identify Kotlin booby-traps and catalogue them. After 4 hours of doing this, I wrote down 12 gotchas. 3 more the next day...
In my experience, developers (junior or senior) are completely unaware of these issues.

Kotlin's surface area is very bloated.  `class` can be: `open`, `abstract`, `data`, `@JvmRecord data`, `sealed`, `value`, `enum`, `annotation`, `expect` / `actual`, `external`, `inner`..., or just `class`.  `interface` adds 3 more variants.  Clearly not very approachable. 

> _If you see an object you should object_   
> &emsp; &emsp; - Phil Wadler


I did some math research, then 2 decades of mainstream programming with many battle scars, then an 8-year-long Haskell job (some math came back only different), now I am going back to mainstream.  Is some convergence possible?  Can I find some middle-ground approach to programming? 
 
## Higher-order functions 

Haskell-like FP in a non-FP language is hard to do. The biggest culprit is the lack of support for higher-kinded / second-order types.  There is a technique called _lightweight higher-kinded types_ or _defunctionalization of higher-kinded types_.  The libraries based on this technique are hard to use and probably harder to learn than Haskell itself.

There is an effort to create a more approachable FP ecosystem (e.g. arrow in Kotlin - new versions, Effect in TypeScript, returns in Python, Vavr in Java ...).  These libraries hide internal implementation of FP concepts behind a "frontend" that feels more idiomatic and familiar in a non-FP language.  Using these libraries results in safer and cleaner code.  This is a great effort. 

My goal is to explore the raw capabilities of higher-order functions, mostly ignoring the question of how these are packaged. 
My personal preference is to work with raw computations directly.  This is what attracted me to FP and Haskell in the first place. 
To me, FP is a study of computations, I want to see the computations
not look at a some API facade. 

> *FP libraries are implemented in their corresponding languages.*   
> *Should we use higher-order functions instead of proprietary language features to create a functional programming frontend?[^propr]. Can we do it in a reasonably idiomatic and simple way?* (Open Portability Question)

[^propr]: With a pragmatic approach to what is proprietary, it is not turtles all the way down proposal.

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

`Traversal` is an interesting optic because unlike `Lens` or `Prism` it is not so easy to define it with the old record approach[^oldlens].  

[^oldlens]: E.g. old [data-lens](https://hackage.haskell.org/package/data-lens-2.11.2/docs/Data-Lens-Common.html) package. Still visible as `lens` and `prism` combinators in the modern lens package.

```Haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

`Setter` immediately looks more mainstream-friendly. 
We can _widen_ `Traversal` to `Setter`.  I justify this by thinking about `forall f. Applicative f` as a set of effects that is replaced with implicit IO. I think of this process as _flattening_ (&#128296;) HKT into plain old first-order type variables.  

We can think about `Setter` as an optic (as a `Traversal` &#128296; or an ordinary `Setter`) and as a computation that can replace `Functor` &#128296;. We are not taking the `Traversable`, `mapM` / `traverse` point of view. If you show the Haskell definitions of `traverse` or `mapM` to a pragmatic mainstream programmer, would they see it as something relevant to their work?  In my experience the answer is no. `Either`- and `Promise`-like types have become popular but come with isolated execution APIs and sometimes do not even expose a `mapM`/`traverse` equivalent.
To most programmers `map` and `traverse` would be the same, and this is what we are giving them here. People who have thought much more about it than I have (maintainers of Kotlin's arrow) have deprecated their [_add_blank_target `Traverse`](https://old.arrow-kt.io/docs/0.12/apidocs/arrow-core-data/arrow.typeclasses/-traverse/).

`Functor` and polymorphic `Traversal` need to satisfy certain laws. These can be intuitively expressed as _structure preserving_. `s` and `t` need to have the same shape (e.g. a tree of `Int`s and a tree of `String`s with the same shape of branches). All of this is intuitively explainable IMO.

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

You can use these with monomorphic types:

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

You can use these with polymorphic types 

```Kotlin

data class Test<A>(val p1: A, val p2: A, val p3: List<A>)

fun <A, B> testSetter(): Setter<Test<A>, Test<B>, A, B> = { f, s ->
    Test(f(s.p1), f(s.p2), s.p3.map(f))
}

// testfold == [1,2,3,4], fold with "Setter' Test<Int> Int"
val testfold: List<Int> =
    ioFoldMap(listMonoidEmpty(), ::listMonoidAppend, { it }, testSetter<Int, Int>(), { listOf(it) }, Test(1, 2, listOf(3, 4)))

// testfold2 == [1,2,3,4], fold with "Setter Test<Int> Test<String> Int String"
val testfold2: List<Int> =
    ioFoldMap(listMonoidEmpty(), ::listMonoidAppend, Int::toString, testSetter<Int, String>(), { listOf(it) }, Test(1, 2, listOf(3, 4)))

// used as setter optic
val testpolytraversal: Test<String> =
    testSetter<Int, String>()(Int::toString, Test(1, 2, listOf(3, 4)))
```

Kotlin local type inference works poorly (it does not infer `testSetter` `A` `B` types) but no dreaded Kotlin contexts here!  No ad-hoc polymorphism so no context is needed. (More on this in the next section.)

Converting `Setter` to folds does not appear to be used in mainstream libraries but it could!  

We can add computational power to `Setter` by asking for more stuff. E.g., adding `s -> a` gives us an equivalent of `Lens s t a b`.  One should be somewhat careful with what is added because it is possible to add too much (e.g. Kotlin's _arrow_ `PTraversal<S,T,A,B>` expects to have an equivalent of `foldMap` defined for the polymorphic case `PTraversal<S,T,A,B>`  making it different from the standard definition where `foldMapOf` can only be defined for `Traversal s s a a`).

Also, `Setter` is almost first-class!  You may occasionally need to pass it rank-N style as `(forall s t a b . Setter s t a b)` but this is supported in many mainstream languages (e.g. TypeScript and Kotlin; I show the Kotlin version in [Composing Setter](#composing-setter)). Also, it composes just as regular functions do.

One could argue that `(a -> b) -> s -> t` is too abstract and hard to understand for some engineers.  But we are in an environment with huge support for encapsulation.  In the simplest case the user will just see `SimpleModifier<S,A>` with private implementation and I am not discussing how this is done. 


## Scrap your ad-hoc  

Even in Haskell there was a movement [_add_blank_target Scrap your type classes](https://haskellforall.com/2012/05/scrap-your-type-classes) to avoid ad-hoc polymorphism in favor of explicit first-class computations.  I never liked this idea in Haskell but am proposing it for mainstream. 
The technique is often called explicit dictionary passing because of how Haskell type classes are implemented.  Dictionary refers to some kind of record-type argument.  One can also pass functions directly as arguments; I ignore this slight semantic difference.
  
To start, consider the ability to polymorphically create (e.g. `Monoid` `mempty`) as a programming language benchmark

```Haskell
mempty                          --Haskell yes
[A: Monoid] .. Monoid[A].zero   --Scalaz  yes

ListMonoid.empty()               //Java   no 
monoidFactory.empty()

context (...) empty()            //Kotlin almost

```

Monomorphization could do it (C++ templates, Rust can, Go chooses not to).  Monomorphization is a complex topic beyond my current PLT interests. 

_Factory Pattern was invented because this is a problem._

At a high level, Haskell ad-hoc polymorphism is linked to a type, OO ad-hoc polymorphism is linked to a runtime object. OO and the JVM use late binding, dynamic method dispatch based on runtime objects to achieve ad-hoc polymorphism.  To have type-specific implementations, there need to be runtime objects on the heap that point to a vtable (virtual method table) to surrogate the types. Kotlin calls these _witness objects_.
In practice this requires explicit code specifying that surrogate/witness object (`context` in Kotlin). This ends up being _cumbersome_ (especially if you try this with FP). Terminology like "context bubbling" is being used.  It feels like writing compiler code, not application code. These "witnesses"/surrogates need to identify concrete types (e.g. `List<String>` not `List<T>`), and this gets very redundant and is unavoidable with any attempt at ad-hoc resolution of HKTs.  

Scala implicits accomplish type-specific ad-hoc through compile-time resolution and rewriting, removing the boilerplate; other languages do not. 

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

-- e.g.: benchmark still passes in Haskell (requires rank-N but code remains simple)
-- it will pass in Kotlin
testcomp :: (forall a b . Setter (Either String a) (Either String b) a b) -> Either String Int -> Either String Int
testcomp setter = computation length . computation (map length) . computation (:[]) . computation show . computation (+2)
   where computation = setter
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

_side_note_start  **Warning:**  using type bounds has safety issues 

```Kotlin
interface FlatFunctor<BOUND> {
    fun <MA : BOUND, MB : BOUND, A, B> flatmap(f: (A) -> B, ma: MA): MB
}
```

OO often dilutes type safety, and it does so here: the caller chooses `MB`, so the implementation can only produce it with an unchecked cast.
_side_note_end

With some extra boilerplate not shown (e.g. `compose` implementation), I (a Kotlin newb) end up with a very cumbersome `testcomp` because of `context` compiler obligations I need to satisfy.

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
If I were forced to use `FlatFunctor`, I would try to convert it to a `Setter` and hope for the best:

```Kotlin
interface PolyEitherSetter<E> {
    operator fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B>
}

// can be used without specifying additional context 
fun <E> polyEitherSetter(): PolyEitherSetter<E> = object : PolyEitherSetter<E> {
    override fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B> =
        context(eitherFlatFunctor<E, A, B>()) { flatmap(f) }
}
```

I am advocating for avoiding ad-hoc in favor of direct "dictionary passing" style.  In Haskell, typeclasses provide better safety (coherence) and lots of convenience.  This type-based ad-hoc in Kotlin is still incoherent and very cumbersome.  Let's ditch it!



### Composing `Setter`

Note that we need higher rank (ability to pass a polymorphic function as an argument) so it can be used with any `a -> b`s.

```Kotlin
// Either is defined above
fun <E, A, B> eitherSetter(): Setter<Either<E, A>, Either<E, B>, A, B> = { f, s ->
    when (s) {
        is Either.Left -> Either.Left(s.left)
        is Either.Right -> Either.Right(f(s.right))
    }
}

// we need a wrapper to pass Setter polymorphically for "small" `A`s and `B`s 
// this is the rank-N definition, in Kotlin you do this with a wrapping interface
// comp(f) is syntax sugar for comp.invoke(f), similar to Scala apply.
// I reuse the same names introduced in the previous section on purpose
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

The `testcomp` benchmark passes; IMO, we have a much saner coding environment.
`testcomp` is effectively a higher-order function with `interface PolyEitherSetter` used to pass the `Setter` polymorphically.
We avoid ad-hoc, we avoid `context`!   

_side_note_start In Haskell, constraints can be converted to dictionaries. E.g. using the previous definitions:

```Haskell
eitherSetter :: Setter (Either e a) (Either e b) a b
eitherSetter = flatmap
```

This technique seems to lead somewhere with Kotlin's contexts as well.  I do not fully understand how this discharging of context obligations works and 
I am not discussing it here.  My preference for avoiding ad-hoc polymorphic code is unchanged. 
_side_note_end

## Using LLMs

I am new to Kotlin and I am using LLMs to learn Kotlin and its quirks.
I am observing that the answers depend on the lingo I use in my prompts. I noticed that using Kotlin lingo when querying for Kotlin gotchas does not give me the best answers. 
A prompt like this "Are there any issues with inference widening in Kotlin" could give a much better answer. Asking for unsafe coercion may give different results than asking about casting. 
Downcasts or narrowing... etc. The lingo you use in the prompt probably changes what parts of the original training dataset impact the token generation. This would also suggest that Kotlin programmers do not know the issues I am trying to identify. 

In my testing, interactive vibe coding with `Setter` is giving me good results.  I see very mixed results 
with generating code based on Kotlin _arrow_ library's `PTraversal<S,T,A,B>`.  The LLM seems to get confused much less with `Setter<S,T,A,B>` as I defined it above.  I think this is again explained by where the LLM pulls the patterns from.  With `Setter` it could be pulling more from Haskell and translating to Kotlin.  Kotlin language complexity probably makes it confused (e.g. coding with `class` vs `data class` has nuanced differences, `class` has over 14 variants with sometimes subtle differences, `interface` adds 3 more).  The nuance of the same code pattern being correct in some cases and not in others is hard on humans; it is probably hard on LLMs too. 
FP could circumvent this nuance.

It is also interesting to think about the long-term effects of vibe coding on code quality.
My prompts about concurrency in Kotlin often produce examples with obvious race conditions.
This makes me concerned about the general quality of Kotlin code that the LLM trained on (which probably is mostly open source).
I expect this situation to get worse when vibe coding replicates these bugs,
causing models to collapse on wrong solutions.
Maybe not surprisingly, if I ask for concurrency program examples using the Kotlin _arrow_ library (an FP library that tries to be Kotlin-idiomatic and approachable), I get what look like bug-free examples. We are starting to observe a decrease in overall software quality and this trend now has a self-feeding loop. Vibe coding could lead to quality collapse and model collapse at the same time.  Possibly, vibe coding functional programs will still produce OK results.
At the end, we may see a reemergence of FP, and this time with even stricter types &#128512; to improve the situation.

Vibe coding is such a game changer. I am completely new to Kotlin and can 
explore its capabilities in depth. I can prompt on a conceptual level and then dive deep into generated Kotlin code, exploring its limitations. 
I can prompt by writing Haskell (even Idris) code snippets, prompt with PLT terminology, and even use operational semantics.  Claude Code even told me that one of my construction rules was not constructive, how great is that! 
People are vibe coding open-source Rust replacements of whole application suites like Adobe or Microsoft Office.  There is undeniable power there and finding effective and safe ways of using this tech can lead us into more FP. 
No one has a crystal ball.  Hopefully the unemployment period will not be too long.


## Closing thoughts

Why did a Kotlin newb write about Kotlin?  Answer: I do not know a lot about Kotlin; I wrote about higher-order functions.
But I am finding that Kotlin has a huge amount of nuance and cognitive bloat.  Lots of this is a result of Java interop doom.  Some is probably of Kotlin's own making.

I have advocated here for ditching type-based ad-hoc (known in Kotlin as Context Oriented Programming) in favor of using higher-order functions and passing instance definitions directly ("dictionary passing" style).  Consider that Kotlin _arrow_'s own `PTraversal` is defined as an _interface_ in terms of (using Haskell syntax)

```Haskell
foldMap :: m -> ((m,m) -> m) -> s -> (a -> m) -> m
modify :: s -> (a -> b) -> t
```

Note that the `Monoid` constraint is replaced with two explicit arguments `m, (m,m) -> m`.  That is consistent with the direction I would like to see. 
I am not a Kotlin dev.  However, my reading tells me the community has largely abandoned using contexts for more polymorphic expressions.  It is still considered very beneficial for certain coding tasks.  Me doing deep sigh "yet another booby-trap complexity to know about". 

> _Not fully safe, very inconvenient to use, what are we waiting for?_   
>   &emsp; &emsp;  -  Gimli's early excitement about Kotlin contexts

As a side note on `PTraversal`: I would prefer to define it as a higher-order function `(a -> b) -> s -> t`.  These 2 approaches are not equivalent (mine is closer to the Haskell _lens_ library definition) but are close.  I like to favor raw functions.  _arrow_ maintainers, I am sure, have done a lot of thinking about it and selected an interface and a different set of underlying computations.  The `PTraversal` interface defines 41 additional methods.  My preference is to have separate combinators but this is not a widely shared preference outside of FP. 


I have discussed flattening HKTs using `Setter`. Here are some non-`Setter` examples that flatten HKTs

```Haskell
type FlatFold ta a = forall m . m -> ((m,m) -> m) -> (a -> m) -> ta -> m

type Embed ta a = a -> ta
type Flatten ta a = ta -> a 

-- Example: Setter, Embed, Flatten lets you express Monad e.g. `Either e`  by passing these 3 args
(
    forall a b . Setter (Either e a) (Either e b) a b,
    forall a . Embed (Either e a) a,
    forall a . Flatten (Either e (Either e a)) (Either e a)
)
```

`Bifunctor`, `Profunctor`, ...   I am not suggesting using these.  It is important to be selective.  Otherwise it is easier to stay with Haskell. 

_Using a non-FP language makes FP coding harder rather than easier._

I like to distinguish easy (to learn) from simple (to understand) (see [_add_blank_target Easy vs Simple](2022-08-30-code-cognitiveload.html#easy-vs-simple)). Easy ~= knowledge, simple ~= understanding.  Mainstream programming lacks a language choice that is approachable and leads to simple (Elm is not mainstream).
Probably, when designing a library we have interfaces = easy,  higher-order functions = simple.  However, my portability question is valid here: 
  
> _Should we base functional programming on higher-order functions instead of proprietary language features?_



 





