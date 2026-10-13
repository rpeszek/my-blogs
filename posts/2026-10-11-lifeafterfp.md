---
title: Life after Haskell, HKT flattening (in Kotlin)
featured: true
summary: todo.
toc: true
tags: Haskell
---

**DRAFT STATUS**

> *It's no use trying to eat a steak with a teaspoon, and a straw.*
> - Anthony T. Hincks  

This quote describes my thoughts about using mainstream languages to write functional programs. But, maybe there are some weird spoons and straws that 
replace some of the functional pearls? 

I will complain about booby-traps in popular mainstream languages. 
If you are tired of such complaints then go directly to [Setter is interesting](#setter-is-interesting) where I consider flattening HKTs. `f a` becomes `fa` (a big type) and `a` sticks around (a small type). `(a -> b) -> f a -> f b` becomes a plain `(a -> b) -> fa -> fb`. 
The idea of, say, replacing `Traversal` optic with plain `Setter` using implicit IO is something you may not have thought about.  Also I will advocate for avoiding ad-hoc polymorphism in favor of passing computations like `Setter s t a b` as first class, direct argument.
You may also find [Using LLMs](#using-llms) interesting.

*Written for functional programmers familiar with Haskell, optics, and Rank-N who work with mainstream languages.
Examples are in Kotlin (I am new to Kotlin).*

## My story

I work on a large Haskell code base that has been sadly shrinking over the last few years.  The biggest piece still in use is a proprietary programming language allowing to manipulate and combine web service endpoints in a type safe way.  It also simplifies UI development.  The UI piece is our "low code" solution for fast development of mostly shot lived apps. LLMs and tooling based on LLMs are replacing the "low code". So a large part of the Haskell code base will become not needed. Our language uses "big" and "small" types terminology and that seems approachable. I will explore this idea here in the context of doing FP in a popular mainstream language. 

A bigger issue is finding and hiring functional programmers.  Our codebase does not do [_add_blank_target Simple Haskell](https://www.simplehaskell.org/) / [_add_blank_target Boring Haskell](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/). I am not sure if I even like these ideas. I get it. It is not easy. I have a math background and I did math research before I started software programming. Learning FP was fascinating but not easy for me. 

> _It as hard to hire developers who know FP as it is for a developer who knows FP to find an FP job_

at the same time (based on my past experience as interviewer):

> _It may be easier to find a developer who knows FP than a Java/TypeScript/... developer who understands Java/TypeScript/... booby-traps_

IMO, a problem with industry priorities and how we learn to program. 

I did some math research, then 2 decades of mainstream programming with many battle scars, then an 8 year long Haskell job (some math came back only different), now I am going back to mainstream.  Is some convergence possible?  Can I find some middle ground approach to programming? 
 



## Mainsteam programming

_side_note_start
A story:  Uber's Go codebase was suffering from frequent crashes. Ignoring Turing Halting Problem / Rice's Theorem as too theoretical, Uber hired a bunch of PhDs to create NilAway, a linter solution to null reference issues. It is written in Go. Apparently it is known to crash with null reference panic from time to time &#128512; 
_side_note_end

We briefly considered Go.  Go trades safety for performance and claims to be junior programmer friendly failing to see this as a contradiction.
We rejected Go because of its safety track record in concurrent programming.

Frankly, I am scared. Consider Kotlin, touted as one of safer mainstream options, even `==` operator is still a booby-trap. 

```Kotlin
data class RegularId(val value: Int)
data class SpecialId(val value: Int)

list.find { it.id == foo.bar.lookupId }
```

`list` has items with `RegularId`s, `bar` class `lookupId` field is refactored from `RegularId` to `SpecialId`. This breaks this code at runtime not at compile time.  Not to mention that `a == b` is not the same as `b == a` for some `a`s and `b`s in standard Java library. 
We cannot stop using `==` and there is no workaround for it other than lots of diligence and testing. Any JVM PL that tries to have decent interop with Java faces such issues. Kotlin can flag some of such errors but not all.
I have not used Java for a long time. I do not remember all the Java gotchas. I am using LLMs to try 
to identify Kotlin booby-traps and catalogue them. After 4 hours of doing this, I wrote down 12 gotchas. 3 more the next day...
In my experience, developers (junior or senior) are completely unaware of these issues.

_If you see an object you should object_ 
- Phil Walder

There are no good popular choices, only compromises. We are trying Kotlin despite Java and OO safety issues and bloat it inherits.  


## Using LLMs

I am using LLMs to learn Kotlin and its quicks. My inquiries about concurrency often produce examples with obvious race conditions.
This makes me concerned about the general quality of Kotlin code that the LLM trained on (which probably is mostly the open source).
I expect this situation to get worse when vibe coding replicates these bugs
causing models to collapse on wrong solutions.
Maybe not surprisingly, if I ask for concurrency program examples using Kotlin _arrow_ library (FP library that tries to be Kotlin idiomatic and approachable) I get what look like bug free examples.

Also, I noticed that using Kotlin lingo when querying for Kotlin gotchas does not give me the best answers. 
A prompt like this "Are there any issues with inference widening in Kotlin" could give a much better answer. Asking for unsafe coercion may give different results than asking about casting. 
Downcasts or narrowing... etc. The lingo you use in the prompt probably changes what parts of the original training dataset impact the token generation. This would also suggest that Kotlin programmers do not know the issues I am trying to identify. 

Vibe coding is such a game change. I am completely new to Kotlin and can 
explore its capabilities in depth. I can prompt on a conceptual level and then dive deep into generated Kotlin code, exploring its limitations. 
I can prompt by writing Haskell (even Idris) code snippets, prompt with PLT terminology, and even use operational semantics.  Claude code even told me that one of my construction rules was not constructive, how great is that! 

At the same time, we are starting to observe a decrease in software quality and this trend now has a self feeding loop. Vibe coding could lead to quality collapse and model collapse at the same time. We may see reemergence of FP but this time with much stricter types &#128512;. (Hopefully the unemployment period will be short &#128512;)

People are vibe coding open source Rust replacements of whole application suites like Adobe or Microsoft office.  There is undeniable power there and finding effective and safe ways of using this tech can lead us into more FP. 
No one has a crystal ball. 


## What and how to copy from Haskell and FP?

_Using non-FP language makes FP coding harder rather than easier._

If we are concerned about a high barrier of entry the attempts on FP have to be very selective, approachable, easy to write, and language idiomatic.  Otherwise it is easier to stay with Haskell. 

The FP libraries which try to mimic Haskell in mainstream languages are hard to use. The biggest culprit is lack of support for higher kinded / second order type definitions. Typically one can do something like `List<T>` but not `F<T>`.  There is a technique referred to as _lightweight higher-kinded types_ or _defunctionalization of higher-kinded types_. 
The libraries based on this technique are hard to use and harder to learn than Haskell itself IMO.
This is being noticed by FP library maintainers and libraries like Kotlin's _arrow_ are moving away from this approach. 

There is an effort to create a more imperative and mainstream friendly version of FP (e.g. arrow in Kotlin - new versions, Effect in TypeScript, returns in Python, ...) and this is the direction I am trying to investigate.

Functional Programming is a buzzword that people like to equate to coding with immutable data.  I do not like this definition.  FP to me is reasoning about computations. Immunity is a necessary ingredient of that, not the final goal. Using libraries like _arrow_ is using FP-like APIs. This is not the same as employing the power of raw principled computations.
Some of these computations can fit into this shape: `Setter s t a b = (a -> b) -> (s -> t)`.
 
## `Setter` is interesting 

Consider the shape of `Functor` definition

```Haskell
 (a -> b) -> f a -> f b
```

Introduce type variables `s` and `t` and substitute `f a` with `s` and `f b` with `t`.  We get 

```Haskell
 (a -> b) -> s -> t
```
which is the definition of `Setter`.

```Haskell
type Setter s t a b = (a -> b) -> s -> t
type Setter' s a = Setter s s a a
```

`Traversal` is an interesting optic because unlike `Lens` or `Prism` it is not so easy to define it with a simple old style definion[^oldlens].  

[^oldlens]: E.g. old (data-lens)[https://hackage.haskell.org/package/data-lens-2.11.2/docs/Data-Lens-Common.html] package. Still visible as `lens` and `prism` combinators in the modern lens package.

```Haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

`Setter` immediately looks more mainstream friendly. 
We do not have much choice if we do not want the higher kinded types. 
`Traversal` needs to be _widened_ to `Setter`.  I justify this by thinking about `forall f. Applicative f` as a set off effects that is replaced with implicit IO. 

I look at this as losing _fidelity_ and _depth_ by _collapsing_ &#128296; FP concepts _windening_ them to computations that can be expressed in a non FP language.  

We can think about `Setter` as an optic (`Traversal` &#128296; or an ordinary `Setter`) and as a computation that can replace `Functor` &#128296;. We are not taking the `Traversable` `mapM` `traverse` point of view. If you show the Haskell definitions of `traverse` or `mapM` to a pragmatic mainstream programmer, would they see is as something relevant to their work?  In my experience the answer is no. `Either`, `Promise` like types become popular but come with isolated execution APIs and sometimes do not even expose `mapM`/ `traverse` equivalent.
To most programmers `map` and `traverse` would be the same, and this is what we are giving them here. People who think much more about it than I did (maintainers of Kotlin's arrow) have deprecated their [`Traversable`]().

`Functor` and polymorphic `Traversal` need to satisfy certain laws. These can be intuitively expressed as _structure preserving_, `s` and `t` need to have the same shape (e.g. a tree of ints and tree of strings with same shape of branches), all of this is intuitively explainable IMO.

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

The spoon (in Kotlin). I use `List` as the monoid to keep things simple (using arbitrary monoid is more cumbersome but is not a real limitation[^monoid]):

[^monoid]: My preference would be to pass monoid as a computation in another argument, in Haskell-like notation `type MonoidComputation m = (() -> m, m -> m -> m)`. It will become clear why later.

```Kotlin
typealias Setter<S, T, A, B> = (f: (A) -> B, s: S) -> T

// io version of foldMap based on just Setter with the Monoid fixed to List
// hf function is needed for polyoptic to collect A's in S. 
fun <S, T, A, B> ioSetterToList(hf: (A) -> B, setter: Setter<S, T, A, B>, s: S): List<A> {
    val visited = mutableListOf<A>()
    setter({ x ->
        visited.add(x)
        hf(x)
    }, s)
    return visited
}
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
    ioSetterToList(Int::toString, testMonoTypeSetter, MonoTest1(1, 2, listOf(3, 4)))

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
    ioSetterToList({ it }, testSetter<Int, Int>(), Test(1, 2, listOf(3, 4)))

// testfold2 == [1,2,3,4], fold with "Setter Test<Int> Test<String> <Int> <String>
val testfold2: List<Int> =
    ioSetterToList(Int::toString, testSetter<Int, String>(), Test(1, 2, listOf(3, 4)))

// used as setter optic
val testpolytraversal: Test<String> =
    testSetter<Int, String>()(Int::toString, Test(1, 2, listOf(3, 4)))
```

Note, no dreaded Kotlin contexts here!  No ad-hoc polymorphism so no context is needed. (More on this in the next section.)

Converting `Setter` to folds does not appear to be used in mainstream libraries but it could!  Interestingly this is a high level how TypeScripts' _Effects_ library does optics:

```TypeScript
// Conceptual simplification of Effect's optics: 
interface Optic<S, T, A, B> { 
    getOptic: (s: S) => Either<Error, Chunk<A>> 
    modifyOptic: (f: (a: A) => B) => (s: S) => T }
```

It uses the `Setter` trick and asks for extracting of `A`s to be implemented separately `getOptic` (I believe that _Effects_ does not use our `ioSetterToList` trick for `Traversal`). My point is we can work around any limitations of `Setter` computational power by asking for more stuff.
E.g, adding `s -> a` gives you `Lens s t a b`. 

TODO arrow  PTraversal note

Also, `Setter` is first class!  You may occasionally need to pass it rank-N style as `(forall. s t a b . Setter s t a b)` but even this is supported in many mainstream languages (e.g. TypeScript, I will show this in Kotlin  (Composing Setter)[#composing-setter]). Also composes just as regular functions do.

One could argue that `(a -> b) -> s -> t` is too abstract and hard to understand for some engineers.  But we are in an environment with huge support for encapsulation.  In the simplest case the user will just see `SimpleModifier<S,A>` with private implementation. 


## Problems with ad-hoc  

I am going to try to persuade you that passing explicit implementation (explicit dictionary passing) works better than attempts at ad-hoc polymorphism.  
  
To start, consider ability to polymorphically create (e.g. `Monoid` `memepty`) as a programming language benchmark

```Haskell
mempty                          --Haskell yes
[A: Monoid] .. Monoid[A].zero   --Scalaz  yes

ListMonoid.empty()               //Java   no 
monoidFactor.empty()

context (...) empty()            //Kotlin almost

```

Monomorphization could do it (C++ templates, Rust can, Go chooses not to).  Monomorphization is a complex topic beyond my current PLT interests. 

_Factory Pattern was invented because this is a problem._

On the high level, Haskell ad-hoc is linked to a type, OO ad-hoc is linked to a runtime object. OO and JVM uses late binding, dynamic method dispatch based on runtime objects to achieve ad-hoc polymorphism.  To have type specific implementations, there need to be runtime objects on the heap that point to vtable (virtual method table) to surrogate the types. Kotlin calls these _witness objects_.
In practice this requires explicit code specifying that surrogate/witness object (`context` in Kotlin). This ends up being _cumbersome_ (especially if you try this with FP). Terminology like "context bubbling" are being used.  It feels like writing compiler code, not application code. These "witnesses"/surrogates need to identify concrete types (e.g. `List<String>` not `List<T>`), and this gets very redundant.  

Scala implicits accomplish type specific ad-hoc through compile-time resolution and rewriting are removing the boilerplate, other languages do not. 
 
_If I cannot express `f a` then I can as well reason about `a`._ `Setter s t a b` and "big" and "small" type concept are a great benchmark for the next 1000 mainstream programming languages &#128512;


```Haskell
-- a classy version of Setter specialized to Functor
class FlatFunctor fa fb a b where 
   flatmap :: (a -> b) -> fa -> fb
```

for this to not be cumbersome we really want to (e.g.) specify `fa` and `b` and have the compiler figure out `a` and `fb`.  

```Haskell
class FlatFunctor fa fb a b | fa -> a, fa b -> fb where 
   flatmap :: (a -> b) -> fa -> fb
```

the target language may have no such type dependency capabilities (e.g. Kotlin does not) and we will need to keep providing even more info to the compiler &#128546; 

Here is how this would look like in Haskell.

```Haskell
class FlatFunctor ma mb a b | mb -> b, a mb -> ma where 
    flatmap :: (a -> b) -> ma -> mb

instance FlatFunctor (Either e a) (Either e b) a b where 
    flatmap = fmap

```

Moving forward I will use this expression to benchmark language ability to 
handle setters or maps polymorphically on small types:

```Haskell
-- Benchmark:
setter length . setter (map length) . setter (:[]) . setter show . setter (+2)  

-- e.g.: 
testcomp :: Either String Int -> Either String Int
testcomp =  flatmap length . flatmap (map length) . flatmap (:[]) . flatmap show  .  flatmap  (+2)
```

Here is Kotlin version:

```Kotlin
infix fun <A, B, C> ((B) -> C).compose(g: (A) -> B): (A) -> C = { a -> this(g(a)) }

// data Either e a = Left e | Right a
sealed interface Either<E, A> {
    data class Left<E, A>(val left: E) : Either<E, A> // Either<E, Nothing> is more idiomatic, Nothing is bottom Void like type.
    data class Right<E, A>(val right: A) : Either<E, A> // Either<Nothing, A> is more idiomatic
}

interface FlatFunctor<MA, MB, A, B> {
    fun flatmap(f: (A) -> B, ma: MA): MB
}

// not curried version of flatmap
context(flat: FlatFunctor<MA, MB, A, B>)
fun <MA, MB, A, B> flatmap(f: (A) -> B, ma: MA): MB = flat.flatmap(f, ma)

// THE CULPRIT, we will need to bring this one into scope using `context` 
fun <E, A, B> eitherFlatFunctor(): FlatFunctor<Either<E, A>, Either<E, B>, A, B> =
    object : FlatFunctor<Either<E, A>, Either<E, B>, A, B> {
        override fun flatmap(f: (A) -> B, ma: Either<E, A>): Either<E, B> = when (ma) {
            is Either.Left -> Either.Left(ma.left)
            is Either.Right -> Either.Right(f(ma.right))
        }
    }

// curried version of flatmap
context(flat: FlatFunctor<MA, MB, A, B>)
fun <MA, MB, A, B> flatmap(f: (A) -> B): (MA) -> MB = { ma -> flat.flatmap(f, ma) }

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

The same version of ad-hoc ("the same instance of FlatFunctor") is used 5 times but we have to specify it for each use!  Compare it to the previous code where we passed `Setter s t a b` directly and contexts were not need.

So why not treat `Functor` as `Setter s t a b` with functor laws obligations? 
_Context_ is not first class but `Setter s t a b` is, it even composes just as functions compose.  If I am forced to write compilation code (that is what contexts are), I can as well mimic Haskell dictionary passing, and pass the implementation directly and avoid all of the context madness.  
I can also use it with monomorphic types in a way `Functor` cannot be used.
`Setter s t a b` can be a learning tool! 


### Dangerous sophistication

When engineers see a problem they will look for 
ways to fix it. Look at this "fix" for my `FlatFunctor` 

```Kotlin
interface FlatFunctor<BASE> {
    fun <MA : BASE, MB : BASE, A, B> flatmap(f: (A) -> B, ma: MA): MB
}

context(flat: FlatFunctor<BASE>)
fun <MA : BASE, MB : BASE, A, B, BASE> flatmap(f: (A) -> B, ma: MA): MB = flat.flatmap(f, ma)

context(flat: FlatFunctor<BASE>)
fun <MA : BASE, MB : BASE, A, B, BASE> flatmap(f: (A) -> B): (MA) -> MB = { ma -> flat.flatmap(f, ma) }

//this causes unchecked cast compiler warning that can be suppressed with 
//@Suppress("UNCHECKED_CAST")
fun <E> eitherFlatFunctor(): FlatFunctor<Either<E, *>> = object : FlatFunctor<Either<E, *>> {
    override fun <MA : Either<E, *>, MB : Either<E, *>, A, B> flatmap(f: (A) -> B, ma: MA): MB = when (ma) {
        is Either.Left<*, *> -> Either.Left<E, B>(ma.left as E)
        is Either.Right<*, *> -> Either.Right<E, B>(f(ma.right as A))
    } as MB
}

// now we need only one context!!
// testcomp sanity benchmark FAILED!
fun testcompComposed(e: Either<String, Int>): Either<String, Int> =
    context(eitherFlatFunctor<String>()) {
        val pipeline: (Either<String, Int>) -> Either<String, Int> =
            flatmap<Either<String, List<Int>>, Either<String, Int>, _, _, _> { l: List<Int> -> l.size } compose
                flatmap<Either<String, List<String>>, Either<String, List<Int>>, _, _, _> { l: List<String> -> l.map { it.length } } compose
                flatmap<Either<String, String>, Either<String, List<String>>, _, _, _> { s: String -> listOf(s) } compose
                flatmap<Either<String, Int>, Either<String, String>, _, _, _> { x: Int -> x.toString() } compose
                flatmap<Either<String, Int>, Either<String, Int>, _, _, _> { x: Int -> x + 2 }
        pipeline(e)
    }
```

It uses rank-n capabilities combined with type bounds!  I was shocked that this is possible! 
Haskell would not allow me to write code like this. 
This is obviously fundamentally unsafe and a bad idea.  


### Composing `Setter`

Note we need higher rank (ability to pass polymorphic function as an argument) so it can be used with any `a -> b`s.

```Kotlin
// using Either defined previously we defined Setter for it
fun <E, A, B> eitherSetter(): Setter<Either<E, A>, Either<E, B>, A, B> = { f, s ->
    when (s) {
        is Either.Left -> Either.Left(s.left)
        is Either.Right -> Either.Right(f(s.right))
    }
}

// we need a wrapper to pass Setter polymorphically in "small" A B types
// this is the rank-N thing but only interface
interface PolyEitherSetter<E> {
    operator fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B>
}

// the rank-N things need to be implemented
fun <E> polyEitherSetter(): PolyEitherSetter<E> = object : PolyEitherSetter<E> {
    override fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B> = { s -> eitherSetter<E, A, B>()(f, s) }
}

// now we have a clean testcomp benchmark!
// 1 = testcomp(polyEitherSetter<String>())(Either.Right(4))
fun <E> testcomp(smap: PolyEitherSetter<E>): (Either<E, Int>) -> Either<E, Int> =
    smap { l: List<Int> -> l.size } compose
        smap { l: List<String> -> l.map { it.length } } compose
        smap { s: String -> listOf(s) } compose
        smap { x: Int -> x.toString() } compose
        smap { x: Int -> x + 2 }
```

We avoid ad-hoc, we avoid `context`. 

### Discharging context obligations with Setter 

Surprisingly Kotlin allows me to convert the ad-hoc `FlatFunctor` 
(that has context obligations) to a `Setter` that does not!

```Kotlin
context(_: FlatFunctor<MA, MB, A, B>)
fun <MA, MB, A, B> flatmapSetter(): Setter<MA, MB, A, B> = { f, ma -> flatmap(f, ma) }

fun <E, A, B> eitherSetter(): Setter<Either<E, A>, Either<E, B>, A, B> =
    context(eitherFlatFunctor<E, A, B>()) { flatmapSetter() }

// using the same PolyEitherSetter interface
// setter(f) is syntax sugar for setter.invoke(f)
fun <E> polyEitherSetter(): PolyEitherSetter<E> = object : PolyEitherSetter<E> {
    override fun <A, B> invoke(f: (A) -> B): (Either<E, A>) -> Either<E, B> =
        context(eitherFlatFunctor<E, A, B>()) { flatmap(f) }
}   

// testcomp benchmark looks clean!
// 1 = testcomp(polyEitherSetter<String>())(Either.Right(4))
fun <E> testcomp(smap: PolyEitherSetter<E>): (Either<E, Int>) -> Either<E, Int> =
    smap { l: List<Int> -> l.size } compose
        smap { l: List<String> -> l.map { it.length } } compose
        smap { s: String -> listOf(s) } compose
        smap { x: Int -> x.toString() } compose
        smap { x: Int -> x + 2 }
```

I am very surprised about this. I would still prefer to reduce type based ad-hoc and use explicit computations instead.

## Final thoughts

The industry's take on beginner friendliness is misguided. 
It can be compared to giving a sharp knife to a baby as 
the first toy.  Kotlin is neither easy to learn nor it is safe. It is a compromise limited by what is available. 

I like to distinguish easy (to learn) from simple (to understand) (see [_add_blank_target Easy vs Simple](2022-08-30-code-cognitiveload.html#easy-vs-simple)). Easy ~= knowledge, simple ~= understanding. Mainstream is trying to be easy at best, FP is trying to be simple. Simple is needed for safety, 
simple is needed for intellectual growth of programmers. Mainstream lacks
approachable and simple (Elm is not mainstream).

_Using non-FP language makes FP coding harder rather than easier._

I will miss the abstractions and concepts the most. Constructors, ...

Attempts to trivialize FP like I did treating `Traversal` and `Functor` as `Setter` IMO have a chance to bring some of the FP along.  





