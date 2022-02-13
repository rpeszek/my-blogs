---
title: Type Enthusiast's Notes about TypeScript. Part 5. Advanced Types
author: Robert Peszek
featured: true
summary:  TypeScript Types series, type level programming, recursive types, a bit of category theory
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 4. Programming with Type Variables](2022-01-09-ts-types-part4.html).

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
When this note disappears, you will know that I gave up.)_   

**Disclaimers:** (imagine this is a very small font, read it very fast in a half whisper)   
_I assume strict compiler flags are on, something you get by default with scaffolding, e.g. using
`create-react-app my-project --template typescript` is close enough.  
The code examples have been tested with TypeScript v4.5.2.   
This post is a pandoc output of a markdown document and code examples are not interactive.  
Most of the code examples are published in [_add_blank_target ts-notes](https://github.com/rpeszek/ts-experiments/tree/master/ts-notes) folder in this github repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)._

**Motivating Quote for the series:**  

> "TypeScript began its life as an attempt to bring traditional object-oriented types to JavaScript so that the programmers at Microsoft could bring traditional object-oriented programs to the web. As it has developed, TypeScript’s type system has evolved to model code written by native JavaScripters. The resulting system is _powerful, interesting and messy._"

_From typescriptlang [_add_blank_target TypeScript for Functional Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html)_



## Nutshell

This is the fifth post in the series devoted to types in TypeScript. In this series, I explore type-centric approaches to writing code and push TS to its limits in doing so. I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.

In the last post I referenced the [_add_blank_target Types and Programming Languages](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) book. 
Similarly to the previous post, this installment will be a little more advanced and a little TAPL-ish. 
I will also introduce a tiny bit of Category Theory.  A great blog series (really a book) about Categories is [_add_blank_target Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/), here it is [_add_blank_target on goodreads](https://www.goodreads.com/en/book/show/33618151-category-theory-for-programmers).



## Recursive types 

`type JsonVal` from [_add_blank_target Part 1](2021-12-12-ts-types-part1.html#typescript-is-great) surprised me. It is recursive, the name `JsonVal` appears on both the LHS and the RHS of the definition.  Here is this definition repeated:

```JavaScript
type JsonVal = 
| {type: "object", val: Map<string, JsonVal>}
| {type: "array", val: JsonVal[]}
| {type: "string", val: string}
| {type: "number", val: number}
| {type: "bool", val: boolean}
| {type: "null"}
```

and there are TAPLish reasons why this is interesting:    

_side_note_start
The two established approaches for implementing recursive types in a programming language are 

* _iso-recursion_ (good fit for nominal types[^nominal]). If you know _recursion schemes_, the compilation technique is very similar to how 
the `Fix` type and the _recursion schemes_ work in nominally typed languages like _Scala_, _Haskell_, etc.  You kinda roll (Fix) or unroll (unFix) one layer of recursion at the time.
* _equi-recursion_ (good fit for structural types).  There is no `Fix/unFix` game. The structure is already unraveled into a potentially infinite 
beast. The compiler needs to deal with the whole beast. This approach is much harder to implement.

`JsonVal` looks like an equi-recursive definition. 
The methodology behind equi-recursion involves monotone functions and other things I never found time to understand very well.  Hard stuff and quite a bit of math.
I have not dug deep enough to know how TS compiles `JsonVal` like types.  No matter what it does, it is IMO impressive.  
_side_note_end

[^nominal]: As we have discussed in [_add_blank_target Part 3](2022-01-09-ts-types-part4.html#phantom-types), TS types 
are structural.  That means the name `Person` in `type Person = {firstNm: String, lastNm: String}` is only an alias, what defines the type is the RHS of the definition, not the LHS. Contrast this with an OO class definition in a language like Java. Two structurally identical classes are still considered different types (this is called _nominal_ typing).

`JsonVal`-like types appear to be hard on the TS type checker.  I have played with some advanced recursive types and have experienced it first hand. I got quite a few 

> 'Type instantiation is excessively deep and possibly infinite' 

compiler errors (e.g. code in [_add_blank_target https://github.com/rpeszek/ts-typecheck-peano](https://github.com/rpeszek/ts-typecheck-peano)).  However, I did not succeed in creating a simple example to demonstrate this.

I will demonstrate something slightly different:

```JavaScript
type List<T> = 
| {type: "nil"} 
| {type: "cons", head: T, tail: List<T>}

//Two seemingly identical definitions of a 1,2,3 list, both lines compile:
const ul_123  = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

const l_123: List<number> = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

```
```Java
//Only, I cannot define l_123 like this:
const l_123: List<number> = ul_123 //compiler error: Argument of type ... is not assignable to type ...

```

That is a recursive definition of a functional _cons_ list. 
TS will not let me assign a correctly structurally typed `ul_123` to the list type!  Similarly, I would not be able to use ` ul_123` as a parameter to a function that expects `(list: List<number>)`.  
This is quite different from how other types behave in TS.

IMO, it is still impressive that TS is able to pull these off.  The damage is that the developer will sometimes need to 
help the type checker.  I consider this feature very useful and underutilized by the ecosystem.  Here is some more advanced
use of recursive types:

_side_note_start
The github repo with code examples for this series contains [_add_blank_target RecSchemes.ts](https://github.com/rpeszek/ts-experiments/blob/master/ts-notes/src/RecSchemes.ts). This module contains code that allows for _folding_ (TS/JS ecosystem tends to use the term _reduce_) and _unfolding_  of arbitrary JSON values expressed as `JsonVal`. 

This approach is called _Recursion Schemes_. 
If you are not familiar with this concept, you are likely to have two reactions: "the code looks surprisingly terse"
and "WTF is going on".  IMO any code that solicits these 2 reactions is worth exploring. 
The first suggests a principled code, the second suggests an opportunity to internalize some fundamental principles. 
One high level intuition about recursion schemes is that they abstract out/hide recursion.   
Readers not familiar with this concept should try implementing an analogous `fold` for the above `List` type and 
compare the resulting type with TS's array `reduce`.  Recursion schemes are not easy, at least they
were not easy for me to learn.  
Since an identical technique can be applied to many other recursive types, recursion schemes could be used as a pattern
in TS.

Recursion Schemes are firmly rooted in theory. For example, the `fold` and `unfold` definitions in my example 
follow from categorical concepts explained [_add_blank_target here](https://bartoszmilewski.com/2017/02/28/f-algebras/)[^ct].   
This technique is also very useful. 
Examples are: manipulating XML documents, rewriting AST (syntax trees) of interpreted programs. 
A lot of code at my work is using recursion schemes (we are not doing it in TS though). 
In the TS/JS world, you could think about presenting very nested data by folding it into a nested React component. 
Working with any recursive type is likely to benefit from using recursion schemes.  

Even though TS is not capable of implementing recursion schemes the way they are done in Haskell or Scala, there is some
simplifying benefit of TS's structural typing.  The linked TS code explains this in code comments. 
It is really nice that code like this is possible.
_side_note_end

[^ct]:  The code examples in the linked CTFP chapter require a `Fix` type that allows for rolling (applying `Fix`) and
unrolling (deconstructing `Fix`), this complexity is due to nominal typing and iso-recursion,  TS makes things actually simpler. 

## Type level programming

TS literal types are singletons (i.e. type `"boo"` has exactly one value `"boo":"boo"`). 
This allows singletons to magically connect types with values. That way, the values can be type checked! 
Literal types should not be that hard to implement in a programming language and it is interesting why they are so uncommon.  Kudos to TS for introducing these!  They are, clearly, a great fit for JS.   
However, TS literal types are very limited in scope (I remember reading somewhere that it was a design decision).
For example, you can do some very basic type level string manipulation but you cannot
concatenate strings or do any arithmetic on numbers and you have no way of defining any additional features on your own (e.g. DIY number addition).  

TypeScript allows for type-level ternary (_Conditional Types_) as well as various type-level built-in functions (e.g. `keyof`).   
Apparently, the type level programming in TypeScript is _Turing Complete_
(see [_add_blank_target https://github.com/microsoft/TypeScript/issues/14833](https://github.com/microsoft/TypeScript/issues/14833)).    
However, type level programming in TS is focused on creating type safety for various
JS code idioms rather than creating a foundation for DIY type level programming. 
IMO this makes it harder to learn.  The _Turing completeness_ appears to be a completely accidental language feature.   

Type level programming can be very useful, we have seen some of it in action in the [_add_blank_target previous post](2022-01-09-ts-types-part4.html) where
we have used it to prevent subtyping and to prevent type checker from using the `unknown` type. 

IMO the best language design direction is for the type level 
and the value level code to look the same (e.g. dependently typed language like Idris).
The second best approach is for type level and value level to be very similar (e.g. Haskell).  
TS cannot and should not do either.  We do not want JavaScript (or very similar) on the type level!

At the same time, the lack of synergy between type level and value level programs makes things 
very complicated. E.g.:

```JavaScript
//example type from https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
type Flatten<Type> = Type extends Array<infer Item> ? Item : Type; 

const head = <T> (t: T[]) : Flatten<T[]> => {
    return t[0] //compiles
}
```
```Java
const generalizedHead = <T> (t: T) : Flatten<T> => {
    if(Array.isArray(t)) 
        return t[0]  //still compiles!
    else 
        return t //compiler error: Type 'T' is not assignable to type 'Flatten<T>'
}
```

here is another one:

```JavaScript
type HasContent<C> = {content: C}

type GetContent<T> = T extends HasContent <infer C> ? C : T
```
```Java
const getContent = <C, T extends HasContent<C>> (t: T): GetContent<T> => {
   //return t.content //compiler error:  Type 'C' is not assignable to type 'GetContent<T>'
}
```

It feels clunky.  It feels like type level and value level have a broken marriage. 
This lack of synergy also feels very confusing.

I think TS type level programming will keep improving and we may see some very interesting use cases in the future.



## Subtyping

This series has discussed subtyping already. I will keep this section comparatively short.

Personally, I try to avoid using subtyping features. 
Subtyping is related to Object Orientation.  OO programming has an appeal of simplicity and I was seduced by it for
many years.  It took me a long time to realize that OO is not that simple. 
Today, I think about OO as very complex. Even language designers often get it wrong (this series has provided a lot of evidence for this statement in the context of TypeScript). 
This comment should be filed under IMO as many developers disagree.  

Before continuing reading pass this code, please try to implement (at least in your head) the `amIFooOrBar` function:

```JavaScript
function verifyExtends<T2 extends T1, T1>() {}

//more specific, fewer variants
type FooOrBar =  
| {foo: string} 
| {bar: string}

//A challenge: implement this function:
declare function amIFooOrBar(o: FooOrBar): "foo" | "bar"

declare function genFooOrBar(): FooOrBar

//more general, more variants
type FooOrBarOrBuz =
| {foo: string} 
| {bar: string}
| {baz: string}

declare function genFooOrBarOrBuz(): FooOrBarOrBuz

const fooOrBarOrBuz: FooOrBarOrBuz = genFooOrBar() //compiles assigns specific to more general 
//const fooOrBar: FooOrBar = genFooOrBarOrBuz() //will not compile tries to assign general to more specific

verifyExtends<FooOrBar, FooOrBarOrBuz>() //compiles, FooOrBar extends FooOrBarOrBuz
//verifyExtends<FooOrBarOrBuz, FooOrBar>() //does not compile, FooOrBarOrBuz does not extend FooOrBar
```

The thing to remember is that `{foo: string} | {bar: string}` extends 
`{foo: string} | {bar: string}| {baz: string}` not the other way around.

Did you implement `amIFooOrBar`, great, let's move on.   

Subtyping in object types will feel familiar to OO developers.
Roughly speaking, you can assign object with more properties
to object with fewer properties:

```JavaScript
type FooAndBar = {foo: string, bar: string} //more general
declare function genFooAndBar(): FooAndBar

type FooAndBarAndBaz = {foo: string, bar: string, baz: string} //more specific
declare function genFooAndBarAndBaz(): FooAndBarAndBaz

const fooAndBar: FooAndBar = genFooAndBarAndBaz()  //specific assigned to general is valid assignment
//const fooAndBarAndBuz: FooAndBarAndBaz = genFooAndBar() // will not compile, tries to assign general to specific

verifyExtends<FooAndBarAndBaz, FooAndBar>() //compiles, FooAndBarAndBaz extends FooAndBar
//verifyExtends<FooAndBar, FooAndBarAndBaz>() //does not compile, FooAndBar does not extend FooAndBarAndBaz
```

Subtyping gets more involved if you combine adding properties to objects and variants to union types.
But I think this covers the basic idea.

Now let's revisit the above challenge.  What will your function return in this call:

```JavaScript
// challenge check:
// what does your function return when used on this value?
// NOTE this does compile, you can assign FooAndBar to FooOrBar, since 'and' implies 'or'
const whatIsThat = amIFooOrBar({foo: "foo", bar: "bar"}) 
```

This is just one of the many gotchas associated with subtyping. 



## Thunks and callbacks, `never` and `unknown`. 

To finish this post I want to pick 4 concepts fundamental to TypeScript: variables, callbacks, `never` and `unknown` types and discuss how they relate in a somewhat more theoretical setting. 
I believe the relationship between these concepts is not commonly understood.  

We have seen `<T> () => T` before, we called it a [_add_blank_target type hole `_: <T>() => T`](2021-12-12-ts-types-part1.html#type-holes).  Now I am changing my mind and want to call it a generic thunk.  
We can think about it as a 'lazy' value.  
Instead of defining `const t: T` (which, incidentally, TS does not allow on the top level[^genvar]) 
I can define a function that, when called, will return me that `t`.  Basically thunks are variables you put the `()` after.  A referentially transparent (no side-effects) `<T> () => T` thunk is morally equivalent to a variable of type `T`.  

[^genvar]: You can use  `const t: T` only inside functions that declare `T` in its type.

A thunk produces a value of type `T`.  A generic callback `<T> (_: T) => void` consumes a value of type `T`. 
There is, clearly, some type of duality between thunks and callbacks.  
Incidentally, many programming languages define a _unit_ type
often denoted as `()` instead of the _C_-style `void`.  If this was the case for TS, we would have written:
`<T> T => ()` for the callback and `<T> () => T` for the thunk.  You can get from one type to the other by 
reversing the arrow `=>`.  These concepts become dual in the categorical sense.  This post is not about Category Theory but this section has just a tiny bit of it.

In TS, the generic thunk `<T> () => T` type is equivalent to `never`.  
You may remember that the [_add_blank_target type hole `_()`](2021-12-12-ts-types-part1.html#type-holes), was implemented by throwing an error (that is `never` in TS).  
`never` assigns to everything but you cannot assign anything else to it.  Well, except for the generic thunk: 

```JavaScript
//thunk assigned to never
const nevr : never = _()

export const __neverFn: () => never =  _
```

In other words `<T> () => T` and `() => never` can be assigned to each other, thus, I consider them equivalent. 

If you replay the same argument with arrows reversed, you will establish equivalence between the generic callback
`<T> (_: T) => ()` and the `unknown` callback types. 

```JavaScript
declare function someUnknownCallback(t: unknown): void 
const overbar: <T>(_:T) => void =  someUnknownCallback

declare function someOverbar<T>(t:T): void
const unknownCallback: (_: unknown) => void = someOverbar
```

The `never` type is the TS's _bottom_ type (can be assigned to anything), while the [_add_blank_target `unknown`](2021-12-24-ts-types-part2.html#note-about-the-unknown-type) type is the TS's _top_ type (anything can be assigned to it). 
These concepts are also dual in the sense of reversing the direction of assignment. 

Let's think about referential transparency again.  There are no interesting referentially transparent functions
that return `void`.  To do something meaningful, such a function would need to mutate some shared state or do
some other effectful things.  E.g. when coding in React, a callback could compute a new state (let me call it `r: R`) and invoke a state hook to make the change.  I like to think about such a callback as having an imaginary type
`<T,R> (t: T) => R`.   

The duality between variables/thunks and callback is quite fascinating and has some depth.  
Let's fix the type variable `T` to, say, `Person`.  Any type would do, I just want to remove the quantification
(remove the genericity) to simplify my explanation.   
JS / TS programs often use higher order functions that accept callbacks as parameters.
Consider a callback that accepts a callback `(f: (_: Person) => void) => void` and computes the same value. 
The imaginary referentially transparent type for it could be 

```TypeScript
//TS-like pseudocode
<R> (f: (_: Person) => R) => R
```

As it turns out, this type is equivalent (isomorphic) to the thunk `() => Person` (or, ignoring side-effects,
to just `Person`)!  
They are not equivalent based on assignments, they are equivalent because one can be easily converted to the other.

It kinda makes sense for a dual of a dual to end up back where we started.  
However, this equivalence is a bit stronger, in a sense that it holds for every fixed type `T`. 
It is also weaker, since what we get is only isomorphism[^iso]. 

[^iso]: Think about isomorphism as being able to convert one type to the other without any information loss or gain

This equivalence is a special case of [_add_blank_target _Yoneda Lemma_](https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/) in Category Theory[^yoneda]. 

[^yoneda]: It is Yoneda applied to the _Identity functor_

I can express this succinctly in TS (note a [_add_blank_target higher rank type](2022-01-09-ts-types-part4.html#higher-rank-types) is used) as:

```JavaScript
//all of these compile
type Yoneda<T> = () => <R>(f: (_: T) => R) => R
type Thunk<T> = () => T

//Yoneda<T> is isomorphic to Thunk<T>
//here are functions defining the isomorphism:
const toYoneda = <T> (th: Thunk<T>): Yoneda<T> => {
   const res = () => <R> (f: (_: T) => R): R => f(th())
   return res
}

const fromYoneda = <T> (y: Yoneda<T>): Thunk<T> => {
    const res = (): T => y()(x => x)
    return res
 }
```

Programmers are divided into 2 camps when exploring this type of information:  some consider it fascinating and important, some consider it a lot of useless nonsense. 
If you are still reading this series, chances are you are in the first camp.   

_side_note_start  **Callback in JS:** 
I believe, JavaScripters intuitively know that equivalence and callbacks are viewed almost as a coding style. 
JS uses callbacks to accomplish all kinds of things. 
Except, for some reason, JS decided to endure _callback hell_ for about 2 decades. 
Today's `async` / `await` code finally brings an end to that mystery.   
Understanding that programming with callbacks (often called _Continuation Passing Style_[^cps]) and vanilla synchronous programming 
can offer very similar interface dates back to very early 1990-ties. 
This has to do with the programming abstraction that comes from Category Theory called _Monad_. 
_side_note_end

[^cps]: I believe the term Continuation Passing Style goes back as far as 1950ties.

Category Theory is very related to types and to programming in general,  I found it only fitting to finish this installment with a note that discussed a little bit of it.  
Bartosz Milewski's CTFP book linked above starts with code examples in C++ and in Haskell.
Bartosz gives up on C++ very fast.  I think it would be possible to stay on a little longer by selecting 
TS instead of C++.  Kudos to TS!  



## Next and the final Chapter

I will finish the series with some final thoughts and rants.  
The last 2 installments got a little on an advanced side of things. 
One question that I have been asking myself is:  When should more advanced types be
used in a TS project?  

The last installment will take me a month or so to finish. 













