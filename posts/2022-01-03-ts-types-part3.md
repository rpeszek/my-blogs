---
title: Type Enthusiast's Notes about TypeScript. Part 3. TS Complexity
author: Robert Peszek
featured: true
summary:  TypeScript Types series, complex and messy types in TS
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 2. Typing Honestly](2021-12-24-ts-types-part2.html).

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

Happy New Year!  Let's hope 2022 it will be way better than 2021.  It has to be.  

This is the third post in the series devoted to types in TypeScript. In this series, I explore type-centric approaches to writing code and push TS to its limits in doing so. I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.  

In this post we will see TS struggle.  We will see compilation inconsistencies and surprising type checker behavior.  
My main goal is to point out the complexity of what TS is trying to accomplish and share my understanding of it.  
On a positive note, I will introduce additional tools for asking TS type questions.   
Also, I promise, the next installment will be about good things in TS.  It will be about programming with type variables.

Before we discuss the messy bits, let's briefly talk about some cool type safety features.


## Interesting safety

TypeScript implements special [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) semantics when processing parts of JS code. These semantic rules provide very surprising and useful type safety features. 
TS can effectively narrow types used in a number of JS operators such as `typeof`, `===`, `==`
and apply this information to `if-else`, `switch` statements.  This post has already shown a few examples where this, almost magically, prevents placing code in a wrong branch of conditional if-else blocks.

Here are some of my favorites with IMO on their use.

### `apple !== orange` type safety

This JavaScript code (I keep reusing `type Person = {firstNm: string, lastNm: string}` from the first post):

```JavaScript
//Bad code
function blah(lhs: string, rhs: Person) {
  if (lhs === rhs) {
    //Do something
  } else {
    //Do something else
  }
}
```

is a programming bug and will not type-check in TypeScript.  You can just replace it with:

```JavaScript
//Actual equivalent
function blah(lhs: string, rhs: Person) {
  //Do something else
}
```

TypeScript prevents from using `===` if it can guess[^1], by looking at the types, that `===` will always be `false`. This is true in general, not just inside `if-else`, but the `if-else` use is the killer app IMO.   
One cool example of `===` type safety combines type narrowing with literal types: `1 === 2` will not compile!

[^1]: [rejected overlap](#semantics-rejected-overlap) section explains why I call it a guess.

This is a big deal. `===` is often used to compare things like `string` or `number` _id_-s or _hashes_ and it is not that uncommon to accidentally try to compare something like an _id_ with something completely different.   
I have seen analogous issues in many programming languages including even _Scala_. 


### `switch` exhaustive check

`if-else` does not provide any mechanism for the type checker to verify that the program checked all possible conditions.  
Interestingly, TS uses the `switch` statement to solve this problem:

```JavaScript
//This compiles!
const contrived_better = (n: 1 | 2): number => {
    switch(n) {
       case 1:
        return 1
       case 2:
        return 2 
    } 
}
```

```Java
//Compilation error
//Function lacks ending return statement and return type does not include 'undefined'.ts(
export const contrived_better_ = (n: 1 | 2 | 3): number => {
    switch(n) {
       case 1:
        return n
       case 2:
        return n 
    } 
}
```

That is another nice example of TS enhancing JS with a nice type safety feature.

IMO an even better solution is provided by the _ts-pattern_ library. 
See this blog post: 
[_add_blank_target Introducing ts-pattern v3.0](https://dev.to/gvergnaud/bringing-pattern-matching-to-typescript-introducing-ts-pattern-v3-0-o1k)



### `null` / `undefined` safety

We have seen `null` safety already.  There is a semantic difference between `null` and `undefined` but most code does not care.  My personal preference is to unify these two. 

In my very first example in the series, [_add_blank_target `getName(p: NullablePerson)`](2021-12-12-ts-types-part1.html#typescript-is-great), was not `undefined` safe, only `null` safe. 
Using it with `undefined` (e.g. typed as `any`) will cause an error.  

My coding preference would be to rewrite my first example like this:

```JavaScript
//Reusable utility type
export type Undefined = null | undefined

export const isUndefined = (d: unknown): d is Undefined =>
   (d === null) || (d === undefined) //I prefer not to use '=='

const getName2 = (p:Person | Undefined): string => {
    //const tst1 = p.firstNm //will not compile
    if(isUndefined(p)){
        //const tst2 = p.firstNm //will not compile
        return "John Smith"
    } else {
        return p.firstNm + " " + p.lastNm //compiles
    }
}
```

This is just my personal preference,  I also use this approach when typing optional `?` object properties. E.g.

```JavaScript
type Person2 = {firstNm: string; middleNm?: string | Undefined; lastNm: string}
```

The extra safety features are what surprised and excited me about TS. 
They reminded me of a functional programming language.  


## Complexity of TS types

Throughout the series, we encountered a few examples where the TS type checker did not work as expected, we will encounter more of TS quirkiness in this section.  This note suggests a reason for this: type complexity. 

My original plan was to write about TS needing to implement a separate ad-hoc semantics for various JS operators. 
I was not able to present anything very insightful and I have abandoned that idea, e.g. these [_add_blank_target type hole](2021-12-12-ts-types-part1.html#type-holes) expressions do not even compile:  

```Java
//Compiliation errors: Object is of type 'unknown'
_() + _()
_() * _()
_() / _()
```

Taking the quote from the top of this post to heart, I concluded that TS is about providing support for OO and other idiomatic uses of JS.  I decided to narrow the focus of this note to subtyping and the `===` operator semantics.


### `===` semantics, rejected overlap

I have picked `===` because we discussed it already in my previous note about the [_add_blank_target `unknown` type](2021-12-24-ts-types-part2.html#note-about-the-unknown-type). 
Selecting `==` would produce a very similar presentation.   

Here is an example of safety around the `===` operator:

```Java
//This condition will always return 'false' since the types '"world!"' and '"Dolly!"' have no overlap.ts(2367)
"world!" === "Dolly!" //does not compile
```

Let's try to figure out the semantic rules around `===`.  What does "not having an overlap" mean?  
I have not seen a formal (or even a somewhat precise) definition of the semantic rules for the `===`.  
(Please comment in git discussions if you know about any place that defines these.)   
The informal definition (from typescriptlang documentation) points to a "common type that both `x` and `y` could take on" but this statement clearly has some loose ends.   

The first part of the error message "This condition will always return 'false'" suggests a way to start:

**(EQ-SAFETY attempt 1):**  _TypeScript prevents using `===` if it can prove, by looking at the types, that the result of `===` would always be `false`._

This is a very high level and does not explain how TS does it. But is this even true?  

```Java
function testEqSemantics(a: {bye: string}, b: {hello: string): boolean {
   //This condition will always return 'false' since the types '{ bye: string; }' and '{ hello: string; }' have no overlap.
   return a === b //does not compile
}
```

Let me temporarily comment the not compiling code:

```JavaScript
function testEqSemantics(a: {bye: string}, b: {hello: string}): boolean {
   //This condition will always return 'false' since the types '{ bye: string; }' and '{ hello: string; }' have no overlap.
   //return a === b
   return true
}

const helloBye = {bye:"world!", hello:"world!"}
testEqSemantics(helloBye, helloBye)  //compiles, here is the overlap!
```

TS has effectively prevented me from using `===` even though there are legitimate cases where the `===` 
would have returned `true`!  This seems like a major blooper.   

**_We have falsified the error message from TS._**

OO is complex and type design issues are not uncommon among OO languages, this could be one of them.   
On the other hand, preventing `{bye: "world!"} === {hello: "world!"}` from compiling seems useful from a pragmatic point of view. 
It is possible that this behavior is intentional. 

I see 2 possible conclusions

1. This is a bug caused by a complexity of TS's semantic rules
2. This is a feature indicating that the rules are indeed complex


### `===` semantics, what's an overlap?

Let's focus on this part of the error message: "types ... and ... have no overlap".

**(EQ-SAFETY attempt 2):**  _`x === y` compiles if `x: X` and `y: Y` and the compiler successfully computes some special non-`never` `Overlap` type that widens to both `X` and `Y`_

`X` is the computed type for `x`, `Y` is the computed type for `y`,  how do we compute `Overlap` type for both? 
I think we can assume that _widens_ simply means `extends`.   
The 64K dollar question is how is the `Overlap` computed? 
It is clearly not the same as intersection (the type operator `&`),  we have falsified that hypothesis in the 
previous section. 
Let's try to look at some patterns:

```JavaScript 
const helloDolly: {hello: string} = {hello: "Dolly!"}
const datedHello: {hello: string, since: number} = {hello: "world!", since:2022}
const one = 1 //const one: 1
const two = 2 //const two: 2
const onenum: number  = 1
const twonum: number  = 2
const world: string = "world"
```
```Java
//fails, different literal types do not overlap
"Dolly!" ===  "world!"
//fails, different literal types do not overlap
one === two
//fails, string and number do not overlap
one === world
```
```JavaScript 
//compilies, note both have the same type
onenum === twonum
//compiles, note 'typeof datedHello' extends 'typeof helloDolly' 
helloDolly === datedHello

//compiles, the overlap seems to be the 'Person' type
function tst (x: number | Person, y: string | Person) {
    return x === y
}

//compiles, the overlap seems to be `{hello: string, since: number}` 
function testEqSemantics2(a: {hello: string} | 1, b: "boo" | {hello: string, since: number}): boolean {
    return a === b
}
```

A possible rule for calculating `Overlap` could be (this is just a rough, high level heuristics, _please comment if you know a better definition_):

* for intersection types `X` and `Y`, if `X extends Y` take `X` else if `Y extends X` take `Y` otherwise reject
* for union types `X = X1 | X2 | ...` and `Y = Y1 | Y2 | ...` recursively check if any `Xi` and `Yj` overlaps (this heuristics ignores performance cost) 
* for complex combinations of union and intersection types? I DUNNO, I have not tested it enough.
 
I have not played with this assumption for a very long time, but so far these rules seem to hold with these exceptions:

```JavaScript
//All compile
1 === null

1 === undefined

function tst2 (x: 1, y: null) {
    return x === y
}
```

Does `1` have an overlap with `null` and `undefined`? 
What does that even mean? 
With the _strictNullChecks_ compiler flag, `null` should be well separated from other types.   
This particular quirkiness is actually useful, it allows for a program to do conservative null checks even if the type indicates that it is not needed. 

I hope you agree.  This is complicated.  
I will hopefully bring this point even closer to home by the end of this post.


### Hidden blooper (side note)

If you remove type annotations from the above definitions, the `helloDolly === datedHello` still compiles:

```JavaScript
const helloDolly = {hello: "Dolly!"}
const datedHello = {hello: "world!", since:2022}

helloDolly === datedHello //still compiles
```

From a pragmatic standpoint this is very strange. 
`"Dolly!" ===  "world!"` is statically rejected, but `{hello: "Dolly!"} === {hello: "world!", since:"2022"}` is not.

This surprising situation is caused by the type inference widening the types. The types inferred in the expression `"world!" === "Dolly!"` are the literal types `"world!": "world!"` and `"Dolly!": "Dolly!"`, while the `helloDolly` and
`datedHello` infer a `string` and `number` for their properties:

```JavaScript
//IntelliSense view of helloDolly
const helloDolly: {
    hello: string;
}
//IntelliSense view of datedHello
const datedHello: {
    hello: string;
    since: number;
}
```

IMO, widening object property types is an arbitrary complexity. 

### DIY equality 

The question is how far can I get by trying to reproduce safety around the `===` on my own.  

```JavaScript
declare function eq<T>(t1: T, t2: T): boolean
```

This generic function (it could be implemented by simply using `===`) forces both arguments to have the same type. 
That should give me at least some level of extra safety and prevent from comparing apples and oranges.  
Let's see, starting with these type holes:  

```JavaScript
//type holes shows a string, not bad, I would prefer the literal "foo".
eq("foo", _())
//type holes shows unknown, Another unexpected 'uknown' widening issue? 
eq(_(), "foo")
```

Let's ignore the second type hole disappointing quirkiness and move on.  

```JavaScript
//These all compile
eq(1 as 1, null)
eq(1, 2)              //NOTE we lost the type safety of ===
eq(1 as 1, 2 as 2)    //NOTE we lost the type safety of ===
eq({bye: "world"}, {hello: "world"})  //NOTE we lost the (possibly erroneous) type safety preventing {bye: "world"} === {hello: "world"}
```

How come these compile? These are all different types but TS can unify them into a supertype (next section will discuss it). 
These are all legitimate statements.  Unfortunately, the type safety has been lost.  This explains why the semantic narrowing 
around the `===` operator is needed.  It is needed because structural subtyping can unify types even if types are very different.

However, quirkiness alert, these do not compile:

```Java
//Argument of type '"boo"' is not assignable to parameter of type '1'.ts(2345)
eq(1, "boo")
//Argument of type '1' is not assignable to parameter of type '"boo"'.ts(2345)
eq("boo", 1)
//Argument of type '{ hello: string; }' is not assignable to parameter of type '1'.ts(2345)
eq(1, {hello: "world"})
//Argument of type '{ hello: string; }' is not assignable to parameter of type '"boo"'.ts(2345)
eq("boo", {hello: "world"})
```

_This is very unfortunate_, you want generic functions to work _consistently_ across types. 
IMO this is a bug or an arbitrary complexity.   
The quirkiness seems to be related to the type inference working inconsistently and failing 
to widen the types if a string literal type is involved (next section discussed it).  

The narrative has run away from me, but the point should be somewhat clear:  Generics provide only limited type safety in TS.  
E.g. enhanced safety semantics around `===` does not transfer to a DIY safety 
that a library solution could expose. 


### Subtyping

How come this compiles?

```JavaScript          
eq(1 as 1, 2 as 2) 
```
 
The type checker widens the types of both arguments to `1 | 2`.  This is because of a subtyping rule
that says that `1 extends (1 | 2)` and `2 extends (1 | 2)`.  
Here is a somewhat clever trick to see that:

```JavaScript
export declare function unify<T>(t1: T, t2: T) : T

//hovering over unify shows me:
//(alias) unify<1 | 2>(t1: 1 | 2, t2: 1 | 2): 1 | 2
unify(1 as 1, 2 as 2)
```

if you do not believe me that `1 extends (1 | 2)` you can check it for yourself with another trick:

```JavaScript 
export function verifyExtends<T2 extends T1, T1>() {}

verifyExtends<1, 1 | 2>()
```

However, TS appears to be not consistently good about inferring these subtyping rules. TS apparently did not notice that
`1 extends (1 | "boo")` and `"boo" extends (1 | "boo")`.  Hence the blooper

```JavaScript
verifyExtends<1, 1 | "boo">()
verifyExtends<"boo", 1 | "boo">()
```
```Java
//Argument of type '"boo"' is not assignable to parameter of type '1'.ts(2345)
eq(1, "boo")
```

Let's try to force TS into compliance by type annotating everything:

```JavaScript
const booone : 1 | "boo" = "boo"
const oneboo : 1 | "boo" = 1
```
```Java
//Argument of type '1' is not assignable to parameter of type '"boo"'.ts(2345)
eq(booone, oneboo) 
```
```JavaScript
//finally compiles with type application on 'eq'
eq<(1 | "boo")>(booone, oneboo)
```

In our above examples we have seen that `===` narrowing is partially consistent with the intersection (`&` operator).  
Let's look at `&` semantics a little closer. 

We can try to double check how the `&` intersection works by doing this:

```JavaScript 
//both compile suggesting that Person is equivalent to the intersection  (number | Person) & (string | Person)
verifyExtends<Person, (number | Person) & (string | Person)>()
verifyExtends<(number | Person) & (string | Person), Person>()
```

However this does not compile, and it does look like a bug (see second line of the error message):

```Java
//Type '(1 | "boo") & ("boo" | Person)' does not satisfy the constraint '"boo"'.
//  Type '1 & Person' is not assignable to type '"boo"'.ts(2344)
verifyExtends<(1 | "boo") & ("boo" | Person), "boo">()
```

_Complexity is a super food for bugs._

Here is my quick summary: subtyping is complex and it weakens type safety. 
TS tries to recover the safety by building complex narrowing semantics around a selected set of JS operators. 
There are many inconsistencies in both the implementation of subtyping and the implementation of narrowing semantics.


### Comparative complexity rant

A "type enthusiast" will associate types with correctness, even formal verification.  To me, the words "messy" and "type" are self contradictory.  TS "types" support some interesting features but are a mess. 

I want to contrast the above `===` and `eq` examples against a programming language that has been designed around types from the beginning. An example could be an FP language like Elm, PureScript, or Haskell (I am not that familiar with ReasonML or OCaml)[^langs].    
These languages have much simpler types.  The safety around equality does not require any special narrowing semantics.  You get it for free in any DIY function that has 2 arguments sharing the same generic type (only they call it polymorphic not generic).   

[^langs]:  All can be used for frontend development and can be compiled to JS.  

One underlying reason for this is the lack of complex subtyping and OO features. `eq(x,y)` will not compile if `x` and `y` have different types. There is no way to unify `x` and `y` to some supertype because there are no subtypes or supertypes.   
But, you may say, JS object polymorphism is very useful.  All the 3 languages listed above provide support for polymorphic record types[^2], only they use much simpler techniques than subtyping to achieve it.   
These languages also come with well thought out semantic rules that are often formalized and come with soundness proofs.   
The types in these languages are much simpler (not necessarily easier but simpler). 


[^2]: Haskell is still improving on this aspect. IMO, the need for polymorphic access to 
record fields is overrated.  I would trade it for a capable compiler any time.

Type complexity translates to a confused type checker and to a confused developer.   
_Programming in a language in which I do not fully understand the types equates to me writing programs I do not fully understand._

It is worth noting that TypeScript has over a million users. FP languages have tens of thousands of users (if combined).  TypeScript has more resources to improve. 
What makes for fewer bugs, lots of dollars or clean types?   
I do not think there is a clear answer to this question.  However, resources can't solve all the problems. 
Programming languages are almost paranoid about backward compatibility and backward compatibility 
does not like changing things, even if the change is fixing bugs.   
So I am afraid, a simple language like Elm will always be cleaner and more robust.

Forgetting about the popularity context, I view it as a trade-off:  suffer because of the type complexity and reduced type safety but see a readable JavaScript and trivially integrate with the rest of JS ecosystem _vs_ introduce a language that has nicer types, greater type safety, 
predictable compiler, but lose generated JS code clarity and suffer when integrating JS libraries.   
This trade-off is IMO not trivial and very project dependent. 
Clean types vs clean JS, I typically select the clean types. 
The ecosystem compatibility issue is a little harder to ignore and the main reason I am writing code in TS. 
Projects with a high correctness requirement, IMO, should select an FP language, the optimal choice for other projects is less clear.    


### Variance problems

I will finish with some examples that may feel even more surprising.   

```JavaScript
const bye = {bye: "world"}
const hello = {hello: "world"}

declare function eqArrays<T>(t1: T[], t2: T[]): boolean

eqArrays([{bye: "world"}], [{hello: "world"}]) //compiles
```
```Java
//Compilation error
//Property 'bye' is missing in type '{ hello: string; }' but required in type '{ bye: string; }'.ts(2741)
eqArrays([bye], [hello])
```

Here is another example:

```JavaScript
interface Payload<T> {payload: T}

// ... we would see the same behavior for:
//type Payload1<T> = {payload: T} 

declare function eqPayloads<T>(t1: Payload<T>, t2: Payload<T>): boolean

eqPayloads({payload: {bye: "world"}}, {payload: {hello: "world"}})  //compilies
```
```Java
// Compilation error:
// Property 'bye' is missing in type '{ hello: string; }' but required in type '{ bye: string; }'.ts(2741)
eqPayloads({payload: bye}, {payload: hello})
```

My first instinct was to assume that this weird behavior is caused by TS treating `T[]` and `Payload<T>` conservatively as invariant. 
Unfortunately, this is not the case. The above quirkiness looks to be just another type inference issue and there is a deeper safety problem.

TS implements variance incorrectly and makes both `T[]` and `Payload<T>` covariant 
(e.g. TS assumes that `P extends T` implies `Payload<P> extends Payload<T>`).  Here is a well known Java language bug reimplemented in TS:

```JavaScript
//how to put a string into a list of numbers
const intlist: number[] = [1,2,3]
const list: unknown[] = intlist
list.push("not a number") //compiles

//array is incorrectly covariant
verifyExtends<typeof datedHello[], typeof helloDolly[]>() //datedHello extends helloDolly type
```

I see the same incorrect subtyping on the `Payload` interface:

```JavaScript
//interface Payload is incorrectly covariant
verifyExtends<Payload<typeof datedHello>, Payload<typeof helloDolly>>()
verifyExtends<Payload<typeof datedHello>, Payload<object>>()
```
 
Implementations of  `interface Payload<T>` do not need to behave in a covariant way.  
An example in the linked github repo exploits `interface Payload<T>` covariance and ends up passing a `number` to a function that accepts `string` input.  

Invariance would have been a better (a more conservative) choice for both `interface Payload<T>` and the array.  
A careful reader may notice that the structurally typed `type Payload1<T> = {payload: T}` should also be invariant
since the `payload` property is mutable (getters are covariant, setters are contravariant).  TS incorrectly makes it covariant.  

I will sound like a broken record now, subtyping is clearly very complex.

I did more digging into it after writing this note.  It appears that the intention was to keep TS conceptually easy
([_add_blank_target issue 1394](https://github.com/microsoft/TypeScript/issues/1394)).  
The result may be easy but is definitely not simple.

_Incorrect is never simple._
 
_side_note_start **Observation (Rant Alert)**: 
There is a tendency to focus on the common case and ignore the corner case. 
This tendency has a broad scope, broader than TS. 
What has (typically) a lower cost:  resolving a problem that every user observes when opening the app or resolving a problem that affects 1% of users once a month?  Are less frequently observed defects assigned a lower priority?  Not really.  
The common approach to software and language design and the economics of software maintenance are an ill matched couple. 
_side_note_end

### Summary

This was a very hard note to write.  I rewrote it several times.  How do I write about complexity and make it simple to read?  
Seems like a catch-22 problem.  

> "One does not simply explain TS types"

_Boromir about TypeScript_

Again, my main claims are:

* subtyping adds significant complexity and lowers type safety
* ad-hoc semantic narrowing around JS operators partially recovers safety, but is complex by itself and scope limited

Languages with simpler and more reliable type systems are not a superset of JS syntax and are idiomatically far from JS[^flow].   

[^flow]: I have not used _flow_ recently, and I cannot compare TS to it.  However _flow_ has subtyping which I do not consider simple. Indeed, some level of subtyping support is needed to support commonly used JS idioms.

We have observed some compilation issues and irregularities.  To summarize these:

* issues inferring literal types widened to a union ([`eq(1, "boo")`](#subtyping))
* issues preventing intersecting unions involving literal types ([`(1 | "boo") & ("boo" | Person)`](#subtyping))
* unexpected widening of literal object property types ([hidden blooper](#hidden-blooper-side-note))
* inconsistent widening of function arguments (top of [variance problems](#variance-problems))
* incorrect handling of variance ([variance problems](#variance-problems))
* `===` rejects the `&` overlap of intersection types, while claiming the opposite in the error message ([rejected overlap](#semantics-rejected-overlap))

Introduced tools 

```JavaScript
declare function unify<T>(t1: T, t2: T) : T
function verifyExtends<T2 extends T1, T1>() {}
```

can ask TS subtyping questions. 

## Next Chapter

This post has been about the "messy" in TS.  The next installment will focus on programming with 
type variables and will present TS in a better light. I decided to split advanced topics into 2 smaller posts. 
I plan to discuss phantom types, type variable scoping, a pattern emulating existential types, and rank 2 types. 
I consider these to be quite useful typing approaches.

Happy New Year to all of my readers.  Thank you for reading.
