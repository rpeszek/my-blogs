---
title: Type Enthusiast's Notes about TypeScript. Part 3. TS Complexity
author: Robert Peszek
featured: true
summary:  TypeScript Types series, types and values that do not match, type predicates, `any`,`unknown`, honest transparent types
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 3. Typing Honestly](2021-12-12-ts-types-part1.html).

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

This is the third post in the series devoted to types in TypeScript. In this series, I explore type-centric approaches to writing code and  push TS to its limits in doing so. I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.

In this post we will see TS struggle.  The section about complexity of TS types will expose compilation inconsistencies and surprising type checker behavior.  I will introduce additional tools for asking type questions to examine the complexities.   
My main goal in this note is to point out the complexity of what TS is trying to accomplish and share my understanding of it.  
As such this note will have a negative feel to it, similar to any gotchas discussion.  
I promise to compensate for this in the next installment.  It will be about techniques for programming with type variables.

Before we discuss complexity, let's briefly talk about cool aspects of TS's type safety.


## Interesting safety

TypeScript implements special [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) semantics when processing parts of JS code. These semantic rules provide very interesting and useful type safety features. 
TS can effectively narrow types used in a number of JS operators such as `typeof`, `===`, `==`
and apply this information to `if-else`, `switch` statements.  This post has already shown a few examples where this, almost magically, prevents placing code in a wrong branch of conditional if-else blocks.

Here are some of my favorites with IMO on their use.

### `apple !== orange` type safety

This JavaScript code:

```JavaScript
//Bad code
function blah(lhs: string, rhs: Person) {
  if (lhs === rhs) {
    //Do something
  } else {
    //Do somethign else
  }
}
```

is a programming bug and will not type-check in TypeScript.  You can just replace it with:

```JavaScript
//Actual equivalent
function blah(lhs: string, rhs: Person) {
  //Do somethign else
}
```

TypeScript prevents from using `===` if it can figure out, by looking at the types that `===` will always be `false`. This is true in general, not just inside `if-else`, but the `if-else` use is the killer app IMO.   
One cool example of `===` type safety combines type narrowing with literal types: `1 === 2` will not compile!

This is a big deal. `===` is typically used to compare things like `string` or `number` _id_-s or _hashes_ and it is not that uncommon to accidentally try to compare something like an _id_ to something completely different.   
I have seen analogous errors in many programming languages including even _Scala_. 

Last set of notes in this post is about complexity of TS types. The `===` type narrowing will be my main example in these notes.



### `switch` exhaustive check

There is something still lacking in the type safety department. 
`if-else` does not provide any mechanism for the type checker to verify that the program checked all possible conditions. E.g.

```JavaScript
const contrived = (n: 1 | 2): number => {
    if(n === 1) 
       return 1
    else if (n === 2)
      return 2
    else 
      return 100  // contrived will not compile if I comment the final else

}
```

Think of `1` and `2` as `Office.MessageCompose`-like "facets" (example borrowed from the previous post) currently offered by some API your code is using.  
You want to know what code needs to change when the API offers a `3`-rd facet.  Do you remember "Just one more thing" from the Lieutenant Columbo?   I may just be much older than you...

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

That is another nice example of TS enhancing JS with a nice type safety feature.

IMO an even better solution is a library (_ts-pattern_) solution, not a language solution.  
I am not going to steal the _ts-pattern_ thunder and refer to the library documentation and this blog post: 
[_add_blank_target https://dev.to/gvergnaud/bringing-pattern-matching-to-typescript-introducing-ts-pattern-v3-0-o1k](https://dev.to/gvergnaud/bringing-pattern-matching-to-typescript-introducing-ts-pattern-v3-0-o1k)



### `null` / `undefined` safety

`null` safety is another cool case of narrowing and we have seen it in action already.  TS defines separate types for `null` and `undefined`. There is a semantic difference between `null` and `undefined` but most code does not care.  My personal preference is to unify these two. 

In my very first example in the series, [_add_blank_target `getName(p: NullablePerson)`](2021-12-12-ts-types-part1.html#typescript-is-great), was not `undefined` safe, only `null` safe. 
Using it with `undefined` values typed as `any` will cause an error.  

My coding preference would be to rewrite my first example like this:

```JavaScript
//Reusable utility type
export type Undefined = null | undefined

//Convenience function
export const isUndefined = (d: unknown): d is Undefined =>
   (d === null) || (d === undefined) //probably equivalent to (d == null), I prefer not to use '=='


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

_Side Note:_ If _null_ is a billion dollar mistake, is `null` + `undefined` two billions? 
Unifying these 2 disasters into one could be a net saving of lots of dollars! 

TypeScript also provides `?` syntax for declaring object properties. E.g.

```JavaScript
type Person2 = {firstNm: string, middleNm?: string, lastNm: string}
```

The type of `person.middleNm` is `string | undefined`.  My preference is to declare optional properties using the above `Undefined` type:

```JavaScript
middleNm? : string | Undefined
```



## Complexity of TS Types

Throughout the series, we encountered a few examples where TS type checker did not work as expected, we will encounter more of TS quirkiness is this note.  This note suggests a reason for this: type complexity. 

I had set out to write this note planning to explore TS in a broader scope and show a set of very simple code examples explaining what TS does and why is that hard.  TS failed to work with me on about half of my examples creating chaos in my presentation. 
For example, an attempt to understand the arithmetic operators: `+`, `*`, `/` using [type holes]() resulted
in a complete failure.  TS did not allow me to use `_()` in any of these
exploratory expressions:  

```Java
//Compiliation errors: Object is of type 'unknown'
_() + _()
_() * _()
_() / _()
```

I decided to narrow the focus of this note.  I will focus only on semantic narrowing rules around the `===` operator.  
As we will see, this one alone will expose a lot of the underlying complexity.   

In this note, the narrative will try to ran away from me.  Talking about compiler complexity and uncovering compilation issues will get tangled together.  TypeScript comes with:

* type support for OO features
* structural subtyping 
* additional semantic rules for a selected set of JS operators 

All 3 are complex and intertwined. This note will try to focus on the last two bullets and how they relate to the `===` operator.  
I will try to demonstrate the complexity behind subtyping and the complexity of supporting ad-hoc semantics for the operators.
I would have been much simpler if `===` was just a function with two parameters, returning a boolean.  In TS, it is not. 

### `===` semantics, rejected overlap

I have picked `===` because we discussed it already in my note about the [`unknown` type](). 
Selecting `==` would produce a very similar presentation.   

Here is an example of safety around the `===` operator:

```Java
//This condition will always return 'false' since the types '"world!"' and '"dolly!"' have no overlap.ts(2367)
"world!" === "dolly!" //does not compile
```

Let's try to figure out the semantic rules around `===`.  What does "not having an overlap" mean?  
I have not seen a formal (or even somewhat precise) definition of the semantic rules for the `===`.  
(Please comment in git discussion if you know about any place that defines these.)   

The informal definition (from typescriptlang documentation) points to a "common type that both `x` and `y` could take on" but this statement clearly has some loose ends.   

The first part of the error message "This condition will always return 'false'" suggests a way to start:

**(EQ-SAFETY attempt 1):**  _TypeScript prevents from using `===` if it can prove, by looking at the types, that the result of `===` would always be `false`._

This is very high level and does not explain how TS does it. But is this even true?  

```Java
function testEqSemantics(a: {bye: string}, b: {hello: string): boolean {
   //This condition will always return 'false' since the types '{ bye: string; }' and '{ hello: string; }' have no overlap.
   return a === b //does not compile
}
```

Let me temporarily comment the not compiling code:

```JavaScript
function tstEqSemantics(a: {bye: string}, b: {hello: string): boolean {
   //This condition will always return 'false' since the types '{ bye: string; }' and '{ hello: string; }' have no overlap.
   //return a === b
   return true
}

const helloBy = {bye:"world!", hello:"world!"}
helloBy === helloBy
tstEqSemantics(helloBy, helloBy)  //compiles, here is the overlap!
```

TS has effectively prevented me from using `===` even though there are legitimate cases where the `===` 
would have returned `true`!  This seems like a major blooper.   

**_We have falsified the error message from TS._**

OO is complex and OO related type design issues are not uncommon among OO languages, this could be one of them.   
On the other hand, preventing `{bye: "world!"} === {hello: "world!"}` from compiling seems useful from a pragmatic point of view.   

I see 2 possible conclusions

1. This is a bug caused by a complexity of semantic rules
2. This is a feature indicating the complexity of semantic rules


### `===` semantics, what's an overlap?

Let's focus on this part of the error message "types ... and ... have no overlap".

**(EQ-SAFETY attempt 2):**  _`x === y` compiles if `x: X` and `y: Y` and the compiler successfully computes some special non-`never` `Overlap` type that widens to both `X` and `Y`_

`X` is the computed type for `x`, `Y` is the computed type for `y`,  how do we compute `Overlap` type for both?  
I think we can assume that widens simply means `extends` (more about `extends` in the future subsection). 

The 64,000 dollar question is how is the `Overlap` computed?  
It is clearly not the same as intersection (the type operator `&`).  We have falsified that hypothesis in the 
previous section.  

Let's try to look at some patterns:

```JavaScript 
const helloDolly: {hello: string} = {hello: "dolly!"}
const datedHello: {hello: string, since: number} = {hello: "world!", since:2022}
const one = 1 //const one: 1
const two = 2 //const two: 2
const onenum: number  = 1
const twonum: number  = 2
const world: string = "world"
```
```Java
//fails, different literal types do not overlap
"dolly!" ===  "world!"
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

//compiles, note 'Person' extends both 'number | Person' and 'string | Person'
function tst (x: number | Person, y: string | Person) {
    return x === y
}

//compiles, the overlap seems to be the extended type `{hello: string, since: number}` 
function testEqSemantics2(a: {hello: string} | 1, b: "boo" | {hello: string, since: number}): boolean {
    return a === b
}
```

A possible rule for calculating `Overlap` could be:

* for intersection types `X` and `Y`, if `X extends Y` take `X` or `Y extends X` take `Y` otherwise use `never`
* for union types `X = X1 | X2 | ...` and `Y = Y1 | Y2 | ...` recursively check if any `Xi` and `Yj` overlaps (this would be slow, quadratic cost!) 
* for complex combinations of union and intersection types? I DUNNO, I have not tested it enough.

The second bullet looks like a simplified, shallow intersection (`X & Y`).   
As the previous sub-section shows, the semantics of `===` and `&` are only partially consistent.  
As we will see shortly, computing `X & Y` is complex on its own.  
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

This particular quirkiness is actually useful, it allows for a program to do conservative null checks even if the type indicates that is not needed. I was happy to use this quirkiness in the above [todo]() section.    

If you are still reading this and your head is not spinning yet, try to recall the accidental widening issues 
we have examined in [``]() and how they impact `===`.  

I hope you agree.  This is complicated.  
I will hopefully bring this point even closer to home by the end of this post.


### Hidden blooper (side note)

If you remove type annotations from the above definitions, the `helloDolly === datedHello` still compiles:

```JavaScript
const helloDolly = {hello: "dolly!"}
const datedHello = {hello: "world!", since:2022}

helloDolly === datedHello //still compiles
```

From a pragmatic standpoint this is very strange. 
`"dolly!" ===  "world!"` is statically rejected, but `{hello: "dolly!"} === {hello: "world!", since:"2022"}` is not.

This surprising situation is caused by the type inference widening the types. The types inferred in the expression `"world!" === "dolly!"` are the literal types `"world!": "world!"` and `"dolly!": "dolly!"`, while the `helloDolly` and
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
eq({bye: "world"}, {hello: "world"})  //NOTE we lost the (possibly erronious) type safety preventing {bye: "world"} === {hello: "world"}
```

These are all different types but TS can unify them into a supertype (next section will discuss it). 
These are all legitimate statements.  Unfortunately, the type safety has been lost.  This explains why the semantic narrowing 
around the `===` operator is needed.  It is needed because structual subtyping can unify types even if types are very different.

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
The quirkiness seems to be related to type inference working inconsistently and failing 
to widen the types if a string literal type is involved (next section discussed it).  

The narrative ran away from me again, but the point should be somewhat clear:  Generics provide only limited type safety in TS.  
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
//finally compiles
eq<(1 | "boo")>(booone, oneboo)
```

In our discussion of `===` semantics I have stated that the intersection semantics (around the `&` operator) is most likely very complex.
I have also stated that the semantics around `===` is partially consistent with `&`.    
The example above allows `x === y` for `x: number | Person` and `y: string | Person`.

We can check the consistency with the `&` intersection by doing this:

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

Complexity is a super food for bugs.

We will discuss subtyping in more detail in the next installment.  

Hopefully you were able to find a linear path in this note that leads to this conclusion:
subtyping weakens the type safety. 
TS tries to recover the safety by building complex narrowing semantics around a selected set of JS operators.  


### Another narrowing issue?

Armed with knowledge of literal strings causing subtyping quirkiness this is simply a good guess:

```Java
//suffix numbers follow examples in the linked repo
function testEqSemantics5(a: {hello: 1 | "boo"} , b: "boo" | Person): boolean {
    //This condition will always return 'false' since the types '{ hello: 1 | "boo"; }' and 'string | Person' have no overlap.ts(2367)
    return a === b
}
```

I believe this is caused by a bug in handling literal strings, it is not a narrowing `===` implementation bug per se.



### Comparative Complexity

I want to contrast the above `===` examples against a programming language that has been designed around types from the beginning. An example could be Elm, PureScript, or Haskell (I am not that familiar with ReasonML or OCaml).    
These language have much simpler types.  The safety around equality does not require any special narrowing semantics.  You get it for free in any DIY function that has 2 arguments sharing the same generic type (only they call it polymorphic not generic).   

One underlying reason for this is the lack of complex subtyping and OO features. `eq(x,y)` will not compile if `x` and `y` have different types. There is no way to unify `x` and `y` to some supertype because there are no subtypes or supertypes.   
But, you will say JS object polymorphism is very useful.  Yes it is.  All the 3 languages listed above provide some support for using polymorphic record types, only they use much simpler techniques than subtyping.  (To be perfectly honest, Haskell is still improving on it.)
These languages also come with well thought out semantic rules that are often formalized and come with soundness proofs. 
The types in such languages are much simpler (not necessarily easier but simpler).  
Fewer surprises is a corollary of simple.  

We have seen several type inference over-widening issues that ended up causing compilation errors later on or a lack of safety.   
For a comparison, I could write a whole (fully type checked) application in Haskell without specifying a single type (Haskell without language extensions has full type inference).  Not that I would really want to do that, I like defining types. It is about the type checker's ability to help me more and needing less of my help.

To us, TS users, type complexity translates to a sometimes confused type checker requiring developer oversight and a possibility of escaped bugs.   

The complexity also translates to simply what it is: complexity. _Programming in a language in which I do not fully understand the types equates to me writing programs I do not fully understand._

It is worth noting that TypeScript has millions of users. FP languages have simpler, more reliable types but have tens of thousands of users (if combined).  TypeScript has more resources to improve.  
TS can achieve correctness by lots of sweat. FP languages largely achieve correctness by design.  What makes for a fewer bugs, lots of dollars or a clean type design?   
I do not think there is a clear answer to this question.  However, resources can't solve all the problems. 
Programming languages are almost paranoid about backward compatibility. Backward compatibility 
does not like changing things, even if the change is fixing bugs.  So I am afraid, a language like Elm will always be cleaner
and more robust.

Forgetting about the popularity context, I view it as a trade-off:  suffer because of the type complexity and reduced type safety but see your code when debugging JavaScript and trivially integrate with the rest of JS ecosystem _vs_ introduce a language that has nicer types, greater type safety, 
predictable compiler, but lose JS debugging and suffer when integrating JS libraries.  This trade-off is IMO not trivial and very project dependent.  Clean types vs debugger, I typically select the clean types.  
Ecosystem compatibility issue is a little harder to ignore and the main reason I am writing code in TS.


### Summary

This was a very hard note to write.  I rewrote it several times.  How do I write about complexity and make it simple to read?  
Seems like a catch-22 problem.  

> "One does not simply explain TS types"

_Boromir about TypeScript_

Again, my main claims are:

* subtyping adds significant complexity and lowers type safety
* ad-hoc semantic narrowing around JS operators partially recovers safety, but is complex by itself and scope limited

Languages with simpler and stronger type systems are not a superset of JS syntax.  

We have observed some new compilation issues and irregularities.  To summarize these:

* subtyping with literal strings appears to cause unsafe widening issues
* unexpected widening of literal object property types
* `===` rejects the `&` overlap of intersection types

Introduced tools 

```JavaScript
declare function unify<T>(t1: T, t2: T) : T
function verifyExtends<T2 extends T1, T1>() {}
```

allow to ask subtyping questions. 



## Next Chapter

