---
title: Type Enthusiast's Notes about TypeScript. Part 1. Typing in Anger
author: Robert Peszek
featured: true
summary:  TypeScript Types series, Introduction, office.js, working with and fighting type checker
changelog: <ul> 
    <li> (2021.12.24) modified historical note about office.js. Linked Part 2. Planned future content adjustment.</li>
    <li> (2021.12.26) <a href="#fn1">footnote [1]</a> 
    <li> (2022.01.03) Linked Part 3.
     </ul>
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
When this note disappears, you will know that I gave up.)_   

**Disclaimers:** (imagine this is a very small font, read it very fast in a half whisper)   
_I assume strict compiler flags are on, something you get by default with scaffolding, e.g. using
`create-react-app my-project --template typescript` is close enough.  
The code examples have been tested with TypeScript v4.4.4 and v4.5.2.   
office.js examples are based on https://appsforoffice.microsoft.com/lib/1.1/hosted/office.js and @types/office-js@1.0.221 
(these match the current scaffold for office.js/React).  
This post is a pandoc output of a markdown document and code examples are not interactive.  
Most of the code examples are published in [_add_blank_target ts-notes](https://github.com/rpeszek/ts-experiments/tree/master/ts-notes) folder in this github repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)._

## Introduction to the series 

> "TypeScript began its life as an attempt to bring traditional object-oriented types to JavaScript so that the programmers at Microsoft could bring traditional object-oriented programs to the web. As it has developed, TypeScript’s type system has evolved to model code written by native JavaScripters. The resulting system is _powerful, interesting and messy._"

_From typescriptlang [_add_blank_target TypeScript for Functional Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html)_


I wanted to write a short post about my experience with TS types, I ended up with a draft the size of a short book. 
I decided to split it into digestible installments and publish it as a series of shorter posts. The series will be about the _powerful, interesting and messy_ types in TS.  This post is the first in that series. 

Here is my plan:

* Part 1 (this post). Is a warm-up. Part 1 has been motivated by a current project at my work that uses TS. 
  I will show code examples that are hard to compile.  I will discuss strategies and methods for resolving compilation issues.
  Part 1 will present code examples that compile but really, really should not, and code examples that should compile but surprisingly don’t.  I will also summarize my overall experience of working with TS.   
  This series needed a JS library with TS bindings to draw examples from, I decided to use _office.js_ and Part 1 will introduce it.
* [_add_blank_target Part 2](2021-12-24-ts-types-part2.html). Will be about keeping types honest. Are runtime values consistent with the types? We hope they always are but, especially in a gradually typed language like TS, types will sometimes lie. We will see concrete examples of type dishonesty from _office.js_.  Part 2 will cover the notorious `any` and its safer cousin `unknown`, the type coercion (casting), and TS's type guards. I will also discuss (or rather rant about) coding conventions for transparent, self documenting types. 
* [_add_blank_target Part 3](2022-01-03-ts-types-part3.html). Will cover some of the TS type safety features that I absolutely love.  Throughout the series, we will encounter several examples where TS compiler does not work as expected.
 One of my notes will argue that what TS is and does it quite complex.
* Part 4, Part 5. Will be more theoretical. Notes in Parts 4-5 will discuss topics such as TS's structural, recursive types, subtyping, phantom types, type variable scoping, higher-rank polymorphism (TS supports a version of it!), and type level programming. 
* Part 6. Will be a wrap-up with some final thoughts. 

**Why am I writing these notes?**   
To be honest, it is because I am really impressed and excited about some of the 
type safety features in TS.   

Despite being a superset of JavaScript, TS stands out among mainstream languages as one that supports some interesting types.  
There is a tiny but important feedback loop: the more we play with types the more they will end up being used.  
So, to be perfectly honest, the goal of these notes is to simply play with some interesting types and see how the compiler reacts.

IMO, to master something is to understand its limitations.   
So, to be brutally honest, the goal of these notes is to explore and handle the TS compiler limitations.

These notes will focus on the experience of using types. Not so much on explaining the types
themselves.

**Target audience and prerequisites.** 
I assume that the reader is interested in types and either uses or considers using TypeScript.  
Types tend to be related to FP.  There will not be much FP in these notes.
However, I will use some basic functional programming concepts, like currying, without explaining them.  
TypeScript is a superset of JavaScript with type syntax very similar to any other C-like language. 
These notes will probably be hard to read without some experience with JavaScript or ability to read C-like types.



**About the author.** 
I am spearheading a rewrite of a legacy front-end component at work, the goal is to rewrite it using the new React.js and TypeScript. 
In recent years, I have been spending all of my time in the back-end designing, writing, and maintaining Haskell programs. 
Haskell code has a lot of types. Thus, I use types a lot. Types allow me to code faster, safer, and with much more confidence.  
I wear a hat with types on it when writing TS.  
I love Programming Language Theory and have some experience and lots of interest in all things compiler related.  
I wear a very thin headband with PLT symbols on it under my hat (should be mostly invisible in this series).   
All of this gives me a different (compared to most typescripters) perspective and a reason to write these posts.
For some readers, parts of these posts will feel strange. Established practices like overloading will be considered a bad thing, writing experimental code (that won’t even run) to answer _type questions_ will be a good thing. Strange is a corollary of different.


**What is TypeScript for?**  Is it just a JavaScript add-on used to prevent typos and trivial code errors?  
Or, will TypeScript more fundamentally change the way the code is written?  
Please have these questions in mind when reading these notes.

We will cover a lot of topics.

> “And we never say anything unless it is worth taking a long time to say.”   

_J.R.R Tolkien and Treebeard about discussing types in TypeScript_


## TypeScript is great!

It literally took me less than one minute of playing with TS to get excited about it.   
Just look at the union types (using a somewhat contrived example):

```JavaScript
type Person = {firstNm: string, lastNm: string} 
type NullablePerson = Person | null

const getName = (p:NullablePerson): string => {
    //const tst1 = p.firstNm //does not compile
    if(p===null){
        //const tst2 = p.firstNm //does not compile
        return "John Smith"
    } else {
        return p.firstNm + " " + p.lastNm //compiles
    }
}
```

How cool!  

Talking about my "literal" excitement, my next play example implements `Either` (I am not trying to implement my own Either type, only to play with the language):

```JavaScript
type Either<A,B> = 
| {type: "left", content: A}
| {type: "right", content: B}

const x1: Either<number, string> = {type: "left", content: 1}
const xone: Either<number, string> = {type: "right", content: "one"}
```
```Java
const wrong: Either<number, string> = {type: "left", content: "one"} // does not compile
```

it almost looks like dependent types! TS calls these literal types. (In this example, `"left"` is a type with a 
single value `"left": "left"`.)   
TypeScript calls this programming pattern _Discriminated Unions_.    

And, TS is serious about string property names too:

```JavaScript
const y: Either<number, string> = {"type": "left", "content": 1}
```
```Java
const wrong: Either<number, string> = {"type": "left", "content": "one"} // does not compile
```
  
TypeScript [_add_blank_target _ts-pattern_](https://www.npmjs.com/package/ts-pattern) library uses discriminated unions to implement _pattern matching_. Exhaustive check is part of it.   
Again, really cool. All of these are really exciting developments to me.

Continuing with play examples, here is the full JSON grammar defined in TS.

```JavaScript
type JsonVal = 
| {type: "object", val: Map<string, JsonVal>}
| {type: "array", val: JsonVal[]}
| {type: "string", val: string}
| {type: "number", val: number}
| {type: "bool", val: boolean}
| {type: "null"}

const tstj: JsonVal = {type:"array", val:[{type: "null"}, {type: "number", val: 5}]} //compiles
```
```Java
const wrong: JsonVal = {type: "number", val: {type: "string", val: "5"}} //does not compile, number cannot a nested string
const wrong2: {type: "object",  val:[{type: "null"}, {type: "number", val: 5}]} //does not compile, object is not an array
```

This could have been expressed with OO classes, but it would not be very easy, would it?  
I wrote the `JsonVal` definition without thinking, I have committed `Data.Aeson.Value` (Haskell's commonly used type for JSON values) definition to memory and I just mimicked it.  Then I looked at it again ... holly ... TS supports complex recursive definitions!
We will discuss recursive types later in this series.  

TypeScript has an ability to do type level programming that goes beyond the demonstrated uses of literal types.  All of this is oriented toward creating type safety over various kinds of idiomatic JS code and is limited in scope.  It is nonetheless interesting.  We will return to this topic in the future as well.

As far as mainstream languages go (I consider _Scala_ or _Reason ML_ a border line just outside the mainstream), TypeScript could be the most interesting choice today IMO.  

This was my trailer/preview section. If the code that excited me feels interesting to you, you may enjoy reading these notes.
There will be some gory details (not a lot violence).  You have to decide if type safety is your genre.   
Developers are divided into 2 camps:  Those who use types because that is the most effective way to write software and those who do not use types because that is the most effective way to write software. 
Since you are still reading, I assume you are in camp 1.


## _office.js_.  Using TS in anger

I will use _office.js_ library as a source of examples for this series.  It is a Microsoft product (like TypeScript). 
It comes with TypeScript type definitions (this series uses @types/office-js@1.0.221).   
Looking into the _office.js_ revision history suggests that the bond between _office.js_ and TypeScript developed very early.  It almost looks like these projects grew up together. 
_office.js_ seems like a good 'comprehensive' example for examining the benefits (and frustrations) of using TS in anger.   
Despite some hardships, TS makes working with office.js much, much easier!

As the name suggests, _office.js_ provides an API for working with _Microsoft Office_. 
It allows implementing custom apps that work inside the office suite of products (Microsoft calls these apps add-ins).  
This is not an office.js tutorial but, I hope, the code should be clear to follow.  

As a working example, we will play with code that extracts data from an email opened in Outlook. 
To start, I want to extract the email body.  
To access data, _office.js_ often uses an old style `getAsync` methods that I will modernize using a custom conversion to a `Promise`.  Node's `util.promisify` will not work well for this task.  This is how this could be done in TS:

```JavaScript
/* Utility to convert office functions to promises */
export const officePromise = <T> (getasync: (fx: (r: Office.AsyncResult<T>) => void) => void): Promise<T> => {
    return new Promise((resolve, reject) => {
      getasync((res: Office.AsyncResult<T>) => {
        if(res.status===Office.AsyncResultStatus.Succeeded){
          resolve(res.value)
      } else
          reject(res.error)
      })
   })
  }

```

_side_note_start **Side Note**[^1]: Here is my first criticism of TS.  The ergonomics of function type definitions is IMO really poor.
These definitions are hard to read and cumbersome to write. This syntax does not scale well to more involved types and makes reasoning about types harder.  
E.g. in the above example parameters `fx:` and `r:` cannot be used anywhere (are outside of the lexical scope) and serve only a documentation purpose.
This simple example needs 6 parentheses. The use of `:` and `=>` is confusing.  Function form `A` to `B` is (depending where in the declaration) either `(a: A) => B` or `(a: A): B`. I admit it took me a long time to figure out how to write these and it still takes me forever to read some of these types.   
Later in this post, I will show some work-arounds that simplify type definitions like this one.   
I am adding a big fat **IMO** to this side note, readability is in the eye of ... well the reader.  But seriously...   
_side_note_end

[^1]: I rewrote the side note and improved `officePromise` type definition based on a [_add_blank_target comment](https://www.reddit.com/r/typescript/comments/rnougu/comment/hptsnvg/?utm_source=share&utm_medium=web2x&context=3) from _u/Tubthumper8_ on reddit. (Thanks!) 

Properly initialized office add-in will have access to `Office.context.mailbox.item: Office.MessageRead`.   
This `item` object allows access to the email data.  (The situation is just slightly more complicated since the `item` property is overloaded but that is not important for now.) 
To retrieve the email body I need to use `item.body.getAsync`.  But wait, the type for that version of `getAsync` accepts not only a callback function but also a "body type" parameter.

I am going to resist the temptation to overload `officePromise`.
Instead I will move in a direction that is more fundamental.

Assume that we want 'html' body format, the code can look something like this:

```JavaScript
//retrieving email body, 1st attempt
const bodyType = Office.CoercionType.Html
 
const partiallyAppliedBodyFn = (fn: ((res: Office.AsyncResult<string>) => void)) => 
     item.body.getAsync(bodyType, fn) 
  
const body  = await officePromise<string> (partiallyAppliedBodyFn) // body: string
```

I had to fully specify the `partiallyAppliedBodyFn` type for this to work. 
That looks like a lot of code to just partially apply `item.body.getAsync`!

### Happy path

There are some libraries that offer a `curry` function conversion, but these are typically JS not TS. So I wrote it myself (again, note the type signature is somewhat hard to read):

```JavaScript
export const curry = <T1, T2, R> (fn: (ax: T1, bx: T2) => R): (a: T1) => (b: T2) => R => {
    const res = (a: T1) => (b: T2) => fn(a, b)
    return res
 }

const addtst = (a:number, b: number) => a + b
const curriedAdd = curry(addtst) //const curriedAdd: (a: number) => (b: number) => number
const tst = curry(addtst)(1) //const tst: (b: number) => number
const tst2 = curry(addtst)(1)(2) //tst2 = 3
```

And I have a much simpler code that compiles right off the bat: 

```JavaScript
//Happy path one liner to get email body
//body2: string
const body2 = await officePromise (curry(item.body.getAsync)(Office.CoercionType.Html)) 
```

This worked out quite well and the type checker was able to infer the types!  
This ended up being a happy path.   


### Bumps on the path

In practice, the type checker will often need some help. 
Even more often, the programmer (me) will need help figuring why the code is not compiling.

For example, `item.body.getAsync` offers a 3 parameter overload
which accepts additional `Office.AsyncContextOptions`.  Using it is much harder.
(I will not delve into what the extra argument is for, I just want to see if my code will compile with 3 parameters)

```JavaScript
//boilerplate 'curry3' implementation is not shown (available in the linked github project), 
//it is almost identical to `curry` but accepts a 3 parameter function  

//trying to pass extra parameter to body.getAsync
const emptyConfig: Office.AsyncContextOptions = {}
```
```Java
//body3 does not compile: "Argument of type 'AsyncContextOptions' is not assignable to parameter of type '(asyncResult: AsyncResult<string>) => void'." 
const body3  = await officePromise (curry3(item.body.getAsync)(Office.CoercionType.Html)(emptyConfig)) 
```

To understand what is happening, I sometimes need to spend time annotating things, or picking up the exact overload I want. E.g.

```JavaScript
const useThisAsync = (coercionType: Office.CoercionType
                     , options: Office.AsyncContextOptions
                     , callback: (asyncResult: Office.AsyncResult<string>) => void): void => {
      item.body.getAsync(coercionType, options, callback)
    }
```

This can be tedious, but it typically gets the job done. 
In this particular case, using `curry3(useThisAsync)` fixes the `body3` (or the "3 body", I just had to pun this) problem. 
So, the issue with `body3` code appears to be related to overloading.

Looking closer at the types, I notice that not only `item.body.getAsync` has two overloads, but the one I want is accepting a union type argument and the callback is optional:

```JavaScript
//from office.js documentation

//2 parameter overload used in happy path
getAsync(coercionType: Office.CoercionType | string
  , callback?: (asyncResult: Office.AsyncResult<string>) => void): void;

//3 parameter overload we are trying to use now
getAsync(coercionType: Office.CoercionType | string, 
        options: Office.AsyncContextOptions, 
        callback?: (asyncResult: Office.AsyncResult<string>) => void): void;
```

So there are sort of _overloads on top of overloads_ and the type checker probably gets confused.   
The compilation error suggests that the compiler gets stuck on a wrong (the 2 parameter) version of `getAsync` despite the use of the 3 parameter `curry3`. I will also confirm this hypothesis using a _type hole_ (we will learn what that is) in the next section.  
I expect the type checker to backtrack and try the next overload, but for some reason it does not want to do that on its own.   
I do not blame TS, overloading gives me a headache too.   
Overloading is known for being not type inference friendly (incidentally, that is the reason why Haskell does not overload names).  

_side_note_start **Side note:** 
Acting on my hypothesis of what is wrong with `body3`, I can push this code to a ridiculous limit:

```JavaScript
//this compiles by using a wrong input parameter type and returns 'unknown'
const crazyConfig : (_: Office.AsyncResult<string>) => void = x => ""
const body4 = await officePromise (curry3(item.body.getAsync)(Office.CoercionType.Html)(crazyConfig)) 
```

Besides this being completely wrong, 
I do not understand what causes the widening of `body4` type to `unknown`. There is enough information in the overloaded `item.body.getAsync` method for the type checker to infer the `string`. My guesswork is too hypothetical to discuss it here.   
Especially for the return types, any widening to `unknown` would IMO be better served as a compilation error.  We want these to be narrow, not wide.  I will show more examples of such widening and
I will discuss safety concerns related to the `unknown` type in future notes.  
_side_note_end


There is something worryingly asymmetric about a 2 parameter overload compiling without additional help and a 3 parameter overload needing a developer intervention. Should I worry that a future version of TS will flip this preference and all of my code that uses the 2 parameter overload will stop compiling?  How stable is this arbitrary complexity?  

_If you are an API owner, my advice is to not overload. IntelliSense works better, type inference works better, developer head hurts less without overloads._


One type that is notorious for needing annotations is the TypeScript's _tuple_.
Typescript overloads array syntax `[]` to define tuples (some readers may prefer the term heterogeneous lists). This is an example of a tuple: `[2,"two"]: [number, string]`. 
The syntax overloading probably does not help TS in inferring the type and the type checker often gives up or infers the array type. 

I am concerned that **many developers will give up** trying to write this type of code. 
My concern is also that developers will resort to unsafe type coercion / type casting. 
There will be a lot of `myvar as IWantIt`, or a lot of the `any` type.

Was this enough gore for you?  You say it was not?  I say you did not see the content of that email!

### Bump leveling tools


#### Readable Type Definitions  
Cumbersome type annotations are not a good excuse to give up!  There is a way to simplify function type definitions.  For example, I can define a helper alias:

```JavaScript
//DIY reusable type for Office getAsync callbacks
export type OfficeCallack<T> = (_: Office.AsyncResult<T>) => void
```

Here is how this simplifies the previously defined `partiallyAppliedBodyFn`: 

```JavaScript
//before:
const partiallyAppliedBodyFn1 = (fn: ((res: Office.AsyncResult<string>) => void)) => item.body.getAsync(Office.CoercionType.Html, fn) 
//after:
const partiallyAppliedBodyFn2 = (fn: OfficeCallack<string>) => item.body.getAsync(Office.CoercionType.Html, fn)
```

Notice **no more redundant parameter definitions** in the type signature and a much easier to read syntax.  
The next version is **my personal preference** (it nicely separates the type and the implementation):

```JavaScript
const partiallyAppliedBodyFn3: (_: OfficeCallack<string>) => void = 
  fn => item.body.getAsync(Office.CoercionType.Html, fn)
```

#### Type Application  
Returning to my failed `body3` example, instead of trying to type annotate with full type signatures, it is sometimes more convenient to apply the types.  Here, I have the "generic" (or polymorphic) `curry3` function that I can apply the types 
`CoercionType`, `AsyncContextOptions`, `OfficeCallack<string>`, and `void` to:

```JavaScript
//type applied version, it just compiles!
const emptyConfig: Office.AsyncContextOptions = {}
const body3  = await officePromise<string> (
  curry3<Office.CoercionType, Office.AsyncContextOptions, OfficeCallack<string>, void> //explicity specified type parameters
     (item.body.getAsync)
     (Office.CoercionType.Html)
     (emptyConfig)
  ) 
```

That is so much easier than specifying the exact `useThisAsync` overload!

#### Type Holes

A DIY type hole technique is sometimes useful to help figure out stubborn types (see [_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck)).  

```JavaScript
//genric (why not say polymorphic) bottom function will allow me to ask type questions
export const _ = <T>(): T => {
    throw new Error("hole"); 
}
```

A type hole allows me to ask the compiler _type questions_.   
You can learn a lot about how the type checker works using it.  E.g. using my `Either` type as an example:

```JavaScript
const tstnum: Either<number, string> = {type: "left", content: _()}
```

if you hover over `_` you will see

```JavaScript
(alias) _<number>(): number
```

Nice!  If you hover over `_` in this expression

```JavaScript
const str = "Hello " + _()
```

you will see

```JavaScript
(alias) _<unknown>(): unknown
```

This can provide a lot of insight into types and how TS uses them!  

I have not been very lucky in using type holes to figure out why TS is confused. 
Returning to my failed `body3` example:

```JavaScript
//body3 inferred type is 'unknown'
const body3  = await officePromise (curry3 (item.body.getAsync)(Office.CoercionType.Html)(_())) 
```

if I hover over the `_` function, the IntelliSense suggests this completely wrong type:

```JavaScript
(alias) _<((asyncResult: Office.AsyncResult<string>) => void) | undefined>(): ((asyncResult: Office.AsyncResult<string>) => void) | undefined
```

The type hole confirms that the compiler is trying to match against the two parameter overload of `item.body.getAsync`.
This verifies my hypothesis from the last section.  There are a few things to note here: 

* We are asking TS "why are you confused?" and that is a funny question. 
* This type hole did not tell us more than the compilation error message itself. 
  However, the type hole is more targeted so it could reveal more specific information in some cases. 
* Type holes may tell us something useful in situations where the code compiles but we do not understand why.

If, as before, I add the type application (`<Office.CoercionType, Office.AsyncContextOptions, OfficeCallack<string>, void>`) to `curry3`
the `_()` will show the type correctly:

```JavaScript
(alias) _<Office.AsyncContextOptions>(): Office.AsyncContextOptions
```

**Unfortunate limitation**   
Sadly, the ` _<T>(): T` is not universally useful, e.g. this will not compile:

```Java
//compilation error: 
//       Argument of type '(ax: never, bx: never) => never' is not assignable 
//       to parameter of type '(ax: unknown, bx: unknown) => unknown'.
const testfn = curry(_()) 
```
```JavaScript
//interestingly the following compiles as curry<unknown, unknown, unknown>
const testfn = curry({} as any)
```

This looks to me like growing pains, it should work, and probably will in future versions of TS.
It is an example of code that really should compile but it does not.

There is an interesting relationship between the `never` type and ` _<T>(): T`. There will be a future note about it.   
The type hole `_` function is a useful tool and we will keep using it in future type explorations. 


Using types requires some experience, knowledge, and patience. 
More advanced types come with more misleading error messages, it takes experience to find the underlying cause of a misleading compilation error, and that is true in any language. 
Eventually, I (and you) will look at a TS compilation error
and will say "ah, you really meant this: ...".

I am mostly left to my own devices when working with more involved types in TS. 
Hopefully the future will bring us mainstream grade interactive tools that allow asking type questions, browsing types,
help solving type puzzles. For now it is mostly the programmer who connects the dots.  
The good news is that this gets easier and easier with practice. I have been working in TS for only about 2 months now and I already see a difference.   

_Good code requires two type checkers: TypeScript and You_


### Compilation bloopers 

We already saw "correct" programs that should have compiled but did not (E.g. `curry(_())`, `body3` example) and we will see more in the future notes.
Our `body4` example compiled but was clearly wrong.  
This note shows other, less contrived, examples that compile but clearly should not.  

All of these type check:

```JavaScript
//annotated correct code added for reference, this code compiles
const good: (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (item.body.getAsync)

//compiles but it should not, compiles even with type annotation
const nonsense1: (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (curry (item.body.getAsync)) 

//compiles but it should not
const nonsense2 = curry(curry)

//more examples in the linked github project
```

and all, except the first one, should not.  

I expect the type checker to be effective at rejecting nonsensical code.  If a blooper happens, it should be rare, contrived code, unlikely for developers to write.  My examples are somewhat surprising since higher order functions are not uncommon in JavaScript.  The second example is a piece of code I accidentally wrote in my project.  
This is very concerning since errors like these are likely to remain uncaught and become escaped bugs. 

No compiler is perfect, but you probably noticed by now that TS compiler seems to get in trouble a lot. 
Compared to other programming languages I use, TS's rate of compilation issues is much higher, the issues are more dangerous, and these bloopers are happening on more commonly used vanilla code (well... at least commonly used by me).  
I have no idea what the underlying issues are but I can see one general reason for this:
gradual typing on top of JS is not easy.  I plan to write a note about the complexity of TS types in a future post.  
I am sure that TS keeps improving and fixing such issues but I expect the progress to be slow. 


### It's all worth it

One common concern related to using types (especially more advanced types) is a slowdown in the development speed.  
There is some truth to this in general because of things like compilation times in some language environments. 
I cannot comment on TS compilation times for large projects, so far it is not a problem for me.  In my experience, having a type checker is a huge 
productivity bust.  In my experience, the more types the faster the development speed. That is true even with compilation bloopers.  
Efficiency considerations are somewhat personal so your experience may vary.  

I rewrote some legacy code using the techniques in this section.
That effort resulted in significant size reduction and an overall big improvement in readability and correctness when compared
to the code I was replacing or to code in the _office.js_ documentation.  
A lot of the improvement comes from using `await` `async` syntax sugar but converting functions to their curried form and figuring out more terse ways to type annotate also results in added clarity and significant syntactic simplification. 

In my book, there is just no comparing TS to JS, TS is the clear winner.  
How does TS compare to statically type checked front-end languages that compile to JS and have capable type checkers and solid types (e.g. ReasonML, Elm, PureScript, even Haskell)?  I am not in a good position to discuss this yet.   
Lots of projects need to stay close to JS, my project at work falls into this group.  For such projects TS is the right choice IMO.


## Next Chapter

We are not done with _office.js_.  I will use it in future notes.

Do statically defined types reflect the actual runtime values? 
How to assure that they do?   
We will discuss these questions in the next installment.
~~I have the draft ready and I hope to publish it in a few weeks.~~  
Here is the link: [_add_blank_target Part 2. Typing Honestly](2021-12-24-ts-types-part2.html)



