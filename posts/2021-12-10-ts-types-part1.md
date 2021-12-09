---
title: Type Enthusiast's Notes about TypeScript. Part1. Using Types in Anger
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary:  TypeScript Types series, Introduction, office.js, working with and fighting type checker
toc: true
tags: TypeScript
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
Proofreading this beast of a post appears to be a Sisyphean task. When this note
disappears, you will know that I gave up.)_   


## Introduction to the series 

This work started as a short post and ended up with the size of a short book.  
I decided to split it into digestible installments and this is the first one.  

Here is my plan:

* Part 1 (this post). Is a warm-up. Part 1 has been motivated by my current project at work that uses TS. 
  I will discuss some type checking hardships and ways to solve them. 
  I will show code examples that compile but really, really should not, and code examples that should compile but surprisingly don’t.  
* Part 2. Will be about keeping types honest. Do types reflect runtime values? We hope they always do but, especially in a gradually  typed language, types will sometimes lie. I will examine concrete examples using _office.js_.  Notes in this post will cover the `any` and `undefined` types, type coercion (casting), and TS type guards.
* Part 3. Will cover some of the TS type safety features that I absolutely love and will include bunch of my rants (e.g. importance of specifying return types).  Throughout the series, we will encounter several examples where TS type checker does not work as expected.
 My final rant will try to explain why this is the case (spoiler alert: blaming it on JS and overall complexity of TS types).
* Part 4. Will be more theoretical. The notes will discuss topics such as TS's structural recursive types, subtyping, higher-rank polymorphism (TS supports a version of it!), and type level programming. 
* Part 5. Will be a wrap-up with some final thoughts. 

**Why am I writing these notes?**   To be honest, it is because I really got impressed and excited about some of the 
type safety features in TS.   

IMO one the highest level of understanding a technology is understanding its limitations.   
If you agree and narrow (sorry about the pun) it to the subject of my notes, you may conclude:  
It is not about knowing the language, it is about knowing the types ...and the language limitations in supporting them.  
(: _imagine this is a footnote:_ continuing this line of thought could lead to the conclusion that hiring manager have it all wrong, 
so maybe there is something in it. :)  
You may disagree with the sentiment that types are everything...     
So, to be perfectly and brutally honest, I am writing these notes to explore these limitations in the context of TS.


**Target audience and prerequisites.** 
I expect that the reader is interested in types and either uses or considers using TypeScript.  
Types tend to be related to FP.  There will not be much FP in these notes.
However, I will use some basic functional programming concepts, like currying, without explaining them.  


**About the author.** 
I am spearheading a rewrite of a legacy front-end component at work, the goal is to rewrite it using the new React.js and TypeScript. 
I have programmed professionally for a very wrong time.  
In recent years I have been spending most of my time in the back-end designing, writing, and maintaining Haskell code.

_So... Please make sure to correct me if I get anything wrong or if I am missing something._  




In this series of notes, I will explore TypeScript types and discuss what it is like to use them in anger.

I love types. I use types a lot. Types allow me to code faster, safer, and with much more confidence. 
I hope these notes will be interesting to like-minded JavaScript and TypeScript developers who are serious about types and try using the type checker to its full advantage. Practitioners of other statically typed languages may find TypeScript interesting and exciting, as did I.  These notes may help those readers decide if TS is something they will add to their quiver.

You can take a quick look at the TOC to get a handle on what these notes are about.   
We will stay close to idiomatic TypeScript but with little twists to demonstrate some interesting uses of types. 
Some familiarity with TypeScript is assumed, but readers not familiar with the language should be able to guess/infer what is going on. I will use some basic functional programming concepts, like currying, without explaining them.   
The end of this post gets a little more theoretical and may be harder to follow for some readers, but really
there is only one prerequisite to reading these notes: interest in types.   

I am spearheading a rewrite of a legacy front-end component at work, the goal is to rewrite it using the new React.js and TypeScript.  One of my goals in these notes is to share my experience with TypeScript and my approach to using it.  
This is my first non-Haskell project in 3 years.  I have my FP hat on when writing TS.  These notes are all about types, but a tiny bit of FP may sneak in, e.g. currying. 
Also, I have not done any major JS development in the last 9 years which makes this experience even more interesting.  

_Please make sure to correct me if I get anything wrong or if I am missing something._  

I think all of this gives me a different perspective than most typescripters have and a reason to write this post for others to see.  
For some readers, parts of this post will feel strange. Established practices like overloading will be considered a bad thing, writing experimental code that won't even run will be a good thing.  Strange is a corollary of different.  
We will work with TS to solve type puzzles, figure out how to effectively beg TS to accept our code, encounter code examples that compile but really, really should not, and code examples that should compile but surprisingly don't. 
If these things look like vanilla to you, then well you still may find something interesting, otherwise
you will find something that is, at least, new to you.
These notes present practical examples, coding strategies and tricks, and some theoretical topics.  The end of the post includes a few tiny bits of Programming Language Theory.  A few high level comparisons with languages that use a lot of types (e.g. Haskell, PureScript, Elm) are also included if there is a strong conceptual relevance to TS.  

What is TypeScript for?  Is it just a JavaScript add-on used to prevent typos and trivial code errors?  
Or, will TypeScript more fundamentally change the way the code is written?
Please have these questions in mind when reading these notes.

This is a long post, probably the longest I have ever written.  It will cover a lot of topics.   
You should make yourself a large coffee
... or better yet pick up a water bottle to stay hydrated.  
... seriously, the best idea is to read just a few notes at a time (that is how I proofread them) 

> “And we never say anything unless it is worth taking a long time to say.”   

_J.R.R Tolkien and Treebeard about discussing types in TypeScript_


## TypeScript is great!

It literally took me less than one minute of playing with TS to get excited about it.   
Just look at the union types (using a somewhat contrived example):

```JavaScript
type Person = {firstNm: string, lastNm: string} 
type NullablePerson = Person | null

const getName = (p:NullablePerson): string => {
    //const tst1 = p.firstNm //will not compile
    if(p===null){
        //const tst2 = p.firstNm //will not compile
        return "John Smith"
    } else {
        return p.firstNm + " " + p.lastNm //compiles
    }
}
```

How cool!  

**Disclaimers:** (imagine this is a very small font:)   
_The above code may require something like `strictNullChecks` compiler flag. In my notes
I assume strict compiler flags are on, something you get by default with scaffolding, e.g. using
`create-react-app my-project --template typescript` is close enough.  
The code examples have been tested with TypeScript v4.4.4 and v4.5.2.  
This post is a pandoc output of a markdown document and code examples are not interactive.
Most code examples are published in _ts-notes_ folder in this repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)   
A lot of interaction with TS happens using IntelliSense, I have used vscode v1.62.3 (2021-11-17T08:00:36.721Z), Linux x64 5.4.0-90-generic snap which comes with TS support built in._

Talking about my "literal" excitement, my next play example implements `Either` (I am not trying to implement my own Either type, only to play with the language):

```JavaScript
type Either<A,B> = 
| {type: "left", content: A}
| {type: "right", content: B}

let x: Either<number, string> = {type: "left", content: 1}
//let wrong: Either<number, string> = {type: "left", content: "one"} // will not compile
```

it almost looks like dependent types! TS calls these literal types.  And, TS is serious about string property names too:

```JavaScript
let y: Either<number, string> = {"type": "left", "content": 1}
//let wrong: Either<number, string> = {"type": "left", "content": "one"} // will not compile
```
  
TypeScript [_add_blank_target _ts-pattern_](https://www.npmjs.com/package/ts-pattern) library uses literal types to implement _pattern matching_. Exhaustive check is part of it.   
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
//const wrong: JsonVal = {type:"array", val:[{type: "number", val: {type: "string", val: "5"}}]} //does not compile, number cannot a nested string
//const wrong2: {type: "object",  val:[{type: "null"}, {type: "number", val: 5}]} //does not compile, object is not an array

```

This could have been expressed with OO classes, but it would not be very easy, would it?  
I wrote the `JsonVal` definition without thinking, I have committed `Data.Aeson.Value` (Haskell's commonly used type for JSON values) definition to memory and I just mimicked it.  Then I looked at it again ... holly ... TS supports complex recursive definitions!
More on this at the end of this post.  

TypeScript provides the ability to do type level programming that goes beyond the demonstrated uses of literal types.  All of this is oriented toward creating type safety over various kinds of idiomatic JS code and is limited in scope.  Again, more on this at the end of this post.

As far as mainstream languages go (I consider _Scala_ or _Reason ML_ a border line just outside the mainstream), TypeScript could be the most interesting choice today IMO.  

This was my intro/trailer section. If the code that excited me feels interesting to you, you may enjoy reading on.  There will be some gory details (not a lot violence), some triumphs and failures.  You have to decide if type safety is your genre. 

Developers are divided into 2 camps:  Those who use types because that is the most effective way to write software and those who do not use types because that is the most effective way to write software. 
Since you are still reading, I assume you are in camp 1.


## _office.js_.  Using TS in anger

I will use _office.js_ library as a source of examples for this post.  It is a Microsoft product (like TypeScript). 
_office.js_ comes with TypeScript type definitions. 
It is also relatively old and has been retrofitted for TS. Many other JS ecosystem libraries have traveled a similar JS to TS path. 
It seems like a good 'comprehensive' example for examining the benefits (and frustrations) of using TS in anger.   
Despite some hardships, TS makes working with office.js much, much easier!

As the name suggests, _office.js_ provides an API for working with _Microsoft Office_. 
It allows implementing custom apps that work inside the office suite of products (Microsoft calls these apps add-ins).  
As a working example, we will play with code that extracts data from an email opened in Outlook.   
This is not an office.js tutorial but, I hope, the code should be clear to follow. 

To start, I want to extract the email body from the email.  
To access data, _office.js_ often uses an old style `getAsync` methods that I will modernize using a custom conversion to a `Promise`.  Node's `util.promisify` will not work well for this task.  This is how this could be done in TS:

```JavaScript
/* Utility to convert office functions to promises */
export const officePromise = <T> (getasync: ((fx: ((r: Office.AsyncResult<T>) => void)) => void)): Promise<T> => {
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

_Side Note:_ Here is my first criticism of TS. What could possibly be the goal of having to name function parameters in the type definitions (`fx:` and `r:` in the above example) in a not dependently typed language? 
You will not use them, they are inside the type definition!  Do they serve a documentation purpose?  This seems like a needless boilerplate and makes writing and reading the code harder.  I will show a work-around for this later in this post.   
The declaration syntax overloads the meaning of both `:` and `=>`.  Function form `A` to `B` is (depending where in the declaration) either `(a: A) => B` or `(a: A): B`.  I will show a work-around for this later in this post as well.    
TS type definitions are over-parenthesized,  hard to read, and confusing to write.  This syntax does not scale well to more involved types and makes reasoning about types harder.   
I am adding a big fat **IMO** to this side note, readability is in the eye of ... well the reader.  But seriously...   
_(Side Note End)_

Properly initialized office add-in will have access to `item: Office.MessageRead` (available statically as `Office.context.mailbox.item`). `item` allows access to elements of the email with which the add-in interacts.  
To retrieve the email body I need to use `item.body.getAsync`.  But wait, the type for that version of `getAsync` accepts not only a callback function but also a "body type" parameter.

I am going to resist the temptation to overload `officePromise`.
Instead I will move in a direction that is more fundamental, and one that turns out to be more type checker friendly.

For now, assume that we want 'html' body format, the code can look something like this:

```JavaScript
//retrieving email body, 1st attempt
const bodyType = Office.CoercionType.Html
 
const partiallyAppliedBodyFn = (fn: ((res: Office.AsyncResult<string>) => void)) => 
     item.body.getAsync(bodyType, fn) 
  
const body  = await officePromise<string> (partiallyAppliedBodyFn) // body: string
```

I had to fully specify the `partiallyAppliedBodyFn` type for this to work. 
That looks like a lot of typing to just partially apply `item.body.getAsync`!

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
const body = await officePromise (curry(item.body.getAsync)(Office.CoercionType.Html)) 
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
//trying to pass extra parameter to body.getAsync
const emptyConfig: Office.AsyncContextOptions = {}
//body3 does not compile: "Type ... is not assignable to type ..." 
//const body3  = await officePromise (curry3(item.body.getAsync)(Office.CoercionType.Html)(emptyConfig)) 
```

Interestingly the following does compile but with `body3:unknown`, why why why!

```JavaScript
//this snipet compiles but uses 'any' and returns 'unknown'
const emptyConfig: any = {}
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

I think of such annotating work as a poor man's REPL.  It can be tedious.   

It appears, the issue is that not only `item.body.getAsync` has two overloads, but the one I want is accepting a union type argument and the callback is optional:

```JavaScript
(method) Office.Body.getAsync(coercionType: string | Office.CoercionType
  , options: Office.AsyncContextOptions
  , callback?: ((asyncResult: Office.AsyncResult<string>) => void) | undefined): void 
```

So there are really _overloads within overloads_ and the type checker probably gets confused. 
The type checker appears to sometimes have a problem with backtracking when analyzing branching overloads. (I do not blame TS, these give me a headache too.)
Overloading is an issue with type inference in general (e.g. the reason why Haskell does not overload names).  

_If you are an API owner, my advice is to not overload. IntelliSense works better, type inference works better, developer head hurts less without overloads._

One type that is notorious for needing annotations is TypeScript's _tuple_.
Typescript overloads array syntax `[]` to define tuples (you may prefer the term heterogeneous lists). This is an example of a tuple type: `[number, string]`. 
The syntax overloading probably does not help TS in inferring the type and the type checker gives up or infers the wrong type (the array). 

We will discuss type inference issues and some reasons behind these issues in future notes.

I am concerned that **many developers will give up** trying to write this type of code. 
My concern is also that developers will resort to unsafe type coercion / type casting. 
There will be a lot of `myvar as IWantIt`, or a lot of the `any` type.

Was this enough gore for you?  You say it was not?  I say you did not see the content of that email!

### Leveling the bumps

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

Notice **no more redundant argument definitions** in the type signature and a much easier to read syntax.  
The next version is **my personal preference** (it separates the type and the implementation more):

```JavaScript
const partiallyAppliedBodyFn3: ((_: OfficeCallack<string>) => void) = 
  fn => item.body.getAsync(Office.CoercionType.Html, fn)
```

Returning to my failed `body3` example, instead of trying to specify a full type signature, it is sometimes more convenient to apply the types.  Here, I have the generic (some call it polymorphic) `curry3` function that I can apply the types 
`CoercionType`, `AsyncContextOptions`, `OfficeCallack<string>`, `void` to:

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

A DIY type hole technique is sometimes useful to help figure out stubborn types (see [_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck)).  
You can use it to learn a lot about how type checker works.  E.g. in my `Either` example:

```JavaScript
//genric (why not say polymorphic) bottom function will allow me to ask type questions
export const _ = <T>(): T => {
    throw new Error("hole"); 
}

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

This can give you a lot of insight into types and how TS uses them!  
However, sometimes the only thing you learn is that the type checker is confused. 
Returning to my failed `body3` example:

```JavaScript
const body3  = await officePromise (curry3 (item.body.getAsync)(Office.CoercionType.Html)(_())) 
```

if I hover over the `_` function, the IntelliSense suggests this completely wrong type:

```JavaScript
(alias) _<((asyncResult: Office.AsyncResult<string>) => void) | undefined>(): ((asyncResult: Office.AsyncResult<string>) => void) | undefined
```

If I add the type application as above (`<Office.CoercionType, Office.AsyncContextOptions, OfficeCallack<string>, void>`) to `curry3`
it will show the type correctly:

```JavaScript
(alias) _<Office.AsyncContextOptions>(): Office.AsyncContextOptions
```

(so that is kinda useless, it just repeated the information I gave to TS).

Sadly, the ` _<T>(): T` is also not universally useful, e.g. this will not compile:

```JavaScript
//const test = curry(_()) //compilation error 

//interestingly the following unifies as curry<unknown, unknown, unknown>
const test = curry({} as any)
```

(This looks like growing pains, it should work, and probably will in future versions of TS.)

We will examine the relationship between [`never` type](#never-type-vs-tt) and ` _<T>(): T` in more details in future notes. 


Using types requires some experience, knowledge, and patience. 
More advanced types come with more misleading error messages, it takes experience to find the underlying cause of a misleading compilation error, and that is true in any language.   

I am mostly left to my own devices when working with more involved types in TS. 
Hopefully the future will bring us mainstream grade interactive tools that allow asking type questions, browsing types,
help solving type puzzles. For now it is mostly the programmer who connects the dots.  
The good news is that it gets easier and easier with practice. I have been working in TS for only about 2 months now and I already see a difference.   

_A good code needs two type checkers: TypeScript and You_


### Type checking bloopers 

This note shows examples of code that compiles but clearly should not.  We already seen some "correct" programs that should compile but do not (e.g. `curry(_())`) and we will see one more later in this post.

All of these type check:

```JavaScript
//annotated correct code added for reference, this code compiles
const good: (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (item.body.getAsync)

//compiles but it should not, compiles even with type anotation
const nonsense1: (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (curry (item.body.getAsync)) 

//compiles but it should not
const nonsense2 = curry(curry)
```

and all, except the first one, should not.  
I expect the type checker to be effective at rejecting nonsensical code.  If it happens, it should be rare, contrived code, unlikely for developers to write.  My examples are somewhat surprising since higher order functions are not uncommon in JavaScript.  The second example is a piece of code I accidentally wrote in my project. It took some effort to figure out why my program does not work.  

I think these are examples of growing pains. TS will fix these in the future. 
Today, I have to be prepared for a little fight with TS and I also have to accept that sometimes TS is wrong. 


### It's all worth it

One common concern among developers related to using types (especially more advanced types) is a slowdown in the development speed.  
There is some truth to this in general because of things like compilation times in some language environments. 
I cannot comment on TS compilation times for large projects, so far it is not a problem for me.  In my experience, having a type checker is a huge 
productivity buster.  Programming languages have not been created equal.  In my experience, the more types the faster the development speed. 
Efficiency considerations are somewhat personal so your experience may vary.  

I rewrote some legacy code using the techniques in this section.
That effort resulted in significant size reduction and an overall big improvement in readability and correctness when compared
to the code I was replacing or to code in the _office.js_ documentation.  
A lot of the improvement comes from using `await` `async` syntax sugar but converting functions to their curried form and figuring out more terse ways to type annotate also results in added clarity and significant syntactic simplification. 

We are not done with _office.js_.  I will use it for more examples and more coding in anger in future notes.

