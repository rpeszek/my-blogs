---
title: Types Enthusiast's Notes on Using TypeScript in Anger and with Curiosity
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary:  A short post that turned into a short book about TypeScript types.
toc: true
tags: TypeScript
codestyle: ts
---
**_A short post that turned into a short book about TypeScript types_**

_Please Leave Feedback in: [git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
Proofreading this beast of a post appears to be a Sisyphean task. When this note
disappears, you will know I gave up.)_   

## Introduction

I love types and I use them a lot. Types allow me to code much faster and with much more confidence. 
These notes are about types in TypeScript.
I hope these notes will be interesting to like-minded JavaScript and TypeScript developers who enjoy exploring types and try using the type checker to its full advantage. Practitioners of other statically typed languages may find TypeScript interesting and exciting, as did I.  These notes may help you to decide if TS is something you want to add to your quiver.

I am spearheading a rewrite of a legacy front-end component at work, the goal is to rewrite it using the new React.js and TypeScript.  One of my goals in these notes is to share my experience with TypeScript and my approach to using it.  

You can take a quick look at the TOC to get a handle on what these notes are about.   
There is only one prerequisite to reading these notes: interest in types.   
We will stay close to idiomatic TypeScript but with little twists to demonstrate some interesting uses of types. 
Some familiarity with TypeScript is assumed, but readers not familiar with the language should be able to guess/infer what is going on. I will use some basic functional programming concepts, like currying, without explaining them.   
The end of this post gets a little more theoretical and may be harder to follow for some readers.
    
This is my first non-Haskell project in 3 years.  I have my FP hat on when writing TS.  These notes are all about types, but a tiny bit of FP may sneak in, e.g. currying. 
Also, I have not done any major JS development in the last 9 years which makes this experience even more interesting.  

_Please make sure to correct me if I get anything wrong or if I am missing something._  

I think all of this gives me a different perspective than most typescripters have and a reason to write this post for others to see.  
For some readers, parts of this post will feel strange. Established practices like overloading will be considered a bad thing, writing experimental code that won't even run will be a good thing.  Strange is a corollary of different.  
We will work with TS to solve type puzzles, figure out how to effectively beg TS to accept our code, encounter code examples that compile but really, really should not, and code that should compile but surprisingly doesn't. 
If these are all vanilla to you, then well you still may find something interesting, otherwise
you will find something that is, at least, new to you.
These notes present practical examples, coding strategies, and some theoretical topics.  The end of the post includes a few tiny bits of Programming Language Theory.  A few high level comparisons with languages that use a lot of types (e.g. Haskell, PureScript, Elm) are also included if there is a strong conceptual relevance to TS.  

What is TypeScript for?  Is it just a JavaScript add-on used to prevent typos and trivial code errors?  
Or, will TypeScript more fundamentally change the way the code is written?
Please have these questions in mind when reading these notes.

This is a long post, probably the longest I have ever written.  It will cover a lot of topics.   
You should make yourself a large coffee
... or better yet pick up a water bottle to stay hydrated.  
... actually you are likely to get overwhelmed if you try reading all notes in one sitting (this happens to me when I try to just proofread them) so if these topics look interesting, then bookmark and come back later. 

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

I will rant about type inference issues some more in the [On complexity of TS types](#on-comparative-complexity-of-ts-types) note.

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

Using types requires some experience, knowledge, and patience. 
More advanced types come with more misleading error messages, it takes experience to find the underlying cause of a misleading compilation error, and that is true in any language.   

I am mostly left to my own devices when working with more involved types in TS. 
Hopefully the future will bring us mainstream grade interactive tools that allow asking type questions, browsing types,
help solving type puzzles. For now it is mostly the programmer who connects the dots.  
The good news is that it gets easier and easier with practice. I have been working in TS for only about 2 months now and I already see a difference.   

_A good code needs two type checkers: TypeScript and You_


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

## Type checking bloopers 

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


## Can I trust the types?

Compiler issues aside, I want to talk about the most obvious gotcha. 
I am going back to my `Person` example. JSON incompatibility issues are a real maintenance challenge. This will be a good conversation starter:

```JavaScript
//Questionable JSON parsing example
const p: Person = JSON.parse('"John Smith"')
```

The actual run-time value of `p` will be a `string`, while the type checker is now convinced it is a `p:Person`.   
`JSON.parse` function is declared to return `any` type.  Using `any` bypasses type checking. `any` can be assigned to any type, here it is assigned to `Person`.  

Now, look at the top rated answer in this stackoverflow: 
[_add_blank_target how-to-parse-json-string-in-typescript](https://stackoverflow.com/questions/38688822/how-to-parse-json-string-in-typescript). 
It appears that the above code matches the user community's preferred way of parsing JSON. 
Yes, better ways are available (look at less popular answers to the same stackoverflow, I will discuss
TS type guards in [Casting Note](#casting-casting-in-a-bad-light)).  

Now, since I already may have angered a large part of the TS community (did I? I hope not.), **let me return to _office.js_**  to beat on it a little more.   
In my [happy path example](#happy-path), I have hard-coded the use of 'http' as the email body type.  Both _office.js_ documentation and the IntelliSense tells me that I can retrieve underlying email body type using an overloaded `item.body.getTypeAsync` method:

> (method) `Office.Body.getTypeAsync` ...
> Gets a value that indicates whether the content is in HTML or text format.

When I tried to use it, this property was always `undefined`, I never saw it using the developer tools either.
A version incompatibility? I do not think it is, I am using office online and the latest available office.js (@types/office-js@1.0.221).
 
_Seems like _office.js_ types sometimes lie._

We should look at the type definition of the _office.js_ `Office.context.mailbox.item` a little closer.  
This property is overloaded to be one of the following types (let me call them _facets_):


> `Office.AppointmentCompose` (for editing calendar entry)   
> `Office.AppointmentRead`  (for viewing calendar entry)   
> `Office.MessageCompose`  (for editing new email)   
> `Office.MessageRead`  (for interacting with received or sent email being viewed by the user)  


These _facet_ types are all different.  For example, to get email subject you use `item.subject:string` if you are working with `Office.MessageRead` or `item.subject:Office.Subject` if you are working with `Office.MessageCompose`.   
`Office.Subject` contains `getAsync`, `setAsync` methods and is completely and absolutely not a `string`.  

The type of `item` provided by _office.js_ is not, as I would expect: 

```JavaScript 
//Type I expected
AppointmentCompose | AppointmentRead | MessageCompose | MessageRead
``` 

Rather it is closer (I have not listed all the `&`-s) to: 

```JavaScript 
//Actual Type with some & parts removed 
AppointmentCompose & AppointmentRead & MessageCompose & MessageRead
```  

Basically, the type chosen by _office.js_ combines all the available properties, methods, overloads into one type. 
This is simply an incorrect type for the `item` property.  Runtime values do not satisfy that type, they satisfy the union type.
Type checked programs will fail at runtime.

_Seems like _office.js_ types sometimes lie._ And I am repeating myself.

None of the examples shown in this note should be used to blame TypeScript. 
None of them should be a surprise. 
Less trustworthy types are a limitation we have to accept if we want gradual typing over the wild-west JS. 

Rather, it is on the TypeScript developers (including API developers) to be extra diligent in making sure that the types match the values.   
What are the types good for if you cannot trust that they are accurate?

> "You take the blue pill — the story ends, you wake up in your bed and believe whatever you want to believe.   
> You take the red pill — you stay in Wonderland, and I show you how deep the rabbit hole goes"

_Morpheus about not believing types in a gradually typed language_    
_... nightmares of JavaScript running on my walls and ceilings make me wake up screaming_


### Side Note about the `any` type

`any` type is a little crazy.  It behaves like the _top_ (you can assign any other type to it). It also behaves like the _bottom_ (it can be assigned to any other type, maybe except of _never_). 
The bottom type is supposed to be empty, this one clearly is not.  

_As a result, any value can have any type._

We should have some fun with this.

```JavaScript
//express yourself with _any_ (notice no casting, only assignments)
const sadAny: any = "emptiness and sadness"
const sadVoid: void = sadAny

const myCallback = (n: number): void => {
    return sadVoid;
}
```
You can have your own favorite `null` that is not `null` value, you can _define_ your own `undefined`. 
Sky and your creativity are the limits.  (I will spoil this party and say that I do not recommend doing it.)  

Bottom type that is not empty will cause language to be unsound. Allowing all values in the bottom type, I would call it insane.   
However, using _any_ type similar to TS' seems to the right thing to do in gradually typed languages and is common practice (e.g. Python does it too). 
Using `any` is like saying "hey, TS, please suspend type checking, I know what I am doing". 
This is the antithesis of type safety, but what else can TS do and maintain JS compatibility?  

Actually, TS has a very clever solution for this, it is described in the next note.   
I view `any` as a form of type coercion or casting.  

## Casting _casting_ in a bad light 

I will use the term casting and type coercion interchangeably. TypeScript documentation also uses the term _type assertion_. I view `any` type to be in the same boat as well (an implicit type coercion).  
TS uses the `t as T` or `<T> t` syntax to cast expression `t` into type `T`, e.g. `iAmSureIsString as string`.    
(The second notation, `<T> t`, is somewhat unfortunate as it is very similar to type application and generic function declaration e.g. `const f = <T>():T` declares,  `<T>f()` casts, `f<T>()` applies. 
I recommend the `v as T` syntax to make casts clear and searchable in your code.)  
 
**Side Note on casting at large:**  Typically (and rightly) casting is considered to be a last resort, _only cast if you must_.

With more involved types it is often harder to write code that type checks.  That increases the appeal of casting or finding some other alternatives for nudging the type checker into agreeing.    
Some languages offer the ability to write a program to persuade the type checker about type equality (write actual _proof of type equality_). This is an advanced feature and is available in only a few languages (e.g. Coq, Idris, Haskell). Writing such programs is often challenging or even impossible.   
(I consider writing such proofs to be one of the highest level "type games" that a developer can play. 
It is both a challenge and fun. A great intro is [_add_blank_target TDD with Idris](https://www.manning.com/books/type-driven-development-with-idris)) 

There is an alternative to type coercion that allows programs to type check but will throw an exception when executed.  
This can be useful for interacting with the type checker when writing code. 
We have seen a TS version of this already (function `_<T>(): T`) in [Leveling Bumps](#leveling-the-bumps). 
Such programming practice is foreign to most languages but becomes very convenient when working with more involved types.   
As an FYI, this technique is analogous to using `undefined` in Haskell and PureScript (a library in PS) or holes in Idris.   
**(Side Note End)**

I can’t help but wonder how popular is (or will be) the use of type casting in TS programs.   

I will beat on _office.js_ some more. 
Here is a piece _office.js_ documentation about `Office.context.mailbox.item`:

> If you want to see IntelliSense for only a specific type or mode, **cast** this item to one of the following:  
>      `AppointmentCompose`  
>      `AppointmentRead` ...     

TS offers a neat alternative to casting.  I will explain it by _not_ following the _office.js_ documentation ;)

As I indicated already, I can interact with outlook email using `Office.context.mailbox.item`. 
However, `item` property is overloaded into several types discussed in previous note (I called them _facets_): 

The legacy code I am currently re-implementing is retrieving the email subject using `item.subject` and checking what kind of `email.subject` it is (a string, has asyc methods, etc) and using it accordingly.  It does a similar _"check before you get"_ game to retrieve `to`, `from`, `cc` and other email information. 
Such an approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one _facet_ can easily break the other _facets_. 
And you can test your heart out on all emails you can think about (we still have not figured out how to do e2e testing for office apps) and your app will still crash and burn if used with an office calendar appointment.

So what is the new TS-idiomatic way to do it?  TS has the `is` types: 

```JavaScript
//Safer type coercion, 'd is Office.MessageRead' is not a proof, hand waving is welcome
export const isMessageRead = (d: any): d is Office.MessageRead => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync === undefined
} 

export const isMessageCompose = (d: any): d is Office.MessageCompose => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync !== undefined 
} 

const doSomethingWithViewedEmail = async (item: Office.MessageRead): void => {...}
const doSomethingWithComposedEmail = async (item: Office.MessageCompose): void => {...}
```

(OK, checking `getAttachmentsAsync` is ugly, office.js could provide some nicer and more stable way to identify the exact `item` type.  This is still not bad. Let's move on.)

I can use these almost without any casting (except for correcting the type _office.js_ gave me): 

```JavaScript
//Correct type provided by office.js, replace `&`-s with `|`-s because that is what the provided runtime value is
type CorrectedItemType = Office.AppointmentCompose |  Office.AppointmentRead |  Office.MessageCompose |  Office.MessageRead

//A possibly clearer alternative is to do this:
//
//const item: unknown = Office.context?.mailbox?.item
//
const item = Office.context?.mailbox?.item as CorrectedItemType | undefined

if(isMessageRead(item)) {
  //doSomethingWithComposedEmail(item) //this will not type check!
  doSomethingWithViewedEmail(item)    
} else if (isMessageCompose(item)) {
  //doSomethingWithViewedEmail(item) //this will not type check!
  doSomethingWithComposedEmail(item)  
} else {   
  calendarEntriesAreNotSupported()
}
```

This is a really nice work TypeScript!  Simple to use, yet very useful.   

`t is T` type is one of the TypeScript [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) tools. The documentation refers to it as _type guards_ and _type predicates_.  
IMO, the idea of a middle ground between type checked safety and unsafe type coercion is brilliant.
This will probably influence other languages (e.g. here is [enhancement proposal for Python](https://www.python.org/dev/peps/pep-0647/)).

I hope the TS community develops a healthy aversion to casting.  Why would you use type checker if you keep subverting it?  I also hope that exporting functions returning type guards will become a standard practice for APIs.  
Use casting with care, or better yet use `t is T` types instead.  

And I have successfully avoided any puns on choosing wrong actors for a movie!


## Short note about the `never` type

The TS's `never` type is an interesting one.    
`never` is the proper _bottom_ (can be assigned to anything, it is empty, nothing else can be assigned to it).  
So, since we can't assign it, let's do it anyway (this code compiles):

```JavaScript
// never assigned to fully polymorphic <T>T type
const nevr : never = _()
```

It may look contradictory, but this is correct behavior. 
Recall how I implemented `_: () => T` (in [Leveling Bumps](#leveling-the-bumps) note).  I used `throw new Error` which has the `never` type.
The definitions of `_: () => T` and the above `const nevr` tell me that 

_`never` and `<T>T` are equivalent_  

`never` definition is almost redundant.  There appears to be no way allowed by TS
to define a fully polymorphic variable of type `<T>T`, I can only define it using a generic function, otherwise `never` is just `<T>T`.  
(for readers not familiar with the generics notation `<T>` but are familiar with the concept of _universal quantification_, `<T>T` is exactly it, think of `<T>` as `forall T.`)

Similar type exists in other languages as well and has similar semantics.  
`never` could be used to remove
information, e.g. recall my `Either<A,B>` definition and adjust it

```JavaScript
type SameAs<A> = Either<never,A>

const onlyA: SameAs<number> = {type: "right", content:_()} //_ is number
const impossible: SameAs<number> = {type: "left", content:_()} //_ is never
```

checking the (`_`) tells us that we cannot construct `impossible`.  I have not seen that approach used in TS yet.


## Interesting safety

TypeScript [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) provides a ton of super cool features. 
TS can effectively unify and narrow types used in a number of JS operators such as `typeof`, `===`, `==`
and apply this information to `if-else`, `switch` statements.  This post has already shown a few examples where this, almost magically, prevents placing code in a wrong branch of conditional if-else blocks.

Here are my favorites:

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

TypeScript prevents from using `===` if it can not unify on both types, e.g. unless something like `any` is used on one side of `===` or types overlap in a meaningful way (e.g. `string | number` and `Person | string` unifies on `string`, inheritance can unify the types in OO code too). This is true in general, not just inside `if-else`, but the `if-else` use is the killer app IMO.   
One cool example of `===` type safety combines type narrowing with literal types: `1 === 2` will not compile!

This is a big deal. `===` is typically used to compare things like `string` or `number` _id_-s and it is not that uncommon to accidentally try to compare something like an _id_ to something completely different.   
I have seen analogous errors in many programming languages including even _Scala_. 

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

Think of `1` and `2` as `Office.MessageCompose`-like "facets" currently offered by some API, you want to know what code needs to change when the API offers a `3`-rd facet.  Do you remember "Just one more thing" from Lieutenant Columbo?   I may just be much older than you...

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

In my very first example `getName(p: NullablePerson)` was not `undefined` safe, only `null` safe. 
Using it with `undefined` values typed as `any` will cause an error.  

My coding preference would be to rewrite my first example like this:

```JavaScript
//Reusable utility type
export type Undefined = null | undefined

//Convenience function
export const isUndefined = (d: any): d is Undefined =>
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


## Rants
### Egg or chicken rant

Think about these two distinct approaches to writing code:  

* Values / programs follow types
* Types follow programs / values

IMO this distinction is not something most programmers think about.  
In statically typed languages with powerful type systems the first approach is dominant.  Whole libraries, APIs, large parts of code bases are understood by just looking at the types.  If you squint, OO advice to _program to interfaces not implementations_ can be viewed as a special case of the first approach as well (; except squinting makes you an FP-ier, squinting is very FP ;).   
When using the first approach, you will think about, say, JSON parsing as implementing a mapping from the JSON Value type to your type.   
If you are defining your type so JS's `JSON.parse` will just work (with a type guard of course), you are using the second approach.   
When working with _office.js_ I constantly think about how to extract types from it (second approach).   
When implementing the `curry` boilerplate I am 100% in the first approach.  The same is true when I write React.js code (but my use of React is not mainstream and possibly a topic of another post).   

I imagine any gradual types will come with a decent amount of the second approach.
TS _Type guards_ fit very well in that space.   

Both approaches are IMO valid, and can and should coexist. However, moving towards the first is always a nice goal.   
IMO it is important to be aware about which approach is used and where in the program.

### The importance of return types rant

Doing exclusively FP for a while now, I have now fully transitioned my brain to thinking about inputs and outputs all the time.  
So it irks me that most of the TypeScript code I am finding on the web does not type the returns.  

This goes against common sense

> "Lenient input, Strict Output."

_Postel’s law_

Protecting the exact output type at its definition point (declaration of the function returning it) is the simplest way of enforcing some level of design sanity. 
Relying on inferred return types is unstable.  Another developer comes in and makes an adjustment. Your function now returns some unexpected union type.  Depending on how your function is used and on _tsconfig.json_ flags, this could cause compilation issues elsewhere or not.  How can you reason about input-output if your outputs are being inferred? 

Just for grins, think about 'T' in _TDD_ as 'Type'. To do that _TDD_ overload, you define not only inputs but also the return type upfront and keep fixing your code (no casting in the final product) until it compiles. OK, that would require some fancy types to fully replace _T(est)DD_ but, still, is the right way to program if you believe that types are important. 

Note, there is a linter rule to enforce this [_add_blank_target typedef](https://palantir.github.io/tslint/rules/typedef/).

### On comparative complexity of TS types rant

Adding types to a language that was not designed with types in mind must be insanely complex.  Such retrofitting has to come with glitches, corner cases and a never ending effort to resolve them.  

**Side Notes:**
Brendan Eich wrote JS in 1995 in 10 days.  TS started adding types to it in 2012. At that time, JS was a 17 year old, popular, and rowdy. For a programming language, popular implies not changeable and not fixable.   

I consider the terms _simple_ and _easy_ to have different semantics.  Easy: Low effort to learn (e.g. language).
Simple: Low effort to reason about (e.g. code written in that language). 
_Easy_ to write is not the same as _simple_ to read and understand.    
(IMO, the popularity of easy and the unpopularity of simple are a systemic problem in programming and elsewhere.)   
(**End Side Notes**)


Let's talk about simple types.  Simple for the developer and simple for the type checker.

In the [Bumps on the path](#bumps-on-the-path) note, we discussed how certain language features (e.g. overloading, tuple syntax) make
the type checker confused. I consider these to be examples of complex (the opposite of simple) types.

Pulling 4 languages (that can be used for front-end development) out of thin air:  Elm types are very simple,  PureScript types are simple but more involved,  Haskell types are simple but get more and more involved with language extensions,  TypeScript types are complex.  That is common about the comparison group is that these languages were designed from the beginning with types in mind.

We have seen in [note on `any` type](#side-note-about-the-any-type) that `any` is insanely complex.  So let's move on and look at some other types and how they are used.  
We will enroll the  `_: () => T` function, introduced in [Leveling Bumps](#leveling-the-bumps) and stolen from [_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck), to help us in this task.   


Now let's try some type puzzles!   
Try guessing the types (for each occurrence of the `_()`) before continuing.  

```JavaScript
const someBool = (): boolean => true

_() ? "foo" : "bar"           //1
someBool() ? "foo" : _()      //2
someBool() ? _() : "bar"      //3  

const s: string = someBool() ? _(): _() //4
```

As you saw, the type checker computes `unknown` in all (1,2,3).   
By comparison, for all 3 in our "placebo" group (Elm, PureScript, Haskell) an `if-else` expression would infer the `Bool` type in (1) and `String` in (2,3).  


`unknown` in (1) is the price TS pays for having JS truthiness.   
TS is a superset of JS. 
What can TypeScript do?  `unknown` seems like a reasonable choice.   
We get `unknown` in (2,3) because ternary operator 2nd and 3rd argument do not need to have the same type.  
Again, what can TypeScript possibly do about it?  `_<unknown>(): unknown` seems about right.  
The ternary operator was not designed with types in mind.   
If you check (4), you will see that TypeScript has aced it.  You get `_<string>(): string` for both parameters. 

I was somewhat surprised that the result of following puzzle  is also `unknown` (I expected
literal `1`):

```JavaScript
1 === _()
1 == _()

//remember, 1 == "boo" and 1 === "boo" will not compile!
// 1 === 2 will also not compile!

1 == ("boo" as unknown) //compiles!
```

You can try similar exercises for arithmetic operations: `+`, `*`, `/`.   
Unfortunately, TS will not allow you to use `_()` at all in these
expressions. Probably another corner case similar to not being able to compile: `curry(_())`.   
You will have more luck with overloaded `"string" + _()`.  

All these operators (ternary, `===`, `==`, arithmetic) are built in language constructs, not generic functions.
TS must handle them one by one.  That is a lot of work and complexity.   
Compare this to our "placebo" group of languages where equivalent operators are just functions, no special case treatment is needed. Also these functions have very clear, 
you guessed it, types.

Gradual typing over JS is, clearly, not a very easy thing to do, so the types have to be complex and type checking has to be somewhat buggy.  This explains the various compilation glitches in previous notes.

To us, TS users, type complexity translates to a sometimes confused type checker requiring developer
intervention when types get just a little more involved.   
For a comparison, I could write a whole application in Haskell (assuming Haskell with no language extensions) without specifying 
a type even once, all types fully inferred by the type checker.  (Not that I would really want to do that, I like defining types. It is about the type checker's ability to help me more and needing less of my help.)

I view it as a trade-off:  suffer a little because of type complexity but see your code when debugging using developer tools _vs_ introduce a language that has nicer types, compiles down to JS, and assume that you will not be using the debugger.

## Interesting types and a bit of theory

[_add_blank_target TAPL](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) is 
the book about types I recommend to everyone, ... so far unsuccessfully.   
Apparently, software developers want to have some life outside of programming. Who knew?  
The good news is that types dramatically increase programming efficiency so learning them is a good investment.   
This section of the post will be a little more TAPL-ish with some more advanced CS. 


### Structural Types

TypeScript is somewhat unique in supporting _Structural Types_. 
Types like `Person` or `NullablePerson` from the beginning of this post are structural. That means the name `Person` is simply an alias, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition. Two structurally identical classes are still considered different types (this is called _nominal typing_).    
Structural types can be more intuitive to use (if you are not thinking about the types or implementing a programming language).  IMO nominal typing could be why OO languages like Java or FP languages like Haskell are hard to learn (not the only reason but a reason, in both cases).  Structural types are also a very good fit for JS objects.  I am very excited about having them in TS.


### Subtyping

Structural types are great but each JS object structure ends up being its own type.  That's a lot of types.  
Sometimes you want to define functions that work across types.  This is where subtyping comes in. 

Structural type is a subtype (i.e. extends) another type if it is more specific. 
You can assign a subtype to the supertype.  
The thing to remember is that you can assign a more specific type to a more general type.
Not the other way around. 

Here are some examples of this happening for the union types. 
Subtyping of union types seems less commonly understood, it is dual to objects, and may seem upside down. 
Roughly speaking, you can assign a type with fewer variants to a type with more variants.   

Before continuing reading pass this code, please try to implement (at least in your head) the `amIFooOrBar` function:

```JavaScript
function verifyExtends<T1, T2 extends T1>() {}

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

verifyExtends<FooOrBarOrBuz, FooOrBar>() //compiles, FooOrBar extends FooOrBarOrBuz
//verifyExtends<FooOrBar, FooOrBarOrBuz>() //does not compile, FooOrBarOrBuz does not extend FooOrBar
```

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

verifyExtends<FooAndBar, FooAndBarAndBaz>() //compiles, FooAndBarAndBaz extends FooAndBar
//verifyExtends<FooAndBarAndBaz, FooAndBar>() //does not compile, FooAndBar does not extend FooAndBarAndBaz
```

Subtyping gets more involved if you combine adding properties to objects and variants to union types.
But I think you get the gist.

Now let's revisit the above challenge.  What will your function return in this call:

```JavaScript
// challenge check:
// what does your function return when used on this value?
// NOTE this does compile, you can assign FooAndBar to FooOrBar, since 'and' implies 'or'
const whatIsThat = amIFooOrBar({foo: "foo", bar: "bar"}) 
```

TS compiler is working as expected.  The behavior of `amIFooOrBar` on `fooAndBar` is something you may 
not have thought about when implementing it.

Personally, I try to avoid using subtyping features.  They can be very useful but IMO should not be overused. 
Subtyping is related to Object Orientation.  OO programming has an appeal of simplicity and I was seduced by it for
many years.  It took me a long time to realize that OO is not that simple. That is a long story, outside of this note's scope.
This comment should be filed under IMO as many developers disagree.  
Some front-end languages, notably PureScript (row polymorphism) and Elm (extensible records) use much more restrictive and simpler approaches that allow implementing functions that work across object-like record types.  


### Recursive Types 

`type JsonVal` from the beginning of this post surprised me. It is recursive, the name `JsonVal` appears on both the
LHS and the RHS of the definition.  Here is this definition repeated:

```JavaScript
type JsonVal = 
| {type: "object", val: Map<string, JsonVal>}
| {type: "array", val: JsonVal[]}
| {type: "string", val: string}
| {type: "number", val: number}
| {type: "bool", val: boolean}
| {type: "null"}
```

and here is the TAPLish reason why this is interesting.    
The two established approaches for implementing recursive types are 

* _iso-recursion_ (good fit for nominal types) If you know _recursion schemes_, the compilation technique is very similar to how 
the `Fix` transformation and the _recursion schemes_ work in nominally typed languages like _Scala_, _Haskell_, etc.  You kinda roll (Fix) and unroll (unFix) one layer of recursion at the time.
* _equi-recursion_ (good fit for structural types).  There is no `Fix/unFix` game. The structure is already unraveled into a potentially infinite 
beast. The compiler needs to deal with the whole beast. This approach is much harder to implement.

`JsonVal` looks like an equi-recursive definition. 
The methodology behind equi-recursion involves monotone functions and other things I never found time to understand very well.  Hard stuff and quite a bit of math.
I have not dug in deep enough to know how TS compiles `JsonVal` like types.  No matter what it does, it is IMO impressive.  

`JsonVal`-like types appear to be hard on the TS type checker.  I have played with some advanced recursive types and have experienced it first hand. I got quite a few 

> 'Type instantiation is excessively deep and possibly infinite' 

compiler errors (e.g. code in [_add_blank_target https://github.com/rpeszek/ts-typecheck-peano](https://github.com/rpeszek/ts-typecheck-peano)).  
But I did not succeed in creating a simple example to demonstrate this.

I will demonstrate something slightly different:

```JavaScript
type List<T> = 
| {type: "nil"} 
| {type: "cons", head: T, tail: List<T>}

const ul_123  = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

//const l_123: List<number> = ul_123 //compiler error: Argument of type ... is not assignable to type ...

const l_123: List<number> = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

```

That is a recursive definition of a functional _cons_ list.  `ul_123` is an equivalent of `[1,2,3]` encoded matching the `List<number>` structure.   
TS will not let me assign it to `const l_123: List<number>`.  Similarly, I would not be able to use it as a parameter to a function
that expects `(l: List<number>)`.  I had to cut-paste the RHS of `ul_123` into `l_123` for it to type check. 
This is quite different from how typical structural types behave in TS.

IMO, it is still impressive that TS is able to pull these off.  The damage is that you will sometimes need to 
help the type checker out.  I consider this feature very useful. 


### Type level programming

TS literal types are singletons (i.e. type `"boo"` has exactly one value `"boo":"boo"`).   
Singletons magically connect types with values. That way, the values can be type checked!   
Literal types should not be that hard to implement in a programming language and it is interesting why they are so uncommon.  Kudos to TS for introducing these!  They are, clearly, a great fit for JS.  

TS literal types are very limited in scope (I remember reading somewhere that it was a design decision).
For example, you can do some very basic type level string manipulation but you cannot
concatenate strings or do any arithmetic on numbers and have no way of defining any additional features on your own.  

TypeScript allows for type-level ternary (_Conditional Types_) as well as various type-level built-in functions (e.g. `keyof`).   
Apparently, the type level programming in TypeScript is _Turing Complete_
(see [_add_blank_target https://github.com/microsoft/TypeScript/issues/14833](https://github.com/microsoft/TypeScript/issues/14833)).    
However, type level programming in TS is focused on creating type safety for various
JS code idioms rather than creating a foundation for DIY type level programming. 
IMO this makes it harder to learn.  The _Turing completeness_, I think, was completely accidental.   

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

const generalizedHead = <T> (t: T) : Flatten<T> => {
    if(Array.isArray(t)) 
        return t[0]
    else 
        // return t //compiler error: Type 'T' is not assignable to type 'Flatten<T>'
        return t as any //ghrrr!
}

type HasContent<C> = {content: C}

type GetContent<T> = T extends HasContent <infer C> ? C : T

const getContent = <C, T extends HasContent<C>> (t: T): GetContent<T> => {
   //return t.content //compiler error:  Type 'C' is not assignable to type 'GetContent<T>'
   return t.content as any //ghrrr!
}
```

It feels clunky.  It feels like type level and value level have a broken marriage. 
It also feels very confusing.

I have no idea how popular the TS type level features are.  I would guess not very.  
But, I also think that they will keep improving and may end up with a decent amount of use.

## Final Thoughts 

I hope the examples I gave here will persuade some of you to explore more advanced uses of types and 
not scare you with bumpy paths and type checking bloopers. 
The message I tried to convey is that types are worth exploring, learning, and getting good at.   
I also tried to seed the idea that types could be an important design tool.  Types are a tool to reason
about the code.   

I was a bit harsh on _office.js_. Sorry! Some negativity is, unfortunately, unavoidable when discussing types.
After all, if types are about preventing design errors, then to show their effectiveness you need to show what they are preventing.
As I said before, _office.js_ predated TS and was not designed with the type safety in mind so picking too much on it was probably 
not fair.  The lesson here could be that even when using plain JS it is good to think about types.

What is the TypeScript for?  Is it an add-on that helps to prevent trivial code errors?  
Or, will TypeScript change the way we write code?  I think it is absolutely the first but it could also be the second.
TypeScript has one thing going for it that languages like _Haskell_, _OCaml_, _Scala_ do not have:

_The low barrier to entry._ 

Many will come for trivial type safety but will stay for more advanced features. 
Programming languages can impact how people write code.  Some languages have done a service to humanity making humans better coders.  Others not so much.  I think TypeScript is and will be in the first camp. 
Or maybe I am wrong about all of this.  Maybe a low barrier to entry is not a good thing.  I have not made up my mind on this.

What about developers who know their types and want to have a mainstream job where they can use their skills?  TypeScript could make a difference for these folk too.  Mainstream programming landscape seems just nicer for everyone with TypeScript being a part of it. ... I am almost sure my place will be hiring a front-end developer next year.  

TypeScript is a rich language supporting various OO features like interfaces, classes, mixins which I have not discussed. These notes have been written by a Haskeller after all. To be honest, I tried to evangelize OO for the first 10-12 years of my programming career, IMO that was long enough.  I am not trying to discourage their use (maybe just a little bit), rather I am pointing out that there are a ton of blogs about these already.  
On the topic of _interfaces_.  I notice that the community seems to prefer interfaces over type aliases. Code examples in this post did not need interfaces. It is my preference to use type aliases and structural types if that is all I need.

This post was focused on types, not so much on FP. 
If places like reddit do not level me with the ground, I may be tempted to write separate posts focusing on _React.js_ and few other mainstream front-endish topics from the FP point of view.  

I hope this was an interesting reading for you.  Thank you for staying with me all the way to the end of this monster of a post.  
