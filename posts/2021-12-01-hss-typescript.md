---
title: Types Enthusiast's Notes on TypeScript
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary: TODO
toc: true
tags: TypeScript
---

This is a TypeScript post from a programmer who loves types and uses them a lot.  

I will try to stay close to idiomatic use of TypeScript but with little twists to demonstrate some interesting uses of types. 
I hope this post will be interesting to like-minded JavaScript and TypeScript developer who enjoy exploring types and try using the type checker to its full advantage. Practitioners of other strongly typed languages may find TypeScript interesting and exciting, as did I.

I will use some basic functional concepts like currying without explaining them.  I assume some familiarity with TypeScript but reader not familiar with the language should be able to guess/infer most of it. 

I am currently leading a re-write of a legacy front-end component that was originally written using Vue.js, the goal is to rewrite it using new React.js and TypeScript.  My goal in this post is to share my experience with TypeScript and my approach to using it.  
This is my first non-Haskell project in about 3 years.  I have my FP hat on when writing in TS.  TS and JS may try to punch some
holes in that hat but it still keeps me warm protects my brain.   
Also, I have not done any major JS development in the last 9 years which makes this experience even more interesting.  
I think all of this gives me a different and a fresh perspective (and a reason to write this post for others to see).  
At the same time, please to correct me if I get anything wrong.  

What is TypeScript for?  Is it just a JavaScript add-on used to prevent typos and trivial code errors?  
Or, will TypeScript more fundamentally change the way the code is written?
Please have these questions in mind when reading these notes.

## TypeScript is great!

It literally took me less than one minute of playing with TS to get excited about it.   
Just look at the union types:

```TypeScript
type Person = {firstNm: string, lastNm: string} | null

const getName = (p:Person): string => {
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

(The above code may require something like `strictNullChecks` compiler configuration, in this post 
I assume conservative configuration flags, something you get with scaffolding, e.g. a typescript react project
`create-react-app my-project --template typescript` by default.
Code examples have been tested with TypeScript v4.4.4 and v4.5.2.)

Talking about my "literal" excitement, my next play example was to check that I can implement `Either` (I am not trying to implement my own Either type, only to play with the language):

```TypeScript
type Either<A,B> = 
| {type: "left", content: A}
| {type: "right", content: B}

let x: Either<number, string> = {type: "left", content: 1}
//let wrong: Either<number, string> = {type: "left", content: "one"} // will not compile
```

it almost looks like dependent types! TS calls these literal types.  And, TS is serious about string fields too:

```TypeScript
let y: Either<number, string> = {"type": "left", "content": 1}
//let wrong: Either<number, string> = {"type": "left", "content": "one"} // will not compile
```
  
TypeScript `ts-pattern` library uses literal types to implement _pattern matching_. Exhaustive check is part of it.   
Again, really cool.

All of these are really exciting developments to me.

Continuing with play examples, here is the full JSON grammar defined in TS.

```TypeScript
type JsonVal = 
| {type: "object", val: Map<string, JsonVal>}
| {type: "array", val: JsonVal[]}
| {type: "string", val: string}
| {type: "number", val: number}
| {type: "bool", val: boolean}
| {type: "null"}

const tstj : JsonVal = {type:"array", val:[{type: "null"}, {type: "number", val: 5}]} //compiles
//const wrong : JsonVal = {type:"array", val:[{type: "number", val: {type: "string", val: "5"}}]} //does not compile, number cannot a nested string
//const wrong2 : {type: "object",  val:[{type: "null"}, {type: "number", val: 5}]} //does not compile, object is not an array

```

This could have been expressed with OO classes, but it would not be very easy, would it?  
I wrote `JsonVal` definition without thinking, I have committed `Data.Aeson.Value` (Haskell's commonly used type for JSON values) definition to memory and I just mimicked it.  Then I looked at it again ... TS supports complex recursive definitions!
Do I even trust this to work?  The answer is: mostly yes.  More on this at the end of this post.  

TypeScript provides ability to do type level programming that goes beyond the demonstrated use of literal types.  All of this is oriented toward creating type safety over various idiomatic JS code and is limited in scope but is a step forward in the use of types.  Again, more on this at the end of this post.

As far as mainstream languages go (I consider _Scala_ or _Reason ML_ not in the mainstream list), TypeScript could be the most interesting choice today IMO.  



## _office.js_.  Working with and fighting the type checker

For the rest of this post I will focus on _office.js_ library.  It is a Microsoft product (like TypeScript). 
_office.js_ comes with TypeScript type definitions.  
It is also relatively old and has been retrofitted for TS. Many other JS ecosystem libraries have gone the same route.   
It seems like a good 'comprehensive' example for examining the benefits (and frustrations) of using TS in anger.  
Despite some hardships, TS makes working with office.js much, much easier!

As the name suggests, _office.js_ provides API for working with _Microsoft Office_. 
It allows implementing applications that work inside the office suite of products (Microsoft calls these apps add-ins).  
As a working example, we will play with code that extracts data from an email opened in Outlook. 
This is not an office.js tutorial but, I hope, the code should be clear to follow. 
To start, I want to extracts the email body from the email.

To access data, _office.js_ often uses an old style `getAsync` methods that I will modernize using a custom conversion to `Promise`.  Node's `util.promisify` will not work well for this task.  This is how this can be done in TS:

```TypeScript
export const officePromise = <T> (getasync: ((fx: ((r: Office.AsyncResult<T>) => void)) => void)): Promise<T> => {
  return new Promise((resolve, reject) => {
    getasync((res: Office.AsyncResult<T>) => {
      if(res.status===Office.AsyncResultStatus.Succeeded){
        resolve(res.value)
    } else
        reject(res.error.message)
    })
 })
}

```

Side Note: Here is my first criticism of TS. What could possibly be the goal of having to name function parameters (`fx:` and `r:` in the above example) in the type definitions in a not dependently typed language?  I cannot come up with anything.  This seems like needless boilerplate and makes writing and reading the code harder.  I will show a work-around for this later in this post.

Properly initialized office add-in will have runtime access to `item: Office.MessageRead` (available statically as `Office.context.mailbox.item`). `item` allows access to elements of the email with which the add-in interacts.

To retrieve the email body I need to use `item.body.getAsync`.  But wait, the type signature for that version of `getAsync` accepts not only a callback function but also a body type parameter.

I am going to resist the temptation to overload the above `officePromise`.
Instead I will move in a direction that is more fundamental.

For now, assume that we want 'html' body format, the code can look something like this:

```TypeScript
const bodyType = Office.CoercionType.Html
 
const partiallyAppliedBodyFn = (fn: ((res: Office.AsyncResult<string>) => void)) => 
     item.body.getAsync(bodyType, fn) 
  
const body  = await officePromise<string> (partiallyAppliedBodyFn) // body: string
```

I had to type annotate (add type information to the LHS of the `const` definition) for this to work. 
That looks like a lot of typing to just partially apply `item.body.getAsync`!

### Happy path

There are some libraries that offer a `curry` function conversion, but these are typically JS not TS. So I wrote it myself (again, note the type signature is somewhat hard to read):

```TypeScript
export const curry = <T1, T2, R> (fn: (ax: T1, bx: T2) => R): (a: T1) => (b: T2) => R => {
    const res = (a: T1) => (b: T2) => fn(a, b)
    return res
 }

const addtst = (a:number, b: number) => a + b
const tst = curry(addtst)(1) //const tst: (b: number) => number
const tst2 = curry(addtst)(1)(2) //tst2 = 3
```

And I have a much simpler code that compiles right off the bat: 

```TypeScript
const body = await officePromise<string> (curry(item.body.getAsync)(bodyType)) 
```

This worked out quite well and the type checker was able to infer the types!  
This ended up being a happy path.   


### Bumps on the path

In practice, the type checker will often need some help.  Even more often the programmer will need help figuring what he or she has done wrong causing the type checker to object.

This seemingly very simple code that uses a 3 parameter overload of `item.body.getAsync`

```TypeScript
//this snipet compiles
const emptyConfig : any = {}
const body3  = await officePromise (curry3(item.body.getAsync)(bodyType)(emptyConfig)) 
```

took me quite a fight to figure out.  At some point I had this code:

```TypeScript
/* Note: the following body3 does not compile */
// const body3 : string = await officePromise<string> (curry3(item.body.getAsync)(bodyType)({}))

// So I type annotate it everywhere
const curriedBody : (ct: Office.CoercionType) => 
                    (a: any) => 
                    (fn: (x: Office.AsyncResult<Office.CoercionType>) => void) => 
                    void 
        = curry3(item.body.getAsync)

/* Note: This version of body3 will not compile */
// const body3  = await officePromise<string> (curriedBody(bodyType)({}))    

//so I annotate some more
const paritallyAppliedBody = (fn: ((res: Office.AsyncResult<string>) => void)) => curriedBody(bodyType)({})

const body3  = await officePromise<string> (paritallyAppliedBody) //finally compiles!
```

I think of such annotating work as a poor man's REPL.  

One type that is notorious for needing annotations is the TypeScript tuple.
Typescript overloads list syntax [] to define tuples (or heterogeneous lists). This is an example of a tuple type: `[number, string]`. 
The syntax overloading probably does not help TS in inferring the type and the type checker gives up or infers wrong type (the array).

Using types requires experience, knowledge, and some patience.  TS is sometimes a struggle for me, and I miss not having good tooling like a REPL (I did try _ts_node_ and _tsun_) for interacting with the type checker. But I am working in TS for only about 1 month now.  

The good news is that it gets easier with practice. Haskell type checker can be stubborn and hard to persuade sometimes, yet I am more effective in Haskell than in any other language I worked in (maybe 4-5X more effective), and I have a lot of respect for it.  I am sure TypeScript will get easier with time too!  More advanced types come with more misleading errors, it takes experience to translate the error
message to a likely underlying cause.

---xxx

I am concerned that many developers will give up when trying to write this type of code.  
My concern is also that developers will resort to unsafe type coercion / type casting. 
There will be a lot of `myvar as WhatIWant`. 


### Leveling the bumps

Cumbersome type annotations are not a good excuse to give up!  In fact there is a way to simplify the type annotations quite a bit.  For example, I can define a helper alias:

```TypeScript
//define my own reusable type for Office getAsync callbacks
export type OfficeCallack<T> = (x: Office.AsyncResult<T>) => void
```

Here is the annotated version for the previously defined `partiallyAppliedBodyFn`: 

```TypeScript
//before:
const partiallyAppliedBodyFn1 = (fn: ((res: Office.AsyncResult<string>) => void)) => item.body.getAsync(bodyType, fn) 
//after:
const partiallyAppliedBodyFn2 = (fn: OfficeCallack<string>) => item.body.getAsync(bodyType, fn)
```

Notice no more redundant parameter definitions in the type signature and much easier to read syntax. 

Here is the `curry3` example. Instead of annotating the LHS, it is sometimes more convenient
to apply types to the polymorphic function:

```TypeScript
const body3  = await officePromise<string> (curry3<Office.CoercionType, any, OfficeCallack<string>, void> (item.body.getAsync)(bodyType)) 
```

### It is all worth it at the end

I rewrote some legacy code using the techniques in this section.
That effort resulted in some 5X size reduction and overall big improvement in readability and correctness when compared
to the code I am replacing or to code in the _office.js_ documentation.  
A lot of the improvement comes from using `await` `async` syntax sugar but converting functions to their curried form and figuring out more terse ways to type annotate also results in added clarity and significant syntactic simplification. 


## Type checker bloopers 

Types in TS sometimes offer a similar frustration to trying to use functional programming in an imperative language.  
It often feels like a struggle.  It feels like trying to do something using tools not fully designed for it.  
Yet, the code size reduction and improved code clarity speak for themselves so I persevere.

All of these compile:

```
const good : (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (item.body.getAsync)

const nonsense1 : (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (curry (item.body.getAsync)) 

const nonsense2 = (curry(curry)) 
```

and all, except the first one, should not type check.  I admit have I opened _ghci_ (Haskell _REPL_) to double check that the last one is wrong. 

Trying to use `nonsense1` or `nonsense2` in actual program created weird results for me, things did not work but no errors were reported in the console or anywhere.  



## Can I trust the types?

Compiler issues aside, I want to talk about the most obvious gotcha.  

I am going back to my `Person` example. This will be a good conversation starter:

```
const p : Person = JSON.parse("John Smith")
```

That will not work well, will it?  You will get errors caused by `undefined` at the time you try to use, say, `p.firstNm: string`.

Look at the top rated answer in this stackoverflow: 
https://stackoverflow.com/questions/38688822/how-to-parse-json-string-in-typescript
It appears that what I just wrote is the user community's preferred way of parsing JSON.   
Yes, better ways are available (look at less popular answers to the same stackoverflow).

Now, let me return to the _office.js_ example.    

I have hard-coded the use of 'http' as the email body type.  
Is there a way to retrieve what is the underlying body type in the viewed email?    
_office.js_ documentation, and the IntelliSense claims that I can get it from `item.body.getTypeAsync`:

> (method) Office.Body.getTypeAsync(options: Office.AsyncContextOptions, callback?: ((asyncResult: Office.AsyncResult<Office.CoercionType>) => void) | undefined): void (+1 overload)
> Gets a value that indicates whether the content is in HTML or text format.

(There is an overloaded version without `options` parameter as well)

When I tried to use it, this property was always `undefined`.  
Seems like _office.js_ types sometimes lie.

We should look at the type definition of the _office.js_ `item` a little closer.

The `item` property is overloaded to be one of these types (let me call them _facets_):

```
Office.AppointmentCompose (for working with calendar)
Office.AppointmentRead  (for working with calendar)
Office.MessageCompose  (for interacting with a new email being composed)
Office.MessageRead  (for interacting with received or sent email being viewed by the user)  
```

These _facet_ types are all different.  For example, to get email subject you use `item.subject: string` if you are working with `Office.MessageRead` or `item.subject: Office.Subject` if you are working with `Office.MessageCompose` (`Office.Subject` contains `getAsync`, `setAsync` methods).  

The type of `item` it is not, as I would expect: `AppointmentCompose | AppointmentRead | MessageCompose | MessageRead`.
Rather it is closer to (I have not listed all the `&`): `AppointmentCompose & AppointmentRead & MessageCompose & MessageRead`.  

Basically the type chosen by _office.js_ combines all the available properties, methods, overloads into one type. 
This is simply an incorrect type for `item`.  Runtime values do not satisfy that type, the satisfy the union type.
As the result, you program is very likely to type check when using a property or method that does not exist
on the runtime value and your program will experience runtime failures.

None of this should be used to blame TypeScript or to be used as an argument against it. 
None of this should be a surprise.  
Less trustworthy types are a limitation we have to accept if we want gradual typing over JS. 

Rather, it is on the TypeScript developers (including API developers) to be extra diligent in making sure that the types match the values.   
What are the types good for if you cannot trust that they are accurate?


## Casting _casting_ in a bad light 

**(Rant Alert)**  I can't help but wonder about what is (or will be) the popularity of using casts in TS programs. 
Most (if not all) of programming languages (languages which have types and have practical use) allow programmers to brute force the type definition whatever the underlying value is. 
This is called type coercion in some languages and casting in others.  The actual use of casting is quite popular in some statically typed languages (e.g. Java) and very rare in others.  
The reasons could be both historical and pragmatic.  

One of my favorites, Idris, documents `believe_me` and `really_believe_me` as

> Use it with care - it can result in segfaults or worse!

`unsafeCoerce` in Haskell has similar dangers and is rarely used.

Here is a piece documentation from _office.js_ about `Office.context.mailbox.item`:

> If you want to see IntelliSense for only a specific type or mode, cast this item to one of the following:
>    AppointmentCompose
>    AppointmentRead ...

**(Rant Alert End)**

TS offers a neat alternative to casting.  I will explain it by _not_ following the _office.js_ documentation ;)

As I indicated already, _office.js_ allows me to interact with outlook email using `Office.context.mailbox.item`.  
However `item` property is overloaded into several types discussed in previous section (let me call them _facets_): 

The legacy code I am currently re-implementing is retrieving `subject` by checking what kind of `subject` is defined in the `item` object and using it accordingly, it does a similar _"check before you get"_ game to retrieve `to`, `from`, `cc` and other email information.    
Such approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one _facet_ can easily break the other _facets_. 
And you can test your heart out on all emails you can think about (I still have not figure out how to do e2e testing for office apps well) and your app will crash and burned if used with a calendar appointment.

So what is the new TypeScript-idiomatic way to do it?  TS has the `is` types: 

```TypeScript
export const isMessageRead = (d:any) : d is Office.MessageRead => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync === undefined
} 

export const isMessageCompose = (d:any) : d is Office.MessageCompose => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync !== undefined 
} 

export const doSomethingWithViewedEmail = async (item: Office.MessageRead) : void => {...}
export const doSomethingWithComposedEmail = async (item: Office.MessageCompose) : void => {...}
```

And I can use these almost without any casting (`item` needs to be widened to `any` because of the way _office.js_ has unfortunately typed it): 

```TypeScript
//Unfortunatelly a corrective cast of "item" is needed. 
//this has to do with office.js incorrectly using "&" instead of "|" in type definition of the "Office.context.mailbox.item" itself
type CorrectedItemType = Office.AppointmentCompose |  Office.AppointmentRead |  Office.MessageCompose |  Office.MessageRead


const item : any = Office.context?.mailbox?.item as CorrectedItemType | undefined

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

This is really nice of TypeScript!

Ideally the office.js would provide `isMessageRead`, `isMessageCompose`, etc. as part of the API. 
This is not a criticism. `office.js` API is old and TS integration has been added to it late.   
(Granted, checking `getAttachmentsAsync` is ugly, office could provide some nicer and more stable way to identify the exact `item` type.)
The point is that such type safety is not hard to accomplish on your own! 

I hope the TS community develops a healthy aversion to casting.  Why would you use type checker if you keep subverting it?


## Big kudos for making `===` type safe!

This JavaScript code:

```JavaScript
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
function blah(lhs: string, rhs: Person) {
  //Do somethign else
}
```

TypeScript prevents from using `===` if it can not unify on both types, e.g. unless something like `any` is used on one sides of `===` (or, class inheritance can unify the types in OO code).

This is a big deal, `===` is typically used to compare things like `string` or `number` `id`s and it is not that uncommon to accidentally try to compare something like an `id` to something completely different.  

I have seen analogous errors in many programming languages including even _Scala_.  
To me this huge and a big reason to avoid using `any`!


## Type Safe `if-else`

This post has shown a few examples of TypeScript magically preventing from placing code
in a wrong branch of conditional `if-else` blocks. 

However, `if-else if-else` construction can be awkward to use.  It was designed for mutating things and in today's more immutable approach to programming it should feel antiquated.  
The problem with `if-else` is that it does not return anything.

Contrast this with ternary operator, it returns.  But ternary syntax is designed for simple one-liners only.

The wildest kludge I have seen for this is (we want `const` not `let`):

```TypeScript
const x = (() => {
  if(p) {
    const res = 1 //first multiline computation
    return res
  } else {
    const res = 2 //second multiline computation
    return res
  }
})()
```

At the same time, the code we end up writing today uses callbacks and very often returns a TS `void`,  so the imperative `if-else` construction is almost good enough.  Sometimes problems have a way to work themselves out, magically.  I find that fascinating.  

But there is something still very much lacking in the type safety department. 
`if-else if-else` do not provide any mechanism for the type checker to verify that the program checked all possible conditions. E.g.

```TypeScript
export const contrived = (n: 1 | 2) : number => {
    if(n === 1) 
       return 1;
    else if (n === 2)
      return 2;
    else 
      return 3; // contrived will not compile if I comment the final else

}
```

(Think of `1` and `2` as 2 `Office.MessageCompose`-like "facets" currently offered by an API, you want to know what code needs to change when the API offers a `3`-rd facet)

Interestingly TS uses the `switch` statement to solve this problem:

```TypeScript
//This compiles!
const contrived_better = (n: 1 | 2) : number => {
    switch(n) {
       case 1:
        return n
       case 2:
        return n 
    } 
}
```

That is a nice example of useful addition of types.

Even better solution is a library solution, not a language solution.  TypeScript `ts-pattern` library has you covered!
I am not going to steal the `ts-pattern` thunder and refer to the library documentation and this blog post: 
https://dev.to/gvergnaud/bringing-pattern-matching-to-typescript-introducing-ts-pattern-v3-0-o1k


## TypeScript, The Interesting Types

The book about types I recommend to everyone I know, and I do not know anyone else who read it, is [TAPL](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages).  Apparently, software developers want to have some life outside of programming. Who knew?  
The good news, again, is that types dramatically increase programming efficiency so learning them is a good investment.
In addition to reading, a good thing to do is to play with types to see what you can do with them.  For example, this code https://github.com/rpeszek/ts-typecheck-peano pushes TS compiler to its limit.  

TypeScript is somewhat unique in supporting what is called _Structural Types_. 
_Structural Types_ are a very natural fit for JS objects.
Types like `Person` from the beginning of this post are structural. That means the name `Person` is simply an alias, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition. Two structurally identical classes are still considered different types (this is called _nominal typing_).    
Structural types are much more intuitive.  IMO nominal typing could be why OO languages like Java or FP languages like Haskell 
are hard to learn (not the only reason but a reason, in both cases).  

The recursive type example `type JsonVal` surprised me.  Here it the [TAPLish]() reason why. 
Programming language theory established two approaches for implementing recursive types _iso-recursion_ (good fit for nominal types, the compilation technique is very similar to how _recursion schemes_ and `data Fix` work in Haskell) and _equi-recursion_ (good fit for structural types).   
`JsonVal` looks like an equi-recursive definition, effectively an infinite structure.  These are much harder to implement. 
The methodology behind equi-recursion involves monotone functions and other things I never found time to understand very well.  Hard stuff and quite a bit of math.
I have not digged in deep enough to know that TS uses equi-recursion.  No matter what it does is IMO impressive.  

`JsonVal`-like types appear to be hard on the TS type checker.  I have played with some advanced recursive types and have experienced it first hand. I got quite a few 'Type instantiation is excessively deep and possibly infinite' 
(e.g. code in https://github.com/rpeszek/ts-typecheck-peano).  But I did not succeed in creating a simple example to demonstrate this.

I will demonstrate something slightly different:

```TypeScript
type List<T> = 
| {type: "nil"} 
| {type: "cons", head: T, tail: List<T>}

const ul_123  = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

//const l_123  :List<number> = ul_123 //compiler error: Argument of type ... is not assignable to type ...

const l_123 :List<number> = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

```

That is a recursive definition of the functional _cons_ list.  `ul_123` is an equivalent of `[1,2,3]` encoded matching the structure. 
Compiler will not let me assign it to `const l_123 :List<number>`.  Similarly, I would not be able to use it as a parameter to a function
that expects `(l : List<number>)`.  I had to cut-paste the LHS of `ul_123` into `l_123` for `l_123` to type check.
This is quite different than typical structural types in TS.

IMO, it is still impressive that TS is able to pull these off.  The damage with these recursive types is that you will need to 
do some type coercion to help the type checker out.  Small price to pay!

Literal types are very emaciated (I remember reading somewhere that is was a design decision) and look like a very thinned down `GHC.TypeLits` in Haskell.
They do not offer any DIY extensiblility. For example, you can do some very basic type level string manipulation, you cannot
concatenate strings or add numbers.  

TypeScript allows for type-level ternary as well as various type level built-in functions (e.g. `keyof`).   
Apparently, the type level programming in TypeScript is _Turing complete_
(see https://github.com/microsoft/TypeScript/issues/14833).    
That means, that at least syntactically, types in TypeScript are very powerful and can express a lot.   

Type level programming in TS is oriented toward creating type safety for various
JS code idioms rather than than creating a foundation for DIY type level programming.  
IMO this makes it harder to learn.  The _Turing completeness_, I think, is completely accidental.   

IMO the best new language design direction would be to move towards a dependently typed semantics in which type level 
and value level code is the same (e.g. Idris).
If it is not the same than at least very similar (e.g. Haskell).  
TS cannot and should not do that.  We do not want JavaScript on the type level!

At the same time the lack of synergy between type level and value level programs makes things 
very complicated to program. E.g.:

```TypeScript
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

const getContent = <C, T extends HasContent<C>> (t: T) : GetContent<T> => {
   //return t.content //compiler error:  Type 'C' is not assignable to type 'GetContent<T>'
   return t.content as any //ghrrr!
}
```

It feels clunky.  It feels like type level and value level have a broken marriage.
It also feels very confusing.



## Final Thoughts 

What is the TypeScript for?  Is it just an add-on that helps to prevent trivial code errors?  
Or, will TypeScript change the way we write code?  This is for its practitioners to decide.
I hope the examples I gave here will persuade some of you to explore more advanced use of types and to more dramatically change how you write the code.

Programming language can impact how people write code.  Some languages have really done a service to humanity making humans better 
coders.  Some not so much.  I think TypeScript is and will be in the first category.  

TypeScript is a reach language supporting various OO features like interfaces and classes which I have not discussed.  These notes have been written by a Haskeller after all.  To be honest, I considered myself OO evangelist during the first 10-12 years of my professional programming, that was long enough. 

This post was focused on types, not that much on FP. 
If places like reddit do not level me with the ground after this post, I may be tempted to write separate posts focusing on React.js and few other mainstream front-endish topics that are FP related.  
