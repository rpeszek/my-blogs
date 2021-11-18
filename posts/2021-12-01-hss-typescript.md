---
title: Haskeller's Notes on TypeScript
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary: TODO
toc: true
tags: TypeScript
---

My goal in this post is to focus on types in TypeScript and examine what is nice and what is not so nice from the view point of a functional programmer using the language.  

I will try to stay close to idiomatic use of TypeScript but with some twists to demonstrate some interesting uses of types. 
I hope this post will be interesting to like-minded JavaScript and TypeScript developer who enjoy exploring types and try using the type checker to its full advantage. Haskellers and other developers who love types may find TypeScript interesting and exciting, as did I.

I will use some basic functional concepts like currying without explaining them.  I assume some familiarity with TypeScript but reader not familiar with the language should be able to guess/infer most of it. I will not use any Haskell code. 

I am currently leading a re-write of a legacy front-end component that was originally written using Vue.js, the goal is to rewrite it using new React.js and TypeScript.  
My goal in this post is to share my experience with TypeScript and my approach to using it.  
This is my first non-Haskell project in about 3 years.  I have my FP hat on when writing TS.  
Also, I have not done any major JS development in the last 9 years which makes this experience even more interesting.  
I think all of this gives me a different and fresh perspective (and a reason to write this for others to see).  
Some call it a bad muscle memory, my muscles have already forgotten! (I hope this statement does not offend anyone.
It seemed to ring true in my career.)
At the same time, please to correct me if I get anything wrong.  

What is TypeScript for?  Is it just a JavaScript add-on used to prevent trivial code errors?  
Or, will TypeScript more fundamentally change the way JavaScript code is written?
Please have these questions in mind when reading these notes.



## TypeScript is great!

It literally took me less than one minute of playing with TS to get excited about it.   
Just look at the union types  
(this may require something like `strictNullChecks` compiler configuration, in this post 
I assume conservative configuration flags, something you get by default with scaffolding, e.g. a typescript react project
`create-react-app my-project --template typescript`):

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

Talking about my "literal" excitement, my next play example was to check that I can implement `Either` (I am not trying to implement my own Either type, only to play with the language):

```TypeScript
type Either<A,B> = 
| {type: "left", content: A}
| {type: "right", content: B}

let x: Either<number, string> = {type: "left", content: 1}
//let wrong: Either<number, string> = {type: "left", content: "one"} // will not compile
```

it almost looks like dependent types! TS calls these literal types.  And, TS is serious about strings too:

```TypeScript
let y: Either<number, string> = {"type": "left", "content": 1}
//let wrong: Either<number, string> = {"type": "left", "content": "one"} // will not compile
```

TypeScript uses literal string types to create type safety over the use of DOM (e.g. DOM events).  This is brilliant!  I am sure literal types will many other uses.  
`ts-pattern` library uses literal types to implement _pattern matching_. Exhaustive check is part of it.   
Again, really cool.

All of these are really exciting developments to me.

Continuing with play examples, here is the full JSON grammar defined in TS.

```TypeScript
export type JsonVal = 
| {type: "object", val: Map<string, JsonVal>}
| {type: "array", val: JsonVal[]}
| {type: "string", val: string}
| {type: "number", val: number}
| {type: "bool", val: boolean}
| {type: "null"}

export const tstj : JsonVal = {type:"array", val:[{type: "null"}, {type: "number", val: 5}]} //compiles
//const wrong : JsonVal = {type:"array", val:[{type: "number", val: {type: "string", val: "5"}}]} //does not compile, number cannot a nested string
//const wrong2 : {type: "object",  val:[{type: "null"}, {type: "number", val: 5}]} //does not compile, object is not an array

```

This could have been expressed with OO classes, but it would not be very easy, would it?  

If you like me and love types, you may have played with some dependently typed code.  Some cool
examples are xxx.

As far as mainstream languages go (I consider _Scala_, _Reason ML_ not in the mainstream list), TypeScript could be the most interesting choice today IMO.  



## _office.js_.  Working with and fighting the type checker

For the rest of this post I will focus on _office.js_ library.  It is a Microsoft product (like TypeScript). 
_office.js_ comes with TypeScript type definitions.  
It is also relatively old and like a lot of other JS ecosystem libraries that TS programs end up needing to use have been retrofitted to TS.  
It seems like a good 'comprehensive' example to examine the benefits and some of the frustrations of using TS in anger.  
Despite some hardships, TS makes working with office.js much, much easier!

As the name suggests, _office.js_ provides API for working with Microsoft Office. 
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
Instead I will move in a direction that will give us a stronger foundation.

For now, assume that we want 'html' body format, the code can look something like this:

```TypeScript
const bodyType = Office.CoercionType.Html
 
const partiallyAppliedBodyFn = (fn: ((res: Office.AsyncResult<string>) => void)) => 
     item.body.getAsync(bodyType, fn) 
  
const body  = await officePromise<string> (partiallyAppliedBodyFn) // body: string
```

I had to type annotate (TS calls these annotations) this to work (give `partiallyAppliedBodyFn` a type).  That looks like a lot of typing to just partially apply `item.body.getAsync`!

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


### Bumpy path

In practice, the type checker will often need some help.  
In other times the issue is a incorrect program that type checker rightfully rejects, but it is not clear why. 
Adding type annotations can help if figuring to see where the compilation breaks.  
I think of this as a poor man's REPL.  

Here are some of the thing that, in my experience, can sometimes confuse TS type checker
and require extra type annotations:

* name overloading (this is a classic reason for a language to have problem inferring types)
* functions as parameters (this is very unfortunate)
* tuples (typescript overloads list syntax [] to define tuples - heterogeneous lists, e.g. [number, string]). Note, this is a form of syntax overloading and could be a part of the reason why type inference suffers.

To demonstrate a more bumpy path, I will use a 3 parameter overload of `item.body.getAsync` which accepts additional `any` parameter in the second position  
(I am not including `curry3` boilerplate code, it is easy to do and very similar to the above `curry` function, it converts a 3 parameter function to its curried form)

To have this one compile I had to add a lot of type signatures:

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

But this was not necessary. The only thing the type-checker needed was to spell out the type of the new parameter:


```TypeScript
const body3  = await officePromise (curry3(item.body.getAsync)(bodyType)({} as any)) //compiles with no annotations!
```


This is not a very happy path indeed and I am concerned that many developers will give up when trying to write this type of code.  
It is definitely not easy for me, and I miss not having good tooling, like a REPL, for interacting with the type checker.  
My concern is also that developers will resort to unsafe type coercion / type casting. 
There will be a lot of `myvar as WhatIWant`. Yet another concern is that some developers will consider the above code examples too complicated and cumbersome to use.  

What is also interesting is that the above code is sort-a equivalent to this one line:

```
const body3  = await officePromise (curry3(item.body.getAsync)(bodyType)({} as any)) //compiles with no annotations!
```

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

and, finally, here is the `curry3` example in just one line:

```TypeScript
//this compiles!
const body3  = await officePromise<string> (curry3<Office.CoercionType, any, OfficeCallack<string>, void> (item.body.getAsync)(bodyType)) 
```

Notice no more redundant parameter definitions in the type signature and much easier to read syntax. 

I rewrote some legacy code using the above techniques.
That resulted in some 5X size reduction and overall big improvement in readability and correctness when compared
to the code I am replacing or to code in the _office.js_ documentation.  
A lot of the improvement comes from using `await` `async` syntax sugar but converting functions to their curried form and figuring out more terse ways to type annotate also result in a significant syntactic simplification. 


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
Similarly, the types tell me that I have `item.body.setTypeAsync`, which is also not there (and probably should not be with a read only email access).  
Seems like _office.js_ types sometimes lie.

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
However `item` property is overloaded and can be one of the following type definitions (let me call them _facets_): 

```
Office.AppointmentCompose (for working with calendar)
Office.AppointmentRead  (for working with calendar)
Office.MessageCompose  (for interacting with a new email being composed)
Office.MessageRead  (for interacting with received or sent email being viewed by the user)  
```

These _facet_ types are all different.  For example, to get email subject you use `item.subject: string` if you are working with `Office.MessageRead` or `item.subject: Office.Subject` if you are working with `Office.MessageCompose` (`Office.Subject` contains `getAsync`, `setAsync` methods).  
The legacy code I am currently re-implementing is retrieving `subject` by checking what kind of `subject` is defined in the `item` object and using it accordingly, it does a similar _"check before you get"_ game to retrieve `to`, `from`, `cc` and other email information.    
Such approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one _facet_ can easily break the other _facets_. 
(... and stable e2e tests for MS office, ...I am still not sure how to do these.)

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
//Unfortunatelly downcasting / widening "const item : any" is needed for type safety, 
//this probably has to do with office.js using "&" instead of "|" in type definition of the "Office.context.mailbox.item" itself
//so it looks to me that office.js `Office.context.mailbox.item` type is not defined right
const item : any = Office.context?.mailbox?.item

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

This is really nice!

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

```
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
`if-else if-else` do not provide any mechanism for the type checker to verify that the program checked all possible conditions.

`switch` statement does not do that either.  

`contrived` can only accept `1` or `2` as its argument

```TypeScript
export const contrived = (n: 1 | 2) : number => {
    if(n === 1) {
       return 1;
    } else if (n === 2) {
        return 2;
    } else {
        return 3; // contrived will not compile if I comment this line
    }
}
```

but I have to add the catch-all `else` and return a default value from it even though I covered all the possibilities in the `if-if else`. 

As a less contrived example, recall the four `item` _facets_ types offered by _office.js_:
`Office.AppointmentCompose`, `Office.AppointmentRead`, `Office.MessageCompose`, and 
`Office.MessageRead` and assume you wrote 200K lines of code that use them viraly. 

Your app proudly handles all 4. Microsoft came up with new version of Outlook that also does _Notes_. New _office.js_ has not changed except for adding 2 new _facets_.  
What do you do to check all the places where the new _Office.NoteCompose_ and _Office.NoteRead_ are not handled?

Amazingly the solution for this exists in TypeScript but it is a library solution, not a language solution.  TypeScript `ts-pattern` library has you covered!
Hmm _office.js_ does not use `ts-pattern` so you would need to do some extra work yourself but you could do that!

I am not going to steal the `ts-pattern` thunder and refer to the library documentation and this blog post: xxx

## Final Thoughts 

What is the TypeScript for?  Is it just an add-on that helps to prevent trivial code errors?  
Or, will TypeScript change the way we write code?  This is for its practitioners to decide.
I hope the examples I gave here will persuade some of you to explore more advanced use of types and to more dramatically change how you write the code.

TypeScript is somewhat unique in supporting what a programming language theorists call structural types.  
The above examples: `Person`, `JsonVal` are structural, that means the names `Person` / `JasonValue` are simply aliases, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition. Two structurally identical classes are still considered different types (this is called nominal typing).    
Structural types are harder to implement, in particular for recursive types.
I was pleasantly surprised that TypeScript allowed me to code the recursive `JsonVal` example.  
There are very few languages that support structural types and I am very excited about TypeScript being one of them.

I am not going to try to list things TypeScript types cannot do.  That would be a ridiculously long list.  
Instead, I can try to guess what will be the most missed feature for FP-iers:
The higher kinded types (HKT) with ability to do some ad hoc polymorphism (e.g. typeclasses).   
I remember this one to be, for a very long time, the most requested feature in F# and I think will be just that, a feature request, forever. 
There is a light-weight HKT workaround that FP-iers have used in non-FP languages.  This work-around is described for TypeScript here:
https://www.thesoftwaresimpleton.com/blog/2018/04/14/higher-kinded-types

In nutshell, TS can define types like `MyList<T>` were `T` is a type variable, e.g. `MyList<string>`. 
It cannot define `<T>string`, or more generally `F<T>` where both `F` and `T` are type variables, one abstracting over things like `MyList`, `Option`, `Queue`, the other over things like `number`, `boolean`, `string`, `Person`.  
Being able to do that is really useful, especially if you can also do some ad-hoc polymorphism (e.g. typeclasses) with it.  
TypeScript FP libraries, like _fp-ts_ are not able to take full advantage of polymorphism available in more functional languages 
like _Scala_, _Haskell_, _Purescript_, ...  
Higher Kinded Types are also needed for many other cool concepts like recursion schemes, effect systems, lots of cool stuff.  

Nuances of implementing gradual types on top of JS are well beyond my level of understanding.  My intuition tells me that HKT could (theoretically) be implementable but typeclass like polymorphism could not. 
Ad-hoc polymorphism depends on a runtime dispatch mechanism and OO class-like dispatch (which in TS/JS is just a property lookup) will not be compatible with it.
TS is basically JS with types and hence its limitation.  

TypeScript is a reach language supporting various OO features like interfaces and classes which I have not discussed.  These have been Haskeller's notes after all.
  

### Ideas for a future post

This post focused on types more than it did on FP.  These two areas, however, are often very intertwined.

It is very interesting to observe how some parts of the JS ecosystem are getting more immutable and more functional.  
New version of React.js is a great example of a change in this direction.  
It is also interesting to observe where the language and the ecosystem adapts to accommodate these changes and where it refuses to do so.  

One very interesting example is the imperative if-else which makes it awkward to work with immutable data but is what JS and TS offers for conditional code.   
At the same time, the code we end up writing today very often returns a TS `void`,  so the imperative `if-else` construction is almost (no exhaustivity check) good enough.  Sometimes problems have a way to work themselves out, magically.  I find that fascinating.  
... and I ended up using the imperative if-else a lot in this post.

Modern React.js has so many interesting design topics to discuss: 
referential transparency in component design, optics for working with component model state ...    

If places like reddit do not level me with the ground after this post, I may be tempted to write separate posts focusing on React.js and few other mainstream front-endish topics that are FP related.  
