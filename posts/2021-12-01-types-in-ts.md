---
title: Types Enthusiast's Notes on Using TypeScript in Anger
author: Robert Peszek
lastmodified: Jul 04, 2021
featured: true
summary:  Long post or a tiny book about advanced types in TypeScript 
toc: true
tags: TypeScript
codestyle: ts
---
**Subtitle: _A long post or a tiny book about advanced types in TypeScript_**

This is a TypeScript post from a programmer who loves types and uses them a lot.  

I will try to stay close to an idiomatic use of TypeScript but with little twists to demonstrate some interesting uses of types. 
I hope this post will be interesting to like-minded JavaScript and TypeScript developer who enjoy exploring types and try using the type checker to its full advantage. Practitioners of other strongly typed languages may find TypeScript interesting and exciting, as did I.

I will use some basic functional concepts like currying without explaining them.  I assume some familiarity with TypeScript but reader not familiar with the language should be able to guess/infer what the example code does. 
There is only one prerequisite to reading this: interest in types.

I am currently leading a re-write of a legacy front-end component that was originally written using Vue.js, the goal is to rewrite it using the new React.js and TypeScript.  My goal in this post is to share my experience with TypeScript and my approach to using it.  
This is my first non-Haskell project in 3 years.  I have my FP hat on when writing TS.  TS and JS may try to punch some
holes in that hat but it still keeps me warm and protected.  

Also, I have not done any major JS development in the last 9 years which makes this experience even more interesting. (Please make sure to correct me if I get anything wrong.) 
I think all of this gives me a different and a fresh perspective and a reason to write this post for others to see.  For some readers parts of this post will feel strange, established practices like overloading will be considered a bad thing, writing experimental code that won't even run will be a good thing.  Strange is a corollary of different.
 
What is TypeScript for?  Is it just a JavaScript add-on used to prevent typos and trivial code errors?  
Or, will TypeScript more fundamentally change the way the code is written?
Please have these questions in mind when reading these notes.

This is a long post, probably the longest I ever wrote.  It will cover a lot of topics.   
You should make yourself a large coffee
... or better yet pickup a water bottle to stay hydrated.

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

**Disclaimers:**   
_The above code may require something like `strictNullChecks` compiler flag. In this post 
I assume strict compiler flags are on, something you get by default with scaffolding, e.g. using
`create-react-app my-project --template typescript` is close enough.  
The code examples have been tested with TypeScript v4.4.4, and partially with v4.5.2.  
This post is a pandoc output of a markdown document and code examples are not interactive.
All examples (except for examples using office.js) are published in ts-notes folder in this repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)   
A lot of interaction with TS happens using IntelliSense, I have used vscode v1.62.3 (2021-11-17T08:00:36.721Z), Linux x64 5.4.0-90-generic snap._

Talking about my "literal" excitement, my next play example implements `Either` (I am not trying to implement my own Either type, only to play with the language):

```JavaScript
type Either<A,B> = 
| {type: "left", content: A}
| {type: "right", content: B}

let x: Either<number, string> = {type: "left", content: 1}
//let wrong: Either<number, string> = {type: "left", content: "one"} // will not compile
```

it almost looks like dependent types! TS calls these literal types.  And, TS is serious about string field names too:

```JavaScript
let y: Either<number, string> = {"type": "left", "content": 1}
//let wrong: Either<number, string> = {"type": "left", "content": "one"} // will not compile
```
  
TypeScript `ts-pattern` library uses literal types to implement _pattern matching_. Exhaustive check is part of it.   
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

const tstj : JsonVal = {type:"array", val:[{type: "null"}, {type: "number", val: 5}]} //compiles
//const wrong : JsonVal = {type:"array", val:[{type: "number", val: {type: "string", val: "5"}}]} //does not compile, number cannot a nested string
//const wrong2 : {type: "object",  val:[{type: "null"}, {type: "number", val: 5}]} //does not compile, object is not an array

```

This could have been expressed with OO classes, but it would not be very easy, would it?  
I wrote the `JsonVal` definition without thinking, I have committed `Data.Aeson.Value` (Haskell's commonly used type for JSON values) definition to memory and I just mimicked it.  Then I looked at it again ... holly ... TS supports complex recursive definitions!
More on this at the end of this post.  

TypeScript provides ability to do type level programming that goes beyond the demonstrated uses of literal types.  All of this is oriented toward creating type safety over various kinds of idiomatic JS code and is limited in scope.  Again, more on this at the end of this post.

As far as mainstream languages go (I consider _Scala_ or _Reason ML_ border line just outside the mainstream), TypeScript could be the most interesting choice today IMO.  

This was my trailer section. If the code that excited me feels interesting to you, you may enjoy reading on.  There will be some gory details (not a lot violence), some triumphs and failures.  You have to decide if type safety is your genre. 



## _office.js_.  Using TS in anger

I will use _office.js_ library as a source of examples for this post.  It is a Microsoft product (like TypeScript). 
_office.js_ comes with TypeScript type definitions. 
It is also relatively old and has been retrofitted for TS. Many other JS ecosystem libraries have traveled a similar JS to TS path. 
It seems like a good 'comprehensive' example for examining the benefits (and frustrations) of using TS in anger.   
Despite some hardships, TS makes working with office.js much, much easier!

As the name suggests, _office.js_ provides API for working with _Microsoft Office_. 
It allows implementing custom apps that work inside the office suite of products (Microsoft calls these apps add-ins).  
As a working example, we will play with code that extracts data from an email opened in Outlook.   
This is not an office.js tutorial but, I hope, the code should be clear to follow. 

To start, I want to extracts the email body from the email.  
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
The declaration syntax overloads the meaning of both `:` and `=>`.  Function form `A` to `B` is (depending where in the declaration) either `(a: A) => B` or `(a: A): B`; (`:`) starts declaration; (`=>`) ends declaration.  This is hard to read, confusing to write, it makes my brain hurt.  This syntax does not scale well to more involved types or helps in reasoning about types.   
I will show a work-around for this later in this post as well. _(Side Note End)_

Properly initialized office add-in will have access to `item: Office.MessageRead` (available statically as `Office.context.mailbox.item`). `item` allows access to elements of the email with which the add-in interacts.  
To retrieve the email body I need to use `item.body.getAsync`.  But wait, the type for that version of `getAsync` accepts not only a callback function but also a "body type" parameter.

I am going to resist the temptation to overload the above `officePromise`.
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


```JavaScript
const emptyConfig : Office.AsyncContextOptions = {}
//body3 does not compile: "Type ... is not assignable to type ..." 
//const body3  = await officePromise (curry3(item.body.getAsync)(Office.CoercionType.Html)(emptyConfig)) 
```

Interestingly the following does compile but with `body3:unknown`, why why why!

```JavaScript
//this snipet compiles but uses 'any'
const emptyConfig : any = {}
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

The issue in this particular case is that, not only `item.body.getAsync` has two overloads, but the one I want is accepting union type argument and the callback is optional:

```JavaScript
(method) Office.Body.getAsync(coercionType: string | Office.CoercionType
  , options: Office.AsyncContextOptions
  , callback?: ((asyncResult: Office.AsyncResult<string>) => void) | undefined): void 
```

So there are really _overloads within overloads_ and the type checker gets confused. 
The type checker appears to have a problem with backtracking when analyzing branching overloads.  (I do not blame TS, and I get a headache too.)
Overloading is an issue with type inference in general (e.g. the reason why Haskell does not overload names).  

_If you are an API owner, my advice is to not overload._

One type that is notorious for needing annotations is the TypeScript's _tuple_.
Typescript overloads array syntax `[]` to define tuples (you may prefer the term heterogeneous lists). This is an example of a tuple type: `[number, string]`. 
The syntax overloading probably does not help TS in inferring the type and the type checker gives up or infers the wrong type (the array). 

I am concerned that **many developers will give up** trying to write this type of code. 
My concern is also that developers will resort to unsafe type coercion / type casting. 
There will be a lot of `myvar as IWantIt`, or a lot of the `any` type.

Was this enough gore for you?  You say it was not?  I say you did not see the body content of that email!

### Leveling the bumps

Cumbersome type annotations are not a good excuse to give up!  There is a way to simplify function type definitions.  For example, I can define a helper alias:

```JavaScript
//DIY reusable type for Office getAsync callbacks
export type OfficeCallack<T> = (x: Office.AsyncResult<T>) => void
```

Here is how this simplifies the previously defined `partiallyAppliedBodyFn`: 

```JavaScript
//before:
const partiallyAppliedBodyFn1 = (fn: ((res: Office.AsyncResult<string>) => void)) => item.body.getAsync(Office.CoercionType.Html, fn) 
//after:
const partiallyAppliedBodyFn2 = (fn: OfficeCallack<string>) => item.body.getAsync(Office.CoercionType.Html, fn)
```

Notice **no more redundant argument definitions** in the type signature and a much easier to read syntax.  
This version is **my personal preference** (it separates the type and the implementation nicely):

```JavaScript
const partiallyAppliedBodyFn3 : ((_: OfficeCallack<string>) => void) = fn => item.body.getAsync(Office.CoercionType.Html, fn)
```

Returning to my failed `body3` example, instead of trying to specify the full type signatures, it is sometimes more convenient to apply the types.  Here, I have the polymorphic `curry3` function that I can apply the types to:

```JavaScript
//type applied version, it just compiles!
const emptyConfig : Office.AsyncContextOptions = {}
const body3  = await officePromise<string> (
  curry3<Office.CoercionType, Office.AsyncContextOptions, OfficeCallack<string>, void> 
     (item.body.getAsync)
     (Office.CoercionType.Html)
     (emptyConfig)
  ) 
```

This DIY type hole technique is sometimes useful to help figure out stubborn types: [_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck).  Sometimes the only thing you learn is that the type checker is confused. 
As an example, returning to my failed `body3` example:

```JavaScript
declare function _<T>(): T
const body3  = await officePromise (curry3 (item.body.getAsync)(Office.CoercionType.Html)(_())) 
```

If you hover over the `_` function, the IntelliSense suggests this completely wrong type for `_`:

```JavaScript
(alias) _<((asyncResult: Office.AsyncResult<string>) => void) | undefined>(): ((asyncResult: Office.AsyncResult<string>) => void) | undefined
```

If I add the type application as above (`<Office.CoercionType, Office.AsyncContextOptions, OfficeCallack<string>, void>`) to `curry3`
it will show the type correctly:

```JavaScript
(alias) _<Office.AsyncContextOptions>(): Office.AsyncContextOptions
```

Sadly, the ` _<T>(): T` is also not universally useful, e.g. this will not compile:

```TypeScript
//const test = curry(_()) //compilation error 

//interestingly the following unifies as <unknown, unknown, unknown>curry
const test = curry({} as any)
```

Using types requires some experience, knowledge, and patience. 
More advanced types come with more misleading error messages, it takes experience to find the underlying cause of a misleading compilation error, that is true in any language.   

I am mostly left to my own devices when working with more involved types in TS. 
Hopefully the future will bring us mainstream grade interactive tools that allow asking type questions, browsing types,
help solving type puzzles. For now it is mostly the programmer who connects the dots.  
The good news is that it gets easier and easier with practice. I am working in TS for only about 2 months now and I already see a difference.   

### It's all worth it

I rewrote some legacy code using the techniques in this section.
That effort resulted in significant size reduction and an overall big improvement in readability and correctness when compared
to the code I was replacing or to code in the _office.js_ documentation.  
A lot of the improvement comes from using `await` `async` syntax sugar but converting functions to their curried form and figuring out more terse ways to type annotate also results in added clarity and significant syntactic simplification. 

If you think that section felt too much like a very soapy soap, I agree!  More gore coming up! 

## Type checking bloopers 

This section shows examples of code that compiles but clearly should not.  I will discuss some "correct" programs that should compile but do not in the [Recursive Types](#recursive-types) section.

All of these type check:

```JavaScript
//annotated correct code added for reference, this code compiles
const good : (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (item.body.getAsync)

//compiles but it should not, compiles even with type anotation
const nonsense1 : (a: Office.CoercionType) 
          => (b: ((asyncResult: Office.AsyncResult<string>) => void)) 
          => void
    = curry (curry (item.body.getAsync)) 

//compiles but it should not
const nonsense2 = curry(curry)
```

and all, except the first one, should not.  
I expect the type checker to be effective at rejecting nonsensical code.  It it happens, it should be rare, contrived code, unlikely for developers to write.  My examples are somewhat surprising since higher order functions are not uncommon in JavaScript.  The second example is a piece of code I accidentally wrote in my project. It took some effort to figure out why my program does not work.  

I think these are examples of growing pains. TS type checker will fix these in the future. The programmer has to be prepared for a little fight with TS and I also has to accept that sometimes TS is wrong. 

And more gore coming right up.

## Can I trust the types?

Compiler issues aside, I want to talk about the most obvious gotcha. 
I am going back to my `Person` example. JSON incompatibility issues are a real maintenance challenge. This will be a good conversation starter:

```JavaScript
//Questionable JSON parsing example
const p : Person = JSON.parse('"John Smith"')
```

The actual run-time `p` value will be a `p:string`, while the type checker is now convinced it is a `p:Person`.   
`JSON.parse` function is declared to return `any` type.  Using `any` bypasses type checking. `any` can be assigned to any type, here it is assigned to `Person`.  

Now, look at the top rated answer in this stackoverflow: 
[_add_blank_target how-to-parse-json-string-in-typescript](https://stackoverflow.com/questions/38688822/how-to-parse-json-string-in-typescript). 
It appears that the above code matches the user community's preferred way of parsing JSON. 
Yes, better ways are available (look at less popular answers to the same stackoverflow, I will discuss
TS type guards in [Casting Section](#casting-casting-in-a-bad-light)).  

Now, since I already may have angered a large part of the TS community (did I? I hope not.), **let me return to _office.js_**  to beat on it a little more.   
In my [happy path example](#happy-path), I have hard-coded the use of 'http' as the email body type.  Both _office.js_ documentation and the IntelliSense tells me that I can retrieve underlying email body type using an overloaded `item.body.getTypeAsync` method:

> (method) `Office.Body.getTypeAsync` ...
> Gets a value that indicates whether the content is in HTML or text format.

When I tried to use it, this property was always `undefined`, I never seen it using the developer tools either.
A version incompatibility? I do not think it is, I am using online office and the latest available office.js (@types/office-js@1.0.221).
 
_Seems like _office.js_ types sometimes lie._

We should look at the type definition of the _office.js_ `Office.context.mailbox.item` a little closer.  
This property is overloaded to be one of the following types (let me call them _facets_):


> `Office.AppointmentCompose` (for editing calendar entry)   
> `Office.AppointmentRead`  (for viewing calendar entry)   
> `Office.MessageCompose`  (for editing new email)   
> `Office.MessageRead`  (for interacting with received or sent email being viewed by the user)  


These _facet_ types are all different.  For example, to get email subject you use `item.subject:string` if you are working with `Office.MessageRead` or `item.subject:Office.Subject` if you are working with `Office.MessageCompose`.   
`Office.Subject` contains `getAsync`, `setAsync` methods and is completely not a `string`.  

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

None of the examples shown in this section should be used to blame TypeScript. 
None of them should be a surprise. 
Less trustworthy types are a limitation we have to accept if we want gradual typing over the wild-west JS. 

Rather, it is on the TypeScript developers (including API developers) to be extra diligent in making sure that the types match the values.   
What are the types good for if you cannot trust that they are accurate?

> "You take the blue pill—the story ends, you wake up in your bed and believe whatever you want to believe.   
> You take the red pill—you stay in Wonderland, and I show you how deep the rabbit hole goes"

_Morpheus about not trusting gradual typing_


**Side Note about the `any` type:**  `any` type is a little crazy.  It is, what is often called a _top_ (you can assign any other type to it). It is also what is called a _bottom_ (it can be assigned to any other type).  I have not seen anything like this
anywhere outside of TypeScript. Typically languages separate their tops from their bottoms ;).   
Using `any` is like saying "hey, TS, please suspend type checking, I know what I am doing". 
This is the antithesis of type safety, but what else can TS do and maintain JS compatibility?  Actually, TS has a very clever solution for this, it is described in the next section.   
I view `any` as a form of unsafe type coercion.  

## Casting _casting_ in a bad light 

I will use the term casting and type coercion interchangeably. TypeScript documentation also uses the term _type assertion_. I view the `any` type to be in the same boat as well.  
 
**Side Note on casting at large:**  The actual popularity of casting differs from language to language.
In some languages type coercion is dangerous and cause segfaults or worse. This discourages its use. 
In interpreted languages, like TS, a wrong run-time value type will only cause an exception. This is less scary and could encourage the use of casting.

With more involved types it is often harder to write code that type checks.  That increases the appeal of casting or finding some ingenious alternatives for nudging the type checker into agreeing.    

Some languages offer ability to write a program to persuade the type checker about type equality (write actual _proof of type equality_). This is an advanced feature and is available in only few languages (e.g. Coq, Idris, Haskell). Writing such programs is often challenging or even impossible.   
(I consider writing such proofs to be one of the highest level type games that a developer can play. 
It is both a challenge and fun. A great intro is [_add_blank_target TDD with Idris](https://www.manning.com/books/type-driven-development-with-idris)) 

There is an alternative to type coercion that allows programs to type check but will throw exception when executed.  
This can be useful for interacting with the type checker when writing code.   
We have seen a TS version of this already (`function _<T>(): T`). 
(the ref again: [_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck).)   
The polymorphic (generic) `_(): T` is typically much better than using something like `{} as any`. TS does not narrow types when `any` is used.   
Such programming practice is foreign to most languages but becomes very convenient when working with more involved types.   
(FYI) This technique is analogous to using `undefined` in Haskell and PureScript (a library in PS) or holes in Idris.   
**(Side Note End)**

I can’t help but wonder how popular is (or will be) the use of type casting in TS programs.   

I will beat on _office.js_ some more. 
Here is a piece _office.js_ documentation about `Office.context.mailbox.item`:

> If you want to see IntelliSense for only a specific type or mode, **cast** this item to one of the following:  
>      `AppointmentCompose`  
>      `AppointmentRead` ...     

TS offers a neat alternative to casting.  I will explain it by _not_ following the _office.js_ documentation ;)

As I indicated already, I can interact with outlook email using `Office.context.mailbox.item`. 
However, `item` property is overloaded into several types discussed in previous section (I called them _facets_): 

The legacy code I am currently re-implementing is retrieving `subject` by checking what kind of `subject` it is and using it accordingly, it does a similar _"check before you get"_ game to retrieve `to`, `from`, `cc` and other email information. 
Such approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one _facet_ can easily break the other _facets_. 
And you can test your heart out on all emails you can think about (I still have not figured out how to do e2e testing for office apps) and your app will still crash and burn if used with an office calendar appointment.

So what is the new TS-idiomatic way to do it?  TS has the `is` types: 

```JavaScript
//Safer type coercion, 'd is Office.MessageRead' is not a proof, hand waving is welcome
export const isMessageRead = (d:any) : d is Office.MessageRead => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync === undefined
} 

export const isMessageCompose = (d:any) : d is Office.MessageCompose => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync !== undefined 
} 

const doSomethingWithViewedEmail = async (item: Office.MessageRead) : void => {...}
const doSomethingWithComposedEmail = async (item: Office.MessageCompose) : void => {...}
```

(OK, checking `getAttachmentsAsync` is ugly, office could provide some nicer and more stable way to identify the exact `item` type.  This is still not bad. Let's move on.)

I can use these almost without any casting (except for correcting the type _office.js_ gave me): 

```JavaScript
type CorrectedItemType = Office.AppointmentCompose |  Office.AppointmentRead |  Office.MessageCompose |  Office.MessageRead

//A possibly clearer alternative is to do this:
//
//const item : unknown = Office.context?.mailbox?.item
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

I hope the TS community develops a healthy aversion to casting.  Why would you use type checker if you keep subverting it?  I also hope that exporting functions returning `t is T` will become a standard practice for APIs.  
Use casting with care, or better yet use `t is T` types instead.  

And I have successfully avoided any puns on choosing wrong actors for a movie!


## Interesting safety

TypeScript [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) provides a ton of super cool features. 
TS can effectively unify and narrow types used in number of JS operators such as `typeof`, `===`, `==`
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

TypeScript from using `===` if it can not unify on both types, e.g. unless something like `any` is used on one side of `===` or types overlap in a meaningful way (e.g. `string | number` and `Person | string` unifies on `string`, class inheritance can unify the types in OO code too). This is true in general, not just inside `if-else`, but the `if-else` use is the killer app IMO.

This is a big deal. `===` is typically used to compare things like `string` or `number` _id_-s and it is not that uncommon to accidentally try to compare something like an _id_ to something completely different.   
I have seen analogous errors in many programming languages including even _Scala_. 

### `switch` exhaustive check

There is something still lacking in the type safety department. 
`if-else` does not provide any mechanism for the type checker to verify that the program checked all possible conditions. E.g.

```JavaScript
const contrived = (n: 1 | 2) : number => {
    if(n === 1) 
       return 1
    else if (n === 2)
      return 2
    else 
      return 100  // contrived will not compile if I comment the final else

}
```

Think of `1` and `2` as `Office.MessageCompose`-like "facets" currently offered by some API, you want to know what code needs to change when the API offers a `3`-rd facet.  Do you also remember "Just one more thing" from Columbo?  I maybe just much older than you...

Interestingly, TS uses the `switch` statement to solve this problem:

```JavaScript
//This compiles!
const contrived_better = (n: 1 | 2) : number => {
    switch(n) {
       case 1:
        return 1
       case 2:
        return 2 
    } 
}
```

That is another nice example of TS enhancing JS with a nice type safety feature.

IMO even better solution is a library (_ts-pattern_) solution, not a language solution.  
I am not going to steal the `ts-pattern` thunder and refer to the library documentation and this blog post: 
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

TypeScript also provides `?` syntax for declaring object fields. E.g.

```JavaScript
type Person2 = {firstNm: string, middleNm?: string, lastNm: string}
```

The type of `person.middleNm` is `string | undefined`.  My preference is to declare optional fields using the above `Undefined` type:

```JavaScript
middleNm? : string | Undefined
```

## The importance of return types rant

Doing exclusively FP for a while now, I have now fully transitioned my brain to thinking about inputs and outputs all the time.  
So it irks me that most of the TypeScript code I am finding on the web does not type the returns.  

This goes against common sense

> "Lenient input, Strict Output."

_Postel’s law_

Protecting the exact output type at its definition point (declaration of the function returning it) is the simplest way of enforcing some level of design sanity. 
Relying on inferred return types is unstable.  Another developer comes in and makes an adjustment. Your function now returns some unexpected union type.  Depending on how your function is used, this could cause compilation issues elsewhere (not good) or not (even worse).
How can you reason about input-output if you really on outputs being inferred? 

Just for grins, think about 'T' in _TDD_ as 'Type'. To do that _TDD_ overload, you define the return type upfront and keep fixing your code (no casting in the final product) until it compiles. OK, that would require some fancy types to fully replace _T(est)DD_ but, still, is a better way to do programming in a statically typed environment. 

## The Interesting Types

[_add_blank_target TAPL](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) is 
the book about types I recommend to everyone I meet, ... so far unsuccessfully. 
(And I should stop doing it when shopping in the supermarket.) 
Apparently, software developers want to have some life outside of programming. Who knew?  
The good news is that types dramatically increase programming efficiency so learning them is a good investment.   
This section of the post will be little more TAPL-ish. 

### Structural Types

TypeScript is somewhat unique in supporting _Structural Types_. 
Types like `Person` or `NullablePerson` from the beginning of this post are structural. That means the name `Person` is simply an alias, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition. Two structurally identical classes are still considered different types (this is called _nominal typing_).    
Structural types are much more intuitive to use.  IMO nominal typing could be why OO languages like Java or FP languages like Haskell 
are hard to learn (not the only reason but a reason, in both cases).  Structural types are also a very good fit for JS objects.

### Recursive Types 

`type JsonVal` from the beginning of this post surprised me.  Here it the TAPLish reason why.  
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

//const l_123  :List<number> = ul_123 //compiler error: Argument of type ... is not assignable to type ...

const l_123 :List<number> = {type: "cons", head: 1, tail: {type: "cons", head: 2, tail: {type: "cons", head: 3, tail: {type: "nil"}}}}

```

That is a recursive definition of a functional _cons_ list.  `ul_123` is an equivalent of `[1,2,3]` encoded matching the `List<number>` structure.   
TS will not let me assign it to `const l_123 :List<number>`.  Similarly, I would not be able to use it as a parameter to a function
that expects `(l : List<number>)`.  I had to cut-paste the RHS of `ul_123` into `l_123` for it to type check. 
This is quite different from how typical structural types in behave in TS.

IMO, it is still impressive that TS is able to pull these off.  The damage is that you will sometimes need to 
help the type checker out.  I consider this feature very useful. 


### Type level programming

TS literal types as singletons (i.e. type `"boo"` has exactly one value `"boo":"boo"`).  
Singletons allow to magically connect type expression with values.  They should not be that hard to implement
and it is interesting why they are so uncommon.  Kudos to TS for introducing these!  They are, clearly, a great fit for JS.  

TS literal types are very limited in scope (I remember reading somewhere that is was a design decision).
For example, you can do some very basic type level string manipulation but you cannot
concatenate strings or do any arithmetic on numbers and have no way of defining any additional features on your own.  

TypeScript allows for type-level ternary (_Conditional Types_) as well as various type level built-in functions (e.g. `keyof`).   
Apparently, the type level programming in TypeScript is _Turing complete_
(see [_add_blank_target https://github.com/microsoft/TypeScript/issues/14833](https://github.com/microsoft/TypeScript/issues/14833)).    
That means that semantically types in TypeScript are very powerful.   
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

const getContent = <C, T extends HasContent<C>> (t: T) : GetContent<T> => {
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
not scare you with bumpy path and type checking blooper examples. 
The message I tried to convey is that types are worth exploring, learning, and getting good at.   

I was a bit harsh on _office.js_. Sorry! Some negativity is, unfortunately, unavoidable when discussing types.
After all, if types are about preventing errors, then to show their effectiveness you need to show what they are preventing.
As I said before, _office.js_ predated TS and was not designed with type safety in mind so picking too much on it was probably 
not fair.

What is the TypeScript for?  Is it an add-on that helps to prevent trivial code errors?  
Or, will TypeScript change the way we write code?  I think is it absolutely the first but it could also be the second.
TypeScript has one thing going for it that languages like _Haskell_, _OCaml_, _Scala_ do not have:

_The low barrier to entry._ 

Many will come for trivial type safety but will stay for more advanced features. 
Programming languages can impact how people write code.  Some languages have done a service to humanity making humans better coders.  Others not so much.  I think TypeScript is and will be in the first category. 
Or maybe I am wrong about all of this.  Maybe a low barrier to entry is not a good thing.  I have not made up my mind on this.

What about developers who know their types and want to have a mainstream job where they can use their skills?  TypeScript could make a difference for these folk too.  Mainstream programming landscape seems just nicer for everyone with TypeScript being a part of it. ... I am almost sure my place will be hiring a front-end developer next year.  

TypeScript is a reach language supporting various OO features like interfaces and classes which I have not discussed. These notes have been written by a Haskeller after all. To be honest, I tried to evangelize OO for the first 10-12 years of my programming career, IMO that was long enough. 

This post was focused on types, not so much on FP. 
If places like reddit do not level me with the ground, I may be tempted to write separate posts focusing on _React.js_ and few other mainstream front-endish topics from the FP point of view.  

I hope this was an interesting reading for you.  Thank you for staying with me all the way to the end of this post.