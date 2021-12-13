---
title: Type Enthusiast's Notes about TypeScript. Part1. Typing in Anger
author: Robert Peszek
featured: true
summary:  TypeScript Types series, Introduction, office.js, working with and fighting type checker
toc: true
tags: TypeScript
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
When this note disappears, you will know that I gave up.)_   

**Disclaimers:** (imagine this is a very small font, read it very fast in a half whisper)   
_The code in this post may require something like `strictNullChecks` compiler flag. 
I assume strict compiler flags are on, something you get by default with scaffolding, e.g. using
`create-react-app my-project --template typescript` is close enough.  
The code examples have been tested with TypeScript v4.4.4 and v4.5.2.  
This post is a pandoc output of a markdown document and code examples are not interactive.  
Most code examples are published in [_add_blank_target ts-notes](https://github.com/rpeszek/ts-experiments/tree/master/ts-notes) folder in this github repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)._




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


## Note about the `any` type

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
However, using _any_ type similar to TS' seems to be a common practice in gradually typed languages (e.g. Python does it too). 
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
export const isMessageRead = (d: unknown): d is Office.MessageRead => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync === undefined
} 

export const isMessageCompose = (d: unknown): d is Office.MessageCompose => {
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

I hope the TS community develops a healthy aversion to casting.  Why would you use a type checker if you keep subverting it?  I also hope that exporting functions returning type guards will become a standard practice for APIs.  
Use casting with care, or better yet use `t is T` types instead.  

And I have successfully avoided any puns on choosing wrong actors for a movie!



## TODO note about `unknown`

may need to be moved into the next section
Concerns about top and `unknown` widening. 
nice story about type guards.
If TS could travel back in time it would declare JSON.parse to return `unknown`.