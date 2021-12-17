---
title: Type Enthusiast's Notes about TypeScript. Part1. Typing Honestly
author: Robert Peszek
featured: true
summary:  TypeScript Types series, types and values that do not match, type guards, `any`, and `unknown`
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
office.js examples are based on https://appsforoffice.microsoft.com/lib/1.1/hosted/office.js and @types/office-js@1.0.221 
(something you get with current office scaffold for React).   
This post is a pandoc output of a markdown document and code examples are not interactive.  
Most code examples are published in [_add_blank_target ts-notes](https://github.com/rpeszek/ts-experiments/tree/master/ts-notes) folder in this github repo: [_add_blank_target ts-experiments](https://github.com/rpeszek/ts-experiments)._


## Nutshell

This is the second post in the series. We will cover TS's type guards, the notorious `any`, and its safer cousin the `unknown`.  
These are well known and heavily blogged topics.  My goal is provide a little different perspective.  
I will start by examining some very practical examples and will end with just a little of theoretical stuff and a bit of rant.  
My main code example is something I am excited about. It demonstrates a case where TS made me completely rethink a previously written JS code.  
This post will include discussion of some safety concerns about `unknown` (no, this is not a typo, I mean `unknown`)
and will start setting the stage for my future note about the complexity of TS types.

## Can I trust the types?

I am going to discuss the obvious gotcha in a gradually typed language like TS: runtime values do not satisfy statically defined types.  
Despite it being an obvious concern, the issue is not something a developer who spends most time in a statically typed language (e.g. me) will have on his / her mind when working in TS.  
The following seem to be the prevalent reasons for why values do not match types: overconfident TS code (e.g. type casting, `any` type), issues with converted JavaScript (declaration files out of sync or containing otherwise incorrect definitions).
I am going to show a real life (or close to real life) example of each.

Previous post started with an example defining the `Person` type, to avoid jumping back and forth I will repeat it here 

```JavaScript
type Person = {firstNm: string, lastNm: string} 
```

This will be a good conversation starter:

```JavaScript
//Questionable JSON parsing example
const p: Person = JSON.parse('"John Smith"')
```

Your experience with consistency of JSON data may be different than mine.  I rarely see JSON issues in front-end -- back-end conversation. On the other hand, my experience with using 3rd party REST APIs is not exactly stellar.
JSON data problems do happen.

The above code illustrates what I used to call 'fail late' and now I call 'a type I cannot trust' case. It is a nasty situation where runtime errors are nowhere near the actual problem.  Looking at the example, `JSON.parse` function is declared to return the TS's notorious `any` type.  Using `any` bypasses type checking and the value can be assigned to any other type, here it is assigned to `Person`.  The actual run-time value of `p` will be a `string`, while the type checker is now convinced it is `p:Person`.  

Now, look at the top rated answer in this stackoverflow: 
[_add_blank_target how-to-parse-json-string-in-typescript](https://stackoverflow.com/questions/38688822/how-to-parse-json-string-in-typescript). 
It appears that the above code matches the top rated answer.
Yes, safer approaches are available (look at less popular answers to the same stackoverflow, we will discuss a much safer way as well).   

Now, since I already may have angered a large part of the TS community (did I? I hope not.), let's beat a little on **_office.js_**.   

**Short Recap**
_office.js_ is a source of code examples for my series. 
We are using it to interact with Outlook emails. In last post, we have implemented  `officePromise` promisifier (a term I invented) and the `curry` boilerplate functions. This allowed us to retrieve email body with a one-liner:

```JavaScript
const body = await officePromise (curry(item.body.getAsync)(Office.CoercionType.Html)) 
```

where `item: Office.MessageRead` is provided to us by _office.js_ (via `Office.context.mailbox.item`)   
**(Recap End)**

I imagine it is not that uncommon for a TS library to have a not nullable property that is not there at runtime.  
The IntelliSense tell me that I can use the overloaded [_add_blank_target `item.body.getTypeAsync`](https://docs.microsoft.com/en-us/javascript/api/outlook/office.body?view=outlook-js-preview#getTypeAsync_callback_) method. 

```JavaScript
(method) Office.Body.getTypeAsync(options: Office.AsyncContextOptions, callback?: ((asyncResult: Office.AsyncResult<Office.CoercionType>) => void) | undefined): void (+1 overload)
```

The online documentation suggests that this is only available in the "Compose" mode but the types tells me it is in `Office.MessageRead` which is the "Read" mode. 
When I tried to use it, this property was never there. I am using office online and the latest available office.js (listed on the top of this post).  It looks to me like the TS declaration files are not in sync with JavaScript.
I have not done much of forensics work on this, please message me in git discussions if you know what is going on.  

_Seems like _office.js_ types are off._

We should look at the type definition of the _office.js_ [_add_blank_target `Office.context.mailbox.item`](https://docs.microsoft.com/en-us/javascript/api/outlook/office.item?view=outlook-js-preview) a little closer.  
This property is overloaded to be one of the following types (let me call them _facets_):


> `Office.AppointmentCompose` (for editing calendar entry)   
> `Office.AppointmentRead`  (for viewing calendar entry)   
> `Office.MessageCompose`  (for editing new email)   
> `Office.MessageRead`  (for interacting with received or sent email being viewed by the user)  


These _facet_ types are all different.  For example, to get email subject you use `item.subject:string` if you are working with `Office.MessageRead` or `item.subject:Office.Subject` if you are working with `Office.MessageCompose`.   
`Office.Subject` contains `getAsync`, `setAsync` methods and is absolutely not a `string`.  

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

Basically, the type _office.js_ chose for `item` mashes all the available properties, methods, overloads into one type. 
This is simply an incorrect type for the `item` property.  Runtime values do not satisfy the _intersection_ type, they satisfy the _union_ type.
Type checked programs will fail at runtime. _office.js_ declaration files are incorrect.

_office.js types are off for sure._


Gradual typing over the wild-west JS has to have maintenance challenges.  
Nonetheless this surprising.  What are the types good for if they're not accurate?

> "You take the blue pill — the story ends, you wake up in your bed and believe whatever you want to believe.   
> You take the red pill — you stay in Wonderland, and I show you how deep the rabbit hole goes"

_Morpheus about not believing types in a gradually typed language_    
_... nightmares of JavaScript running on my walls and ceilings make me wake up screaming_


## Note about the `any` type

My first example used the infamous `any` type.  Let's have a closer look.   
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
Sky and your creativity are the limits.  (I will spoil this party and say that I do not recommend doing it.  Oh, maybe just a little.)  

Bottom type that is not empty will cause language to be unsound. Allowing all values in a bottom type, I would call it insane.   
However, using _any_ type similar to TS' seems to be a common practice in gradually typed languages (e.g. Python does it too).  
Using `any` is like saying "hey, TS, please suspend type checking, I know what I am doing". 
This is the antithesis of type safety, but what else can TS do and maintain JS compatibility?  

Actually, TS has a very clever solution for this, it is described in the next section.   
I view `any` as a form of type coercion or casting.  



## Casting _casting_ in a bad light 

I will use the term casting and type coercion interchangeably. TypeScript documentation also uses the term _type assertion_. I view `any` type to be in the same boat as well (an implicit type coercion).  
TS uses the `t as T` or `<T> t` syntax to cast expression `t` into type `T`, e.g. `iAmSureIsString as string`.    
(The second notation, `<T> t`, is somewhat unfortunate as it is very similar to type application and generic function declaration e.g. `const f = <T>():T` declares,  `<T>f()` casts, `f<T>()` applies. 
I recommend the `v as T` syntax to make casting more explicit and searchable in your code.)  
 
**Side Note on casting at large:**  
Typically (and rightly) casting is considered to be a last resort, _only cast if you must_.

With more involved types it is often harder to write code that type checks.  That increases the appeal of casting or finding some other alternatives for nudging the type checker into agreeing.    
Some languages offer the ability to write a program to persuade the type checker about type equality (write actual _proof of type equality_). This is an advanced feature and is available in only a few languages (e.g. Coq, Idris, Haskell). Writing such programs is often challenging or even impossible. 
(I consider writing such proofs to be one of the highest level "type games" that a developer can play. 
It is both a challenge and fun. A great intro is [_add_blank_target TDD with Idris](https://www.manning.com/books/type-driven-development-with-idris)) 

There is an alternative to type coercion that allows programs to type check but will throw an exception when executed.  
This can be useful for interacting with the type checker when writing code. 
We have seen a TS version of this already (function `_<T>(): T`) defined in my previous post and stolen from 
[_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck). 
Such programming practice is foreign to most languages but becomes very convenient when working with more involved types.   
**(Side Note End)**

I can’t help but wonder how popular is (or will be) the use of type casting in TS programs.   

Let's beat on _office.js_ some more. 
Here is a piece _office.js_ documentation about (you guessed it, this post is so very predictable) the
`Office.context.mailbox.item`:

> If you want to see IntelliSense for only a specific type or mode, **cast** this item to one of the following:  
>      `AppointmentCompose`  
>      `AppointmentRead` ...     

TS offers a neat alternative to casting.  I will explain it by _not_ following the _office.js_ documentation ;)

As I indicated already, I can interact with outlook email using `Office.context.mailbox.item`. 
However, `item` property is overloaded into several types discussed in the previous section (I called them _facets_): 

The legacy code I am currently re-implementing at work is retrieving the email subject using `item.subject` and checking what kind of `email.subject` it is (a string, has asyc methods, etc) and using it accordingly.  It does a similar _"check before you use"_ game to retrieve `to`, `from`, `cc` and other email information.  
Such an approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one _facet_ can easily break the other _facets_. 
And you can test your heart out on all emails you can think about (we still have not figured out how to do e2e testing for office apps) and your app will still crash and burn if used with an office calendar appointment.

So what is the new TS-idiomatic way to do it?  TS has the `is` types.

### Type guards

```JavaScript
export const isMessageRead = (d: any): d is Office.MessageRead => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync === undefined
} 

export const isMessageCompose = (d: any): d is Office.MessageCompose => {
  return (d.itemType === Office.MailboxEnums.ItemType.Message) && d.getAttachmentsAsync !== undefined 
} 

const doSomethingWithViewedEmail = (item: Office.MessageRead): void => {...}
const doSomethingWithComposedEmail = (item: Office.MessageCompose): void => {...}
```

(OK, checking `getAttachmentsAsync` is ugly, office.js could provide some nicer and more stable way to identify the exact `item` type.  This is still not bad. Let's move on.)

`doSomethingWithViewedEmail` and `doSomethingWithComposedEmail` can now be coded with confidence following the corresponding `MessageRead` or `MessageCompose` types.  IntelliSense makes writing these a breeze and the code is very clean.
E.g., `subject` is just a `string` in `MessageRead`.

I can use these without any casting (except for correcting the type _office.js_ gave me): 

```JavaScript
//'unknown' replaces incorrect office.js type(see pervious section)
const item: unknown = Office.context?.mailbox?.item

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

This is a really nice work, bravo TypeScript!  Simple to use, yet very useful.   

It is also IMO a very interesting case of TS making a bigger impact of how we actually code. 
_"Check before you use"_  game becomes type assisted and happens on a larger scale of `item` types instead of
single (e.g. _email subject_, _email cc_, etc.) properties. This adds a lot of clarity to the code.  
TS types not just check my code, types change how I code!


`t is T` type is one of the TypeScript [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) tools. The documentation refers to it as _type guards_ and _type predicates_.  
IMO, the idea of a middle ground between type checked safety and unsafe type coercion is brilliant.   
It is something that sits half way between a cast and type equality proof, again, simply brilliant.  
This will probably influence other languages (e.g. here is [enhancement proposal for Python](https://www.python.org/dev/peps/pep-0647/)).

The syntax `t is T` is interesting, it clearly borrows from dependently typed languages.  The value `t` appears next to the type `T` and comes from the earlier part of the declaration.  This also justifies a bit the existence of otherwise cumbersome parameter names in the type definition (something I complained about in my previous post).

I hope the TS community develops a healthy aversion to casting.  Why would you use a type checker if you keep subverting it?  I also hope that exporting functions returning type guards will become a standard practice for APIs.  


**Use of `any` in type guards**  
Arguably, a safer approach was for me to define `isMessageRead` and `isMessageCompose` using a parameter type more restrictive than `any`.   
As we discussed in the previous section, the `item` type provided by _office.js_ is incorrect.  So to keep this example simple I decided to widen `item` to `unknown` as if _office.js_ did not provide me with any type information about it.   
Using `any` in type guards appears to be a common practice. Implementing a type guard typically requires checking existence of object properties and `any` gives access to these.   
My suggestion is avoiding type guards in certain places, e.g. in generics. We want generics to be generic.
More on this at the end of this post.


## Note about the `unknown` type

This post started with unsafe use of `JSON.parse`.  I am quite sure that if only TypeScript could travel back in time
`JSON.parse` would return `uknown` instead of `any`. 

```JavaScript
export const safeParseJSON : (_: string) => unknown = JSON.parse

const isPerson = (p: any): p is Person => 
        typeof p.firstNm === 'string' && typeof p.lastNm === 'string'

const possiblyPerson = safeParseJSON('"John Smith"') 

if (isPerson(possiblyPerson)) {
    console.log(possiblyPerson.firstNm)
} else {
    // console.log(possiblyPerson.firstNm) //does not compile
}
```

`unknown` is a newer and a safer alternative to `any`. 

`unknown` type is (only) the top type (you can assign anything to it but you cannot assign it to anything else, maybe except for `any`).  This is a much better safety than being both the _top_ and the _bottom_.
Compared to `any` it is more cumbersome to use but significantly safer.  

A rough view IMO of what the type safety is: an ability to _separate apples from oranges_. 
If you can assign both an apple to `unknown` and an orange to `unknown` then they are no longer separated.   
What makes this worse in TS, is its occasional tendency to widen return types to `unknown`. TS tends to do that 
if it cannot find a more precise return type and sometimes when it is confused. 
We saw two examples of this in the last post:

```JavaScript
//compilation bug allows this incorrect code to compile with
// emailBody4: unknown
//this code will accutally work at runtime because 'crazyConfig' ends up not being used 
const crazyConfig : (_: Office.AsyncResult<string>) => void = x => ""
const emailBody4 = await officePromise (curry3(item.body.getAsync)(Office.CoercionType.Html)(crazyConfig)) 

//test: (a: unknown) => (b: unknown) => unknown
const test = curry({} as any)
```

and we will encounter a few more in future notes.

I really love the fact that this code does not compile:

```Java
//Compilation error:
//This condition will always return 'false' since the types 'string' and 'number' have no overlap.
"some email body" === 1 
```

However, this does:

```JavaScript
("some email body" as unknown) === 1
```

and so does this:

```JavaScript
emailBody4 === 1
```

Let's bring in the type hole  `_: <T>() => T` from the last post.
The type hole is a convenient way to ask the compiler type questions. 

```JavaScript
//hovering over res and _ allows me to see the typing of '===`
const res = _() === _()
```

So the type signature of `===` is:

```JavaScript
declare function eqeqeq(a: unknown, b: unknown): boolean
```

Except

```JavaScript
eqeqeq("some text", 1) //compiles
```
```JavaScript
"some text" === 1 //does not compile
```

In fact, `===` _does not have a type_.  It is a built-in JS operator.  TS applies semantic narrowing rules to the code that uses it. This complex approach is needed to provide type safety and achieve compatibility with JS at the same time.   
TS's semantic rules prevent something like `someText === someNumber` from compiling,
except, this safety is fragile and assumes that `someText` or `someNumber` are not accidentally widened to `unknown` by the type inference.   And TS uses a similar approach for other built-in JS operators.   

I attribute this quote to Paul Phillips (former Scala's compiler engineer). These are not his exact words, the quote is from memory:

> "There is nothing obviously wrong.  What is wrong is not obvious."

I look at it this way: in TS, type safety is designed to work most of the time. 
If more is needed TS may be not the right choice, but what TS gives us is still much better than plain JS.  

**General safety concerns about `unknown`:** 
Developers, like me, who had spent decades working in languages like Java and then switch to a typed FP language see immediate 
safety benefits just because there isn't any `uknown`-like top type.
The concern about `unknown` is that it can be used with many JS functions and operators. 
These operations effectively bypass the type checking.  Generic functions do not need to be generic if any type variable
can be used in all of these JS functions.

_A statement about everything is either trivial or incorrect_.  

Since I just made it, I wonder which one is it?  Ignoring this paradox, 
consider for example `JSON.stringify` which accepts `unknown`.  
Does this expression (returns `undefined`) make much sense to you: `JSON.stringify(() => {})`?   
Something like `unknown` is probably the only way for TS to achieve JS compatibility, nonetheless it is not ideal.   

I will come back to this discussion again, I plan to discuss the complexity of TS types.
I will also return to the `unknown` type itself in the future in a more theoretical setting. 


## Misleading types

I will finish this installment with a philosophical rant.   
My next post will be a bunch of rants, so I thought I start a bit early. (You probably will say: "hey you have started already!".)

One of my former colleagues liked to use the phrase "gentlemen's agreement". 
It means an agreement between developers (: notice gentlemen is presumptuous and sexist :) to self impose certain limitations on the code they write. 
These limitations are not enforced by the compiler, only by developers who agree to abide by the set rules. 
Coding guidelines, design patterns, you know what I am talking about.

There is a term in Programming Language Theory called _parametricity_. 
Roughly speaking, a language that supports _parametricity_ can assure that a generic function cannot discover what is 
the type behind a type variable. Remove the top (it is discoverable) and the bottom from the language too. You are left with very precise types.
As an example:

```JavaScript
declare function someName<T>(t:T): T
```

could be only implemented as an identity.  Incidentally, there are a few languages that support strict parametricity
and a few that come very close. 

Can you write a whole single page app in TS and give it that signature?  I bet you can.  
We would probably not call it a type-lie. 
Calling it not descriptive would probably be more accurate. Or, maybe just not the best design?   
If some type definitions are better than others, which of them are better?  Apps are written so the decisions 
are being made, but based on what?

I will give you an IMO, a very type centric view of programming:

1. Well written program means well typed
2. Types live outside of TS (or any programming language). Bring the types in, conventions or "gentlemen's agreements" can fill in the gaps if TS falls short.  
3. TS (or any programming language) programming needs a balancing act.  My approach for writing TS
is to balance principled and safe with approachable and informative. That balance is subjective and my balance point may differ from yours.  

Expanding on 1 and 2.  TS does not support parametricity, it is still a good idea to keep generics generic. 
TS type checks my code, I type check TS.  A library (e.g. _office.js_) give me types, I type check it too. 

Expanding on 3: The best communication tools for developers and the best documenting tools for the code, **IMO** and in that order, are:  **types and tests**.  In TS I want my types to be very informative.  For example, here is a vanilla React component in IntelliSense that I like (I like vanilla React):

```JavaScript
const PersonCard: ({ model, onChange }: {
    model: Person;
    onChange: (_: Person) => void;
}) => JSX.Element

//or 
const PersonCard: React.FC<{
    model: Person;
    onChange: (_: Person) => void;
}>
```

I like the the more common approach less:

```JavaScript
const PersonCard: React.FC<Props>
```

And, I am not suggesting the names for optical _setters_ and _getters_ here. I would be equally happy with this:

```JavaScript
const PersonCard: React.FC<{
    get: Person;
    set: (_: Person) => void;
}>
```

my focus is on types not values. There is no safety benefit of doing this.  Communication and documentation are the only goals.


## Next Chapter

It will be more of ranting time!  There are parts of TS that I absolutely love and adore and I will talk about them.
How we approach programming with types or thinking in types?  The complexity of TS types is another big topic to discuss.