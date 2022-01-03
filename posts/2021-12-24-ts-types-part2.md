---
title: Type Enthusiast's Notes about TypeScript. Part 2. Typing Honestly
author: Robert Peszek
featured: true
summary:  TypeScript Types series, types and values that do not match, type predicates, `any`,`unknown`, honest transparent types
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 1. Typing in Anger](2021-12-12-ts-types-part1.html).

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

**Motivating Quote for the series:**  

> "TypeScript began its life as an attempt to bring traditional object-oriented types to JavaScript so that the programmers at Microsoft could bring traditional object-oriented programs to the web. As it has developed, TypeScript’s type system has evolved to model code written by native JavaScripters. The resulting system is _powerful, interesting and messy._"

_From typescriptlang [_add_blank_target TypeScript for Functional Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html)_




## Nutshell

This is the second post in the series devoted to types in TypeScript. 
In this series, I explore type-centric approaches to writing code and often push TS to its limits in doing so. 
I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.

This post will cover TS's type predicates, the notorious `any`, and its safer cousin the `unknown`. 
These are well known and heavily blogged topics.  My goal is provide a little different perspective with a more type-centric view point.  
This series uses _office.js_ as a source of code examples. This post examines the correctness of 
_office.js_ types and fixes them using type predicates.   
My main code example is something I am excited about. It demonstrates a case where TS made me completely rethink a previously written JS code.  
I will discuss some safety concerns about `unknown` (no, this is not a typo, I mean the `unknown` type)
and will set the stage for my future note about complexity of TS types.  
I will finish in the realm of coding conventions discussing transparent, self documenting type definitions. 

## Can I trust the types?

I am going to discuss the obvious gotcha in a gradually typed language like TS: runtime values do not satisfy statically defined types.  
Despite it being an obvious concern, the issue is something a developer who spends most time in a statically typed language (e.g. me) will not have on his / her mind when working in TS.  
The following seem to be the prevalent reasons for why values do not match types: overconfident TS code (e.g. type casting, `any` type), issues with converted JavaScript (declaration files out of sync or containing otherwise incorrect definitions).
I am going to show a real life (or close to real life) example of each.

The series started with an example defining the `Person` type, to avoid jumping back and forth I will repeat it here 

```JavaScript
type Person = {firstNm: string, lastNm: string} 
```

This will be a good conversation starter:

```JavaScript
//Questionable JSON parsing example
const p: Person = JSON.parse('"John Smith"')
```

Your experience with consistency of JSON data may be different from mine.  I rarely see JSON issues in front-end -- back-end conversation. On the other hand, my experience with using 3rd party REST APIs is not exactly stellar.
JSON data problems do happen.

The above code illustrates what I used to call 'fail late' and now I call 'a type I cannot trust' case. It is a nasty situation where runtime errors are nowhere near the actual problem.  Looking at the example, `JSON.parse` function is declared to return the TS's notorious `any` type.  Using `any` bypasses type checking and the code assigns the result to `Person`.  The actual run-time value of `p` will be a `string`, while the type checker is now convinced it is `p:Person`.  

Now, look at the top rated answer in this stackoverflow: 
[_add_blank_target how-to-parse-json-string-in-typescript](https://stackoverflow.com/questions/38688822/how-to-parse-json-string-in-typescript). 
It appears that the above code matches the top rated answer.
Yes, safer approaches are available (look at less popular answers, we will discuss a much safer way as well).   
I am not claiming this to be a prevalent problem in TS code, but it is an interesting issue caused by the coexistence of the typed and the untyped. 

Now, since I already may have angered a large part of the TS community (did I? I hope not.), let's beat a little on **_office.js_**.   

_office.js_ is a source of code examples for my series. Looking into _office.js_ release history suggests that the bond between _office.js_ and TypeScript developed very early. It looks like these projects grew up together. _office.js_ might have even been one of
these Microsoft projects that spearheaded the development of TS.  

**Short Recap**
We are using _office.js_ to interact with Outlook emails. 
_office.js_ provides us with `item: Office.MessageRead` allowing us to 
retrieve data from an email opened in read-only mode.
**(Recap End)**

I imagine it is not that uncommon for a TS library to have a non-nullable property that is undefined at runtime.  
The IntelliSense tells me that `item: Office.MessageRead` contains an overloaded `item.body.getTypeAsync` method. I was hoping to use it to retrieve the type (plain text vs html) of the email body. 

```JavaScript
(method) Office.Body.getTypeAsync(options: Office.AsyncContextOptions, callback?: ((asyncResult: Office.AsyncResult<Office.CoercionType>) => void) | undefined): void (+1 overload)
```

`getTypeAsync` is undefined at runtime.  It looks to me like the TS declaration files are not in sync with JavaScript.  My hypothesis seems to be confirmed by the 
[_add_blank_target `item.body.getTypeAsync`](https://docs.microsoft.com/en-us/javascript/api/outlook/office.body?view=outlook-js-preview#getTypeAsync_callback_) documentation suggesting that this method is 
available when email is open in compose mode (not when using `Office.MessageRead`).  (I am using office online and the latest _office.js_ as of the time of this writing.)  
_Please message me in git discussions if you think I am misrepresenting it._  

_It seems like _office.js_ types are a little off._

We should look at the type definition of the _office.js_ [_add_blank_target `Office.context.mailbox.item`](https://docs.microsoft.com/en-us/javascript/api/outlook/office.item?view=outlook-js-preview) a little closer.  
This property is overloaded to be one of the following types (let me call them _facets_):


> `Office.AppointmentCompose` (composing calendar entry)   
> `Office.AppointmentRead`  (read-only calendar entry)   
> `Office.MessageCompose`  (composing email)   
> `Office.MessageRead`  (read-only email)  


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
Type checked programs will fail at runtime. _office.js_ type declarations are incorrect.

_office.js types are off for sure._

In a weird way, this explains why the undefined `item.body.getTypeAsync` has not been noticed. 
Without a corrective reassignment to, say, `Office.MessageRead` many other methods are `undefined` at runtime and it is harder to single this particular one out.


Gradual typing over the wild-west JS has to come with maintenance challenges.  
Nonetheless this is surprising.  What are the types good for if they're not accurate?

> "You take the blue pill — the story ends, you wake up in your bed and believe whatever you want to believe.   
> You take the red pill — you stay in Wonderland, and I show you how deep the rabbit hole goes"

_Morpheus about not believing types in a gradually typed language_    
_... nightmares of JavaScript running on my walls and ceilings make me wake up screaming_


## Note about the `any` type

My first example in this post used the infamous `any` type.  Let's have a closer look.   

`any` type is crazy.  It behaves like the _top_ (you can assign any other type to it). It also behaves like the _bottom_ (it can be assigned to any other type, maybe except of _never_). 
Ideally, the bottom type is empty, this one clearly is not.  

_As a result, any value can have any type._

We should have some fun with this.

```JavaScript
//express yourself with _any_ (notice no casting, only assignments)
const sad: any = "emptiness and sadness"
const sadVoid: void = sad

const myCallback = (n: number): void => {
    return sadVoid;
}
```

You can have your own favorite `null` that is not `null` value, you can _define_ your own `undefined`. 
Sky and your creativity are the limits.  I will spoil this party and say that I do not recommend doing it.  Oh, maybe just a little.  Well OK, one more:

```JavaScript
const sassy: any = {netWorth: "billion dollars", popularityLevel: "celebrity"}
const sassyNull: null = sassy

const p: Person | null = sassyNull
```

A bottom that is not empty will cause the language to be unsound. Allowing all values in a bottom type, I would call it insane.   
However, using an _any_ type similar to TS's seems to be a common practice in gradually typed languages (e.g. Python does it too).  
Using `any` is like saying "hey, TS, please suspend type checking, I know what I am doing". 
This is the antithesis of type safety, but what else can TS do and maintain JS compatibility?  

Actually, TS has a very clever solution for this, it is described in the following sections.   
I view `any` as a form of type coercion or casting.  



## Casting _casting_ in a bad light 

I will use the term casting and type coercion interchangeably. TypeScript documentation also uses the term _type assertion_. I view the `any` type to be in the same boat as well (an implicit type coercion).  
TS uses the `t as T` or `<T> t` syntax to cast expression `t` into type `T`, e.g. `iAmSureIsString as string`.    
(IMO, the second notation, `<T> t`, is somewhat unfortunate as it is very similar to type application and generic function declaration e.g. `const f = <T>():T` declares,  `<T>f()` casts, `f<T>()` applies. 
I recommend the `v as T` syntax to make casting more explicit and searchable in your code.)
 
_side_note_start **Type enthusiast's note on casting at large:**  
Typically (and rightly) casting is considered to be a last resort, _only cast if you must_.

With more involved types it is often harder to write code that type checks.  That increases the appeal of casting or finding some other alternatives for nudging the type checker into agreeing.    
Some languages offer the ability to write a program to persuade the type checker about type equality (write actual _proof of type equality_). This is an advanced feature and is available in only a few languages (e.g. Coq, Idris, Haskell). Writing such programs is often challenging or even impossible. 
(I consider writing such proofs to be one of the highest level "type games" that a developer can play. 
It is both a challenge and fun. A great intro is [_add_blank_target TDD with Idris](https://www.manning.com/books/type-driven-development-with-idris)) 

There is an alternative to type coercion that allows programs to type check but will throw an exception when executed.  
This can be useful for interacting with the type checker when writing code. 
We have seen a TS version of this already, function [_add_blank_target `_<T>(): T`](2021-12-12-ts-types-part1.html#type-holes), defined in my previous post and stolen from 
[_add_blank_target Type holes in TS](https://dev.to/gcanti/type-holes-in-typescript-2lck). 
Such programming practice is foreign to most languages but becomes very convenient when working with more involved types.  We are using it in this series. 
_side_note_end

Let's beat on _office.js_ some more. 
[_add_blank_target Here](https://docs.microsoft.com/en-us/javascript/api/outlook/office.item) is a piece _office.js_ documentation about (you guessed it, this post is so very predictable) the
[_add_blank_target  `Office.context.mailbox.item`](https://docs.microsoft.com/en-us/javascript/api/outlook/office.item):

> If you want to see IntelliSense for only a specific type or mode, **cast** this item to one of the following:  
>      `AppointmentCompose`  
>      `AppointmentRead` ...     

TS offers a neat alternative to casting.  I will explain it by _not_ following the _office.js_ documentation ;)

As I indicated already, I can interact with outlook email using `Office.context.mailbox.item`. 
However, `item` property is overloaded into several types discussed in the previous section (I called them _facets_): 

The legacy code I am currently re-implementing at work is retrieving the email subject using `item.subject` and checking what kind of `item.subject` it is (a string, has asyc methods, etc) and using it accordingly.  It does a similar _"check before you use"_ game to retrieve `to`, `from`, `cc` and other email information.  
Such an approach is typical, almost idiomatic to JS.  It is also hard to maintain as making changes directed at one facet can easily break the other facets. 
And you can test your heart out on all emails you can think about and your app will still crash and burn if used with an office calendar appointment.

So what is the new TS-idiomatic way to do it?  TS has the `is` types.

### Improving _office.js_ with type predicates

```JavaScript
export const isMessageRead = (item: any): item is Office.MessageRead => {
    return (item.itemType === Office.MailboxEnums.ItemType.Message) && item.getAttachmentsAsync === undefined
} 
  
export const isMessageCompose = (item: any): item is Office.MessageCompose => {
    return (item.itemType === Office.MailboxEnums.ItemType.Message) && item.getAttachmentsAsync !== undefined 
} 

declare function doSomethingWithViewedEmail(item: Office.MessageRead): void
declare function doSomethingWithComposedEmail(item: Office.MessageCompose): void
declare function onlyEmailEntriesAreSupported(): void
```

(OK, checking `getAttachmentsAsync` is ugly, office.js could provide some nicer and more stable way to identify the exact `item` type.  This is still not bad. Let's move on.)

`doSomethingWithViewedEmail` and `doSomethingWithComposedEmail` can now be coded with confidence (if I trust _office.js_ types) following the corresponding `MessageRead` or `MessageCompose` types.  IntelliSense makes writing these a breeze and the code is very clean.
E.g., `subject` is just a `string` in `MessageRead`.

I can use these without any casting: 

```JavaScript
//'unknown' replaces incorrect office.js type (see previous section). 
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

This is a really nice, bravo TypeScript!  Simple to use, yet very useful.   

It is also IMO a very interesting case of TS making a bigger impact on how we actually code. 
_"Check before you use"_  game becomes type assisted and happens on a larger scale of `item` types instead of
single (e.g. the email _subject_, _from_, _cc_, etc.) properties.  
This adds a lot of clarity to the code. 
TS types not just check my code, types change how I code!


`t is T` type is one of the TypeScript [_add_blank_target narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) tools. The documentation refers to it as a _type predicate_ or a _type guard_ (a more general term).  
IMO, the idea of a middle ground between type checked safety and unsafe type coercion is brilliant.   
It is something that sits a half way between a cast and a type equality proof.  
This will probably influence other languages (e.g. here is [_add_blank_target enhancement proposal for Python](https://www.python.org/dev/peps/pep-0647/)).

The syntax `t is T` is interesting, it clearly borrows from dependently typed languages.  The value `t` appears next to the type `T` and comes from the earlier part of the declaration.  This also somewhat justifies the existence of otherwise cumbersome parameter names in type definitions (something I complained about in my [_add_blank_target previous post](2021-12-12-ts-types-part1.html#office.js.-using-ts-in-anger)).

I hope the TS community develops a healthy aversion to casting.  Why would you use a type checker if you keep subverting it?  I also hope that exporting functions returning type predicates will become a standard practice for APIs.  


**Use of `any` in type predicates**  
Arguably, a safer approach was to define `isMessageRead` and `isMessageCompose` using a parameter type that is more restrictive than `any`.   
My goal was to keep this example very simple and avoid introducing a `CorrectedOfficeItem` type to fix _office.js_ typing. In real code, I would opt in for introducing the corrected type. Linked github repo defines and uses `CorrectedOfficeItem`.    
However, using `any` in type predicate implementations appears to be a common practice. Implementing a type predicate typically requires checking for existence of object properties and `any` provides access to these.   
My suggestion is to avoid type guards in certain places, e.g. in generics. We want generics to be generic. 


## Note about the `unknown` type

This post started with unsafe use of `JSON.parse`.  I am quite sure that if TypeScript could travel back in time
`JSON.parse` would return `unknown` instead of `any`. 

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

A rough view (IMO) of what type safety is: an ability to _separate apples from oranges_. 
If you can assign both an apple to `unknown` and an orange to `unknown` then they are no longer separated.   
What makes this worse in TS, is its occasional tendency to widen return types to `unknown`. TS tends to do that 
if it cannot find a more precise return type or when it gets confused. 
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

Also, notice `unknown` in some [_add_blank_target blooper](2021-12-12-ts-types-part1.html#compilation-bloopers) examples from the previous post:

```JavaScript
//these should not compile but they do. Names are consitent with previous post and the linked github repo

//const nonsense2: <T1, T2, R>(a: (ax: T1, bx: T2) => R) => (b: unknown) => (a: T1) => (b: T2) => R
const nonsense2 = curry(curry) 
//const nonsense3: <T1, T2, T3, R>(a: (ax: T1, bx: T2, cx: T3) => R) => (b: unknown) => (a: T1) => (b: T2) => (c: T3) => R
const nonsense3 = curry(curry3)
//const nonsense4: <T1, T2, R>(a: (ax: T1, bx: T2) => R) => (b: unknown) => (b: unknown) => (a: T1) => (b: T2) => R
const nonsense4 = curry(curry(curry))
```

and we will encounter more examples of _unsafe widening_ in future notes.  It seems like TS tends to widen types to `unknown` instead of reporting a compilation error.  This, in turn, makes things less safe.

I really love the fact that this code (a contrived example but generalizes easily to real situations) does not compile:

```Java
//Compilation error:
//This condition will always return 'false' since the types 'string' and 'number' have no overlap.
"some email body" === 1 
```

However, this does compile:

```JavaScript
("some email body" as unknown) === 1
```

and so does this:

```JavaScript
emailBody4 === 1
```

Let's bring in the type hole  [_add_blank_target `_<T>(): T`](2021-12-12-ts-types-part1.html#type-holes) from the last post.
The type hole is a convenient way to ask the compiler type questions. 

```JavaScript
//hovering over res and _ allows me to see the typing of '===`
const res = _() === _()
(1 as 1) === _()
_() === (1 as 1)
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

In fact, `===` _does not have a type_.  It is a built-in JS operator.  TS applies semantic narrowing rules to the code that uses it.   
This complex approach is needed to provide type safety while maintaining compatibility with JS.   
TS's semantic rules prevent certain types like `someText === someNumber` from compiling,
except, this safety is somewhat fragile and breaks when `someText` or `someNumber` are accidentally widened to `unknown` by the type inference.   TS uses a similar approach for other built-in JS operators.  (We will discuss `===` semantics in a deeper detail in the next post.)


**General safety concerns about the top:** 
Developers, like me, who had spent decades working in languages like Java and then switched to a typed FP language see immediate 
safety benefits just because there isn't any top type.
The concern about `unknown` is that it can be used with many JS functions and operators. 
Such use is not type safe, similarly to how Java's `Object` methods are not type safe.   
From the type safety point of view, these JS functions and operators are not implemented well either.
Consider for example `JSON.stringify` which accepts `any`. 
Does this expression (it returns `undefined`) make much sense to you: `JSON.stringify(() => {})`?   
Generic functions lose safety too, generics are not _generic_ if a generically typed function parameter can use a _specific_ JS function.  

Something like `unknown` is probably the only way for TS to achieve JS compatibility, nonetheless `unknown` is not ideal.   

I will come back to this discussion again, I plan to discuss the complexity of TS types.
I will also return to the `unknown` type itself in the future in a more theoretical setting. 


## Honest typing conventions

These notes will be a little ranty (you'll probably ask: "Did you read your other notes?"). Any coding convention is effectively a hand waving rant.  That is why we use types, so we can rant less!  

One of my former colleagues liked to use the phrase "gentlemen's agreement". 
It means an agreement between developers to self impose certain limitations on the code they write. 
These limitations are not enforced by the compiler, only by developers who agree to abide by the set rules. 
Coding guidelines, design patterns, you know what I am talking about.

There is a term in Programming Language Theory called _parametricity_. 
Roughly speaking, a language that supports _parametricity_ can assure that a generic function cannot discover what is 
the type behind a type variable. Remove the top and the bottom from the language too. You are left with very precise types.
As an example, 

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

I will give you my very type centric view of programming:

1. Well written program means well typed. Well typed means the types express what is happening.   
2. Types are more fundamental than a programming language. 
3. Coding conventions supplement the language in implementing typing concepts. 
4. TS (or any programming language) programming needs a balancing act.  My approach for writing TS
is to balance principled and safe with approachable and informative. That balance is subjective and
project specific, my balance point may differ from yours.  

**Expanding on 2:**   
TS type checks my code, I type check TS (last post).  A library (e.g. _office.js_) provides types, I type check these types and fix some of them (this post). Developer interventions are needed.  Understanding of types does not
change with a programming language environment.  The cumbersomeness of their use does.  TS is, comparatively speaking, not that bad.  

**Expanding on 3:**  
In TS, almost any program can have almost any type. I can implement 

```JavaScript 
function program(): void {...}
```
and do almost anything I want in that code.  
It would not be very clear if most of my types looked like this. There needs to be some coding convention
that discourages such code.   
Enforcing some level of parametricity when implementing generics would be another
example of a coding convention.  

_The goal is to move from designing programs to designing types._   
_This post suggests that types are used to define coding conventions._

Here are some bootstrapping ideas.

### Referential Transparency

Referential Transparency is an FP topic but is also very relevant to types and crucially important to the discussion of "type honesty".  

A function is referentially transparent if it does the same thing every time it is called. 
Referential transparency comes with clear type signatures.  The output needs to be a function of the inputs and of nothing else.  You can do things like curry or partially apply, but you cannot say, retrieve the current time and act on it (that time parameter would need to be provided as input).   
The above `program:() => void` would be referentially transparent only if it did not do anything, just returned.   
IMO well written programs identify and separate the referentially transparent parts.

In TS, referential transparency is a coding convention.  I will use _React.js_ example to demonstrate this.  Readers not familiar with React should think about creating a function from some model (`Person` in this example) to an actual part of the HTML DOM.  Here is my example of a vanilla React component type 
(I like React to be vanilla as much as possible)

```JavaScript
const PersonCard: ({ model, onChange }: {
    model: Person;
    onChange: (_: Person) => void;
}) => JSX.Element
```

Hopefully, the implementation does not use any hooks, it only uses the parameters (I call them setters and getters) to create bits of HTML with event handlers.  This would be an example of a referentially transparent React type.
It also would be an example of a very explicit type that is very "honest". 

_side_note_start **FP side notes:**
Such approach is not novel at all (e.g. Elm uses a similar approach, only not as a coding style but as its architecture).   
Lenses can be used as just a coding convention too.
_side_note_end

Many developers will very much disagree with me on this.  E.g. many will prefer to encapsulate state handling inside
components. I do not intend to argue 
which approach is better.  I will just point out that encapsulation is secretive in the type definition and I am looking
for transparency.  Many parts of React will require some use of hooks, 
my approach is to do that only when I have to, not when I want to. The goal is to make things very type-explicit.  It is an IMO.

Such type is also self documenting.  

**Expanding on my point 4:** IMO, the best communication tools for developers and the best documenting tools for the code, in that order, are:  _types and tests_.  I will only focus on the first.

### Types as documentation 

When I write TS, I want my types to be very informative.  For example, compare these two slightly modified versions of the above React component:

```JavaScript
const PersonCard: React.FC<{
    model: Person;
    onChange: (_: Person) => void;
}>
```

vs:

```JavaScript
const PersonCard: React.FC<Props> //Commonly used 'Props' type alias defined next to 'PersonCard'
```

I like the first one better.   
And, I am not suggesting the names for the setters and getters here. I would be equally happy with this:

```JavaScript
const PersonCard: React.FC<{
    get: Person;
    set: (_: Person) => void;
}>
```

My focus is on types, not values. There is no safety benefit in doing this.  Communication, documentation and accessibility are the only goals.  
In a modernized interpretation of the KISS principle I think of "Simple" as a lot of very transparent types.


## Next Chapter

There are parts of TS that I absolutely adore and I will talk about them.
The complexity of TS types is another big topic to discuss. 
Complexity causes compilation issues (we will encounter some new bloopers) and makes the language hard to use. 
~~This will take me a few weeks to finish.~~   
Here is the link: [_add_blank_target Part 3](2022-01-03-ts-types-part3.html).

I am working on these notes during the 2021 holiday season.
_Merry Christmas, Happy New Year!_ Stay happy and healthy!
