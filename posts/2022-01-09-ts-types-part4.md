---
title: Type Enthusiast's Notes about TypeScript. Part 4. Programming with Type Variables
author: Robert Peszek
featured: true
summary:  TypeScript Types series, higher rank types, existentials, phantom types, preventing type widening
toc: true
changelog: <ul> 
    <li> (2022.05.10 - 2022.05.29) Minor edits </li>
    <li> (2022.05.29) Draft warning removed </li>
     </ul>
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 3. TS Complexity](2022-01-03-ts-types-part3.html).

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

This is the fourth post in the series devoted to types in TypeScript. In this series, I explore type-centric approaches to writing code and push TS to its limits in doing so. I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.

This post will be a little more advanced and will focus on programming with type variables. 

[_add_blank_target Types and Programming Languages](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) is 
the book about types I recommend to everyone (... even if not very successfully). 
Reading TAPL will be a big eye opener for many developers. 
The good news is that types dramatically increase programming efficiency so learning them is a good investment.   
This section of the post will be a little more TAPL-ish with some more advanced CS.
The topics I am about to present are IMO very useful and I will try my best to present them in a digestible way. 

I will discuss type variable scoping, rank-2 types, and existential types. Some examples show a level of safety that I did not expect to be able to pull off!  As it turns out, we can even prevent subtyping in TS.

Before we start I need to build up some tooling.  I will start with a tiny bit of type level programming. 

## Safety preventing `unknown`

In previous posts, we have seen examples where TS decided to widen types to `unknown` rather than report a compilation error.   
Interestingly, TS allows enough type level programming so we can try to fix such issues ourselves.  

```JavaScript
type IsUnknown<T> = unknown extends T? true: false

function verifyUnknown<T>(p: IsUnknown<T>, t: T): T {
    return t
}

verifyUnknown(false, "test")
const unk: unknown = {}
verifyUnknown(true, unk)
```
```Java
//Compilation Error
//Argument of type 'false' is not assignable to parameter of type 'true'.ts(2345)
verifyUnknown(false, unk)
```

In my first post, I had an example of incorrect code [_add_blank_target `body4`](2021-12-12-ts-types-part1.html#bumps-on-the-path) inferred as `unknown` instead of a `string`.  Wrapping such code
in `verifyUnknown(false, body4)` would have alerted me with a compilation error.   
You may point out that a much simpler solution is to just type annotate: `const body4: string`.   
I agree. However, having a more generic solution at our disposal is also useful.  We will see shortly why. 

Here is a short TAPL-ish explanation of what just happened. TS allows me to use type level ternaries. 
`IsUnknown<T>` is a type level function (TAPL'sh term for this is _Type Family_) that maps types `T` to literal boolean types `true` or `false`.  These types have only a single (a _singleton_) value: `true: true` and `false: false`. 
If I write `verifyUnknown(false, someExpression)`,
TS will figure out that it has to use `false` as the type. `false` matches the second part of the type level ternary and, thus, implies that the ternary predicate `unknown extends T` is not true. 
Hence `T` is not `unknown`.  

I will use `verifyUnknown` to do some type level trickery. 
You may wonder if we can extend this approach to other types, not just to `unknown`.  I will get there in this post as well.




## Type variable scoping 

Type variable scoping has two aspects. Let's start with the most obvious one.  The type variable being visible inside of the implementation body.  This is simple stuff, I just want to share an obvious gotcha that got me at some point:

```JavaScript
export const bodyScopeExample1 = <T>(value: T | undefined | null): void => {
    if(value) {
        const t: T = value //you can access type variables in function body
    }
}
```

This approach to defining type signatures (actually my preferred way to write function type signatures)
puts `T` out of scope:

```Java
export const bodyScopeExample2: <T>(_: T | undefined | null) => void = value => {
    if(value) {
        const t: T = value  //Cannot find name 'T'.ts(2304)
    }
}
```

For the type variables to be visible in the implementation they need to be on the _RHS_ of `=`.

The other aspect of type variable scoping is much more interesting:



## Higher Rank types

Consider these 2 function declarations:

```JavaScript
declare function fn1<T> (f:(t:T)=> void): void 
declare function fn2(f: <T>(t:T)=> void): void 
```

In `fn2` the scope of `T` is much narrower.  In TAPL-ish this would be called a rank-2 type.  
So what is the difference?  Let's try to use both:

```JavaScript
const useStr = (s:string): void => {}
fn1(useStr)
```
```Java
//Compilation Error:
//const useStr: (s: string) => void
//Argument of type '(s: string) => void' is not assignable to parameter of type '<T>(t: T) => void'.
//  Types of parameters 's' and 't' are incompatible.
//    Type 'T' is not assignable to type 'string'.ts(2345)
fn2(useStr)
```

Basically `fn2` requires the argument to be fully generic and `useStr` is not.  

I can play the same games with generic arguments that return `T` 

```JavaScript
declare function fn4(f: <T>() => T): void
```

but will not do that here as these tend to be less practically useful. 

Here is how I think about it:

_Higher rank means generics are first class_



## Existential types

In TAPL-ish this is called _existential quantification_  and it has to do with the ownership of definitions. In OO you would say "code to interfaces, not implementation", it is also related to the OO concepts of _inversion of control_ and _dependency injection_.  Here is how the story goes:



### Replacing factory pattern 

```JavaScript
interface Foo {
    foo: string
}

class MyFoo implements Foo{
    foo: string
    constructor() {
        this.foo = "bar"
    }
}
```

We want to be able to hide which implementation of `Foo` we are passing to a callback.  
Our first approach tries to use a vanilla TS generic function with a callback argument.  
The input function parameter uses some `<T extends Foo>` of an unknown exact implementation type:

```Java
function factoryWithCallback<T extends Foo> (f:(_:T) => void): void {
    //Argument of type 'MyFoo' is not assignable to parameter of type 'T'.
    // 'MyFoo' is assignable to the constraint of type 'T', but 'T' could be instantiated with a different subtype of constraint 'Foo'.ts(2345)
    f(new MyFoo()) //Compilation Error
}
```

It does not work and it should not work!  We need to use a rank-2 definition:

```JavaScript
//Compiles!
function existencialFactory(f: <T extends Foo>(_:T) => void): void {
    f(new MyFoo())
}
```

This simulates what is called an existential type.  The function that accepts a callback owns the definition of the exact type that is passed to the callback.  The callback itself needs to be generic and accept any possible implementation.  
Note the scoping of `T` inside the type defining the function parameter. 

This _inverts the control_ from the implementation of the callback to the caller.

_side_note_start **Note on terminology:**  Repeating the above definitions 

```JavaScript
declare function fn1<T> (f:(t:T)=> void): void 
```

The `fn1` needs to be defined for all possible types `T`.  The name for it is _universal quantification_.  Some languages
even use the `forall` keyword to describe it.  

```JavaScript 
declare function fn2(f: <T>(t:T)=> void): void 
```

`fn2` function parameter `f` needs to be defined for all possible types `T`. 
However, `fn2` can pick whatever type it wants for `T` and use `f` with it.  
In other words, there exists some type `T` that will be used but `f` has no way of knowing which. 
The name for it is _existential quantification_.  Some languages
even use the `exists` keyword to describe it.  

The general concept of existentials is broader than what I am describing here and what TS supports. 
However, this by itself is plenty powerful. 
_side_note_end


### Preventing information escape 

I am drawing a blank trying to think about an OO analogy for this.  It is somewhat related to friend classes in C++, package-private scope in Java ... only not exactly.  
This example will accomplish more than the above 'factory' pattern and will not use any interfaces or classes:

```JavaScript
// Using higher rank to protect data
// Imaginary world without debuggers, JSON.stringify, etc
type Api = {getGoodies: string[]}

//provides access to API, password needs to be protected
declare function login<Password>(p: Password): Api 

//provide password to a computation, that computation should be able to use the password but shouldn't return it
const secretive = <R> (fn: <Password> (p: Password) => R): R  => {
   const s : any = "topsecret"
   return fn (s)
}
```

The example is somewhat contrived with the main goal of illustrating the point.   
This code exposes building blocks that work together. 
To get the access to the `Api` type, you have to use `login` and you have to use it inside the provided
`secretive` function. 
Working with an API like this is like assembling a jigsaw puzzle. Types prevent from jamming a square peg into a round hole.  
Note, `Password` is a type variable and we have used the existential type trick.  

This code uses the building blocks:

```JavaScript
const goodProgram = <Password>(p: Password): string[] => {
    const api = login(p)
    return api.getGoodies
}

const stealPassword = <Password>(p: Password): Password => p

secretive(goodProgram)
secretive(stealPassword)
```

Unfortunately, `secretive(stealPassword)` compiles.  Somewhat typical of TS, instead of providing robust type safety, the compiler infers `unknown` and accepts my questionable code. Hovering over `secretive` shows me this:

```JavaScript
//const secretive: <string[]>(fn: <Password>(p: Password) => string[]) => string[]
secretive(goodProgram)

//const secretive: <unknown>(fn: <Password>(p: Password) => unknown) => unknown
secretive(stealPassword)
```

That is why I have created the `verifyUnknown` safety in the previous section:

```JavaScript
const valid = verifyUnknown(false, secretive(goodProgram)) //valid: string[]
```
```Java
//Argument of type 'false' is not assignable to parameter of type 'true'.ts(2345)
const invalid = verifyUnknown(false, secretive(stealPassword)) //does not compile!
```

To make it a bit nicer we can package `verifyUnknown` and `secretive` into one function:

```JavaScript
const verySecretive = <R> (_: IsUnknown<R>, fn: <Password> (p: Password) => R): R  => {
    const s : any = "topsecret"
    return fn (s)
 }

const valid = verySecretive(false, goodProgram) //valid: string[]
```
```Java
const invalid = verySecretive(false, stealPassword) 
```

This creates some interesting safety.   Obviously you could still do a lot of mischief if you wanted to. 
There is a need for some 'gentlemen's agreements' to not use casting, `JSON.stringify`, to not use `true` in `verySecretive` etc. 
However, if you think about creating clear contract APIs, this approach could be very powerful. 

Existentials are not exactly equivalent to OO.  However, using existential types can often accomplish a lot of the same things and often in a cleaner way.  Using existentials and disabling OO features like `unknown` 
feels a bit contrived, but IMO is still useful.  It would be nice if TS provided a cleaner way to disable the use of `unknown`.   
I do not know how robust this type of coding is.  I have not played enough with this approach in TS to give you a list of gotchas.
In my very limited experience, this seems similar to the rest of TS, TS stops working if I start pushing harder.

_side_note_start **Existentials and higher rank at large:** 
These concepts have lead to some amazing programming.   
For example, existentials are related to dependent pairs (dependent sums) in depenently typed programming languages.  Dependent typing provides some very strong types. One example could be
lists with a type checked length. You want to be able to use such lists when processing runtime data that can have arbitrary size.  That size 'exists' but cannot be known statically at the compile time. 
This is in essence an existential construction.  

Another amazing example is an old (1993) code called _State Threads (ST)_ (currently part of std base library in Haskell).  It allows to use a local mutable state to define computations that have to be _referentially transparent_ (I have discussed referential transparency in [_add_blank_target Part 2](2021-12-24-ts-types-part2.html#referential-transparency)).
This is possible because the access to mutate the state cannot escape outside of these computations.  ST API remains unchanged since it was created 30 year ago, you can't improve on perfection!

I see higher rank types, mostly rank-2 being used a lot.  Having ability to pass generic (polymorphic) functions
around is very useful. In my non-TS projects, the problem of 'separating interface from implementation' is typically solved by defining an EDSL (Embedded Domain Specific Language) and an interpreter. Interpreters are polymorphic (generic in TS lingo). Rank-2 types have to be used to make them first class and pass them around.
_side_note_end


## Safety preventing subtyping 

Many TS users have observed the need for this.  The term _exact type_ is floating around, I believe _flow_
introduced this name.  I have seen solutions like this one being proposed:

```JavaScript
function exact<T>(item:T): T {
    return item
}

type Hello = {hello: string}
```
```Java
//Argument of type '{ hello: string; since: number; }' is not assignable to parameter of type 'Hello'.
//  Object literal may only specify known properties, and 'since' does not exist in type 'Hello'.ts(2345)
exact<Hello>({hello: "world", since:2002})
```

This safety is fragile (and a TS design inconsistency IMO) as the following example shows:

```JavaScript
const helloSince = {hello: "world", since:2002}

exact<Hello>(helloSince) //complies
```

To create something more robust, here is a code that combines the above `unknown` verification idea with existentials:

```JavaScript
type Same<P,T> = P extends T? (T extends P? true: false): false

const verifySame = <P> () => <T> (_: Same<P,T>, t:T): T => t

verifySame<Hello>()(true, {hello: "world"}) //'true' indicates that type matches
verifySame<Hello>()(false, {hello: "world", since : 2020}) //'false' is needed to acknowledge types are different 
```
```Java
//Argument of type 'true' is not assignable to parameter of type 'false'.ts(2345)
verifySame<Hello>()(true, {hello: "world", since : 2020})
verifySame<Hello>()(true, helloSince)
```

You may have noticed a case of typing euphoria here.  I used rank-2 construction because it allows me
to type annotate with only one type variable.  This is nice but often not essential. 

Here is an implementation of `safePush` that acts invariant, it does not use any 
existential tricks:

```JavaScript
//Note to get 'safePush' I ended up with casting, this is a quick and dirty example and can be done slightly better
// However, this cast could be an indication that we are changing how TS compiler works
// Kinda makes sense, to overrule the compiler I may need to cast
const safePush = <P, T> (_: Same<P,T>, ps: P[], t: T): number => ps.push(t as any)

const intlist: number[] = [1,2,3]
const unklist: unknown[] = intlist  //exploits array covariance
unklist.push("not a number") //unsafe 'push' adds a 'string' to 'intlist'

safePush(true, intlist, 1) //this is safe
```
```Java
safePush(true, unklist, 1)    //this is risky and will not compile 
safePush(true, unklist, "not a number") //this is risky (here wrong) and will not compile 
```

Note, to be even safer I would need to prevent `unknown` as well:

```JavaScript
const unkstr: unknown = "not a number"
safePush(true, unklist, unkstr)  //unfortunately compiles

//An even safer version of 'Same'
type SameAndKnown<P,T> = P extends T? (T extends P? (unknown extends T? false: true): false): false

const verySafePush = <P, T> (_: SameAndKnown<P,T>, ps: P[], t: T): number => ps.push(t as any)

verySafePush(true, intlist, 1)  //this is safe
```
```Java
verySafePush(true, unklist, unkstr) //this is risky and will not compile!
```

We have discussed problems with the TS approach to variance in the [_add_blank_target previous installment](2022-01-03-ts-types-part3.html#variance-problems). We have a DIY approach to fight back!

Side Note: The linked github repo has an existentially typed version of `safePush` (`safePush2`) that has just one top level type variable.
That version is more cumbersome to use. TS ends up not working well with it.  

Another fun exercise:

```JavaScript
const safeEq = <P, T> (_: Same<P,T>, a: P, b: T): boolean => a === (b as unknown)

safeEq(true, {hello: "word"}, {hello:"dolly"})
```
```Java
safeEq(true, {hello: "word"}, {hello:"word", since:2022}))
safeEq(true, 1, "str")
```

We have discussed problems with TS approach to `===` narrowing in the [_add_blank_target previous installment](2022-01-03-ts-types-part3.html#complexity-of-ts-types). Again, we have a DIY approach to fight back.


This section is related to a number of feature requests: 
[_add_blank_target TypeScript issue 12936](https://github.com/microsoft/TypeScript/issues/12936) and
[_add_blank_target TypeScript issue 7481](https://github.com/microsoft/TypeScript/issues/7481). 
Hopefully a future version of TS will provide a simpler way to achieve invariance and disable subtyping. 


## Phantom types

TypeScript is somewhat unique in supporting _Structural Types_. 
Types like `type Person = {firstNm: string, lastNm: string}` are structural. That means the name `Person` is only an alias, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition in a language like Java. Two structurally identical classes are still considered different types (this is called _nominal typing_).    

It is sometimes convenient to be able to define different types that share the same structure. 
_Phantom types_ are a way to do that. We say _phantom_ because these types have no impact on runtime values. 

Somewhere around 2006, haskell wiki published a write-up about [_add_blank_target phantom types](https://wiki.haskell.org/Phantom_type).  The write-up was expanded in 2010 to include a form validation example. Since then all blogs (in any programming language) about phantoms show a validation example.  I decided to be as unoriginal as everyone else.  This will allow me to better focus on how it is done in TS.

My first attempt at phantom types in TS will fail.  But this code should make the idea behind phantoms clear:

```JavaScript
//Marker type
type Validated = {type: "validated"}

//For simplicity this is just a string
type ValidationError = string

//Extra phantom type variable 'T' 
type Person<T> = {firstNm: string, lastNm: string}


//Validate person in some way returning 'Validated' phantom marker
declare function validate<T>(p: Person<T>):  ValidationError | Person<Validated> 

//Function to be used only if phantom 'T' is the 'Validated' type 
declare function doSomethingValidated(p: Person<Validated>): void
```

Again, these types are trying to create a jigsaw puzzle. One I can assemble in a specific way only.  
If the puzzle machinery works, I will have to call `validate` first to be able to use `doSomethingValidated`.

Only, this machinery does not work. The following code compiles:

```JavaScript
function validatedOrNot<T>(p: Person<T>): void {
    doSomethingValidated(p)
}

type ClearlyNotValidated = {type: "notvalidated"}

function notValidated (p: Person<ClearlyNotValidated>): void {
    doSomethingValidated(p)
}
```

The fix is to provide a value level information about `T` in an optional property.  
This type definition replaces the one above:

```JavaScript
//Modified definition adds value level representation `phantom?: T` 
type Person<T> = {firstNm: string, lastNm: string, phantom?: T}  

//provide a way to create person that ignores the additional 'phantom' property:
const createPerson : <T>(fst: string, lst: string) => Person<T> = (fst, lst) => {
    return {firstNm: fst, lastNm: lst}
} 
```

Now this compiles:

```JavaScript
function validated(p: Person<Validated>): void {
    doSomethingValidated(p)
}
```

But these no longer do:

```Java
// Compilation Error
// Argument of type 'Person<T>' is not assignable to parameter of type 'Person<Validated>'.
//   Type 'T' is not assignable to type 'Validated'.ts(2345)
function validatedOrNot<T>(p: Person<T>): void{
    doSomethingValidated(p)
}

// Compilation Error
// Argument of type 'Person<ClearlyNotValidated>' is not assignable to parameter of type 'Person<Validated>'.
//   Type 'ClearlyNotValidated' is not assignable to type 'Validated'.
//     Types of property 'type' are incompatible.
//       Type '"notvalidated"' is not assignable to type '"validated"'.ts(2345)
function notValidated (p: Person<ClearlyNotValidated>): void {
    doSomethingValidated(p)
}
```

I believe phantom types are used by some FP libraries in TS, e.g. _fp-ts_, these libraries use somewhat different techniques to get phantoms.  There may be advantages to doing phantom types differently than what I have presented. 
The above approach is the simplest I can think of.  

_side_note_start **Phantom types at large:**  Phantom types can be used to do a lot of crazy type level stuff.  The most wild use I have seen is 
[_add_blank_target Ghosts of Departed Proofs](https://iohk.io/en/research/library/papers/ghosts-of-departed-proofsfunctional-pearls/) (this uses Haskell).  
Here is a simplified and easy to understand example in TS. Think about a non-mutable list, your function accepts a list and does something with it, 
your code needs the list to be sorted to work.
You can encapsulate this and conservatively sort it just in case (this approach seems not performance optimal),  you can document your function by saying
that it is the caller responsibility to sort (do developers read documentation?), ... or you can introduce a phantom type:

```JavaScript
//Sort status as a phantom type,  'List' has type level information about its sort status.
type List<T, SortStatus> = ...

interface Comparator<T> {
    compare (o1: T, o2: T): number
}

declare function sortAscending <T extends Comparator<T>, AnyStatus> (list: List<T, AnyStatus>):  List<T, "ascending">

declare function doSomethingWithSortedList <T extends Comparator<T>> (list: List<T, "ascending">): void
```

Again, notice the types form pieces of a puzzle and can be fitted only in a specific way.  
You can think about a `sort` as something that not only does what it says, but also provides a _token_
to use later to prove that the sort was done.  This _token_ is a phantom type. You can think about creating a library that helps orchestrate a similar approach to programming and this is what the linked article talks about.   
Many FP programming languages support GADTs, these are very powerful types and limit the popularity of (subsume) phantom typing.  
_side_note_end


Phantom types could be a very powerful API building tool.   
I am sure you can think about many other interesting use cases, ... like state machines[^state].

[^state]:  As an example, office.js is very stateful. It has _uninitialized_ state known to cause
problems, there is the application state (e.g. user is writing new email), and much more. My experience with 
office.js is that the code I write is very sensitive to where is placed and can be very brittle.  API like this could be made both safe and self-documenting by using phantom types.

## Next Chapter

I want to talk about recursive types and type level programming.  It will be more of a review of TS capabilities in these areas.

I need to take a break from writing posts. 
The next installment will take me longer, maybe a month or a little more, to finish.  
Thank you for reading.  Happy New Year! 

Here is the link: [_add_blank_target Part 5](2022-02-13-ts-types-part5.html).














