---
title: Type Enthusiast's Notes about TypeScript. Part 4. Programming with Type Variables
author: Robert Peszek
featured: true
summary:  TypeScript Types series, higher rank types, existentials, phatom types, preventing type widening
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 2. Typing Honestly](2021-12-24-ts-types-part2.html).

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
When this note disappears, you will know that I gave up.)_   

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

This is the forth post in the series devoted to types in TypeScript. In this series, I explore type-centric approaches to writing code and push TS to its limits in doing so. I am writing these posts for like minded developers who are interested in types and either use or consider using TypeScript.

This post will be a little more advanced and will focus on programming with type variables. 

[_add_blank_target Types and Programming Languages](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) is 
the book about types I recommend to everyone, ... so far unsuccessfully.   
Reading TAPL will be a big eye opener for many developers.   
Apparently, software developers want to have some life outside of programming. Who knew?   
The good news is that types dramatically increase programming efficiency so learning them is a good investment.   
This section of the post will be a little more TAPL-ish with some more advanced CS.
Nonetheless the topics I am about to present are IMO very practically useful and I will try to do my best to present them in a digestible way. 

I will discuss type variable scoping, rank-2 types, and existential types. Some of code examples show a level of safety I did not expect to be able to pull off. 

Before we start I need to build up some tooling.  We will start with a tiny bit of type level programming 

## Type safety preventing `unknown`

In previous posts, we have seen examples TS compilation issues caused by widening types to `unknown` instead of reporting a 
compilation error.  Interestingly, TS allows enough of type level programming so we can try to fix such issues ourselves.  

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

E.g. in my first post, I had an example of a email body type inferred as `unknown` instead of a `string`.  Wrapping such code
in `verifyUnknown(false, body4)` would have alerted me with a compilation error.   
You may point out that a much simpler solution was to just type annotate `const body4: string`. 
I completely agree, but having this done more generically is useful as we will see shortly. 

Here is a short TAPL-ish explanation of what just happened. TS allows me to use type level ternaries that map types. 
`IsUnknown<T>` is a type level function (TAPL'sh term for this is _type family_) that maps types `T` to literal boolean types `true` or `false`.  Since these are TS literal types, I can also use them in value level program so I can just write `verifyUnknown(false, someExpression)`.
TS will figure out that `false` means the second part of type level ternary and, thus, that `unknown extends T` is not true. 
Hence `T` is not `unknown`.  

I will use `verifyUnknown` to do some cool type level trickery. 
Oh, and you may be wondering if we can extend this approach to other types, not just `unknown`.  I will get there in this post as well.




## Type variable scoping 

The scoping happens has more than one aspect. Let's start with the most obvious one.  The type variable being visible inside of the implementation body.  This is simple stuff, I just want to share an obvious gotcha that got me at some point:

```JavaScript
export const bodyScope = <T>(value: T | undefined | null): void => {
    if(value) {
        const t: T = value //you can access type variables in function body
    }
}
```

This approach to defining type signatures (actually my preferred way to write function type signatures)
puts `T` out of scope:

```Java
export const bodyScope2: <T>(_: T | undefined | null) => void = value => {
    if(value) {
        const t: T = value  //Cannot find name 'T'.ts(2304)
    }
}
```

For the type variables to be visible in the implementation they need to be on the _RHS_ of `=`.

The other type of scoping is much more interesting.



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

but will not do that here as these tend to be less practicallly useful. 

Here is how I think about it:

_Higher rank means first class generics_



## Existential types

In TAPL-ish it is called _existential quantification_  and it has to do with the ownership of definitions. In OO you would say "code to interfaces, not implementation", it is also related to the OO concept of _inversion of control_.  Here is how the story goes.



### Replacing factory pattern 

```JavaScript
//Factory pattern replacement
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

What we want to be able to hide which implementation of `Foo` we are passing to the callback.  
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

This simulates what is called an existential type.  A function that accepts a callback owns the definition of the exact type
that is passed to the callback.  The callback itself needs to be generic and accept any possible implementation.  
Note the scoping of `T` inside the type defining the function parameter. 

This _inverts the control_ from the implementation to the caller.

_side_note_start **Note on terminology:**  Repeating the above definitions 

```JavaScript
declare function fn1<T> (f:(t:T)=> void): void 
```

The `fn1` needs to be defined for all possible types `T`.  The name for it is _universal quantification_.  Some languages
even use `forall` keyword to describe it.  

```JavaScript 
declare function fn2(f: <T>(t:T)=> void): void 
```

`fn2` function parameter `f` needs to be defined for all possible types `T`. 
However, `fn2` can pick whatever type it wants for `T` and use `f` with it.  
Another words, there exists some type `T` that will be used but `f` has no way of knowing which. 
The name for it is _existential quantification_.  Some languages
even use `exists` keyword to describe it.  

The general concept of existentials is broader than what I am describing here and what TS supports. 
However, this by itself is plenty powerful. 
_side_note_end


### Preventing information escape 

I am drawing a blank trying to think about an OO analogy for this.  
This example will accomplish more than the above 'factory' pattern and will not use any interfaces or classes:

```JavaScript
// Using higher rank to protect data
// Imaginary world without debuggers, JSON.stringify, etc
type Api = {getGoodies: string[]}

//some login function provides access to API, password needs to be protected
declare function login<Password>(p: Password): Api 

//provide password to a computation that compuatation should be able to use the password but not return it
const secretive = <R> (fn: <Password> (p: Password) => R): R  => {
   const s : any = "topsecret"
   return fn (s)
}
```

The example is somewhat contrived with the main goal of illustrating the point.   
This code exposes building blocks that work together without using interfaces, classes, or mixins.   
To get the access to the `Api` type, you have to use `login` and you have to use it inside the provided
`secretive` function. 
Working with code like this is like playing with lego and types prevent from jamming a square peg into a round hole.  
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

Unfortunately, `secretive(stealPassword)` compiles.  Typical of TS, if things make not sense, the compiler returns `unknown`. Hovering over `secretive` shows me this:

```JavaScript
//const secretive: <string[]>(fn: <Password>(p: Password) => string[]) => string[]
secretive(goodProgram)

//const secretive: <unknown>(fn: <Password>(p: Password) => unknown) => unknown
secretive(stealPassword)
```

That is why I have created the `verifyUnknown` safety in the above section:

```JavaScript
const valid = verifyUnknown(false, secretive(goodProgram)) //valid: string[]
```
```Java
//Argument of type 'false' is not assignable to parameter of type 'true'.ts(2345)
const invalid = verifyUnknown(false, secretive(stealPassword)) //does not compile!
```

This creates some interesting safety.   Obviously you could still do a lot of mischief if you wanted to. 
There is a need for some 'gentlemen's agreements' to not use casting, `JSON.stringify` etc. 
However, if you think about creating clear contract APIs, this approach can be very powerful. 

Existentials are not exactly equivalent to OO.  However, using existential types can often accomplish a lot of the same things and often in a cleaner way.  I have not played enough with these in TS to give you a list of gotchas.  I do not know how robust this type of code would be. 

_side_note_start **Existential types at large:** 
Tricks that prevent types from escaping have lead to some amazing programming.   
For example, existentials are related to dependent pairs in depenently typed programming languages.  Dependent typing provides some very strong types. One example could be
lists with a statically checked length. You want to be able to use such lists when processing runtime data that can have arbitrary size.  The size information cannot escape the computation for the types to be sound.  

Another amazing example is an old (1993) code called _state threads_ (currently part of std base library in Haskell).  It allows to use local mutable state to define code that has to be _referentially transparent_ (I have discussed referential transparency in [Part 2]()).
This is possible because the ability to mutate the state cannot escape outside of the computation using it.  The API how to do that remains unchanged for 30 years, you can't improve on perfection.   

I see higher rank types, mostly rank-2 being used a lot.  Having ability to pass generic (polymorphic) functions
around is very useful. The problem of separating the 'interface' from an implementation, in the non-TS code I work on, is typically solved by defining an EDSL and an interpreter. Interpreters are polymorphic (generic in TS lingo) and rank-2 types have to be used to make them first class.
_side_note_end


## Safety preventing subtyping 

Here is the code, it combines the above `unknown` verification idea with existentials:

```JavaScript
type Same<P,T> = P extends T? (T extends P? true: false): false

const verifySame = <P> () => <T> (_: Same<P,T>, t:T): T => t


type Hello = {hello: string}

verifySame<Hello>()(true, {hello: "world"})
verifySame<Hello>()(false, {hello: "world", since : 2020})
```
```Java
//Argument of type 'true' is not assignable to parameter of type 'false'.ts(2345)
verifySame<Hello>()(true, {hello: "world", since : 2020})
```

You may have noticed a case of typing euphoria here.  I used rank-2 construction because it allows me
to type annotate with only one type variable which is nice.  However, this is often not needed. 

Here is an implementation that fixes variance on the array's `push` method:

```JavaScript
//Note to get safety I end up with casting, this is quick and dirty and can be done slightly better
// This cast could be an indication that we are changing how TS compiler thinks
const safePush = <P, T> (_: Same<P,T>, ps: P[], t: T): number => ps.push(t as any)

const intlist: number[] = [1,2,3]
const list: unknown[] = intlist
list.push("not a number") //unsafe TS push adds a 'string' to 'intlist'

safePush(true, intlist, 1) //this is safe
```
```Java
safePush(true, list, 1)    //this is risky and will not compile 
safePush(true, list, "not a number") //this is risky (here wrong) and will not compile 
```

The link github repo has an existentially typed version of `push` that just has one top level type variable.
That version is more cumbersome to use. TS ends up not working well with it as well. 

This approach is related to this old feature request:
[TypeScript issue 7481](https://github.com/microsoft/TypeScript/issues/7481)


## Phantom types

TypeScript is somewhat unique in supporting _Structural Types_. 
Types like `type Person = {firstNm: string, lastNm: string}` are structural. That means the name `Person` is only an alias, what defines the type is the RHS of the definition, not the LHS.  Contrast this with an OO class definition. Two structurally identical classes are still considered different types (this is called _nominal typing_).    

It is sometimes convenient to be able to define different types that share the same structure. 
_Phantom types_ is one of the ways to do that. We say _phantom_ because these types have no impact on runtime values. 

Somewhere around 2006, haskell wiki published a write-up about [phantom types](https://wiki.haskell.org/Phantom_type).  The write-up was expanded in 2010 to include a form validation example. Since then all blogs (in any programming language) about phantoms show a validation example.  I decided to be as unoriginal as everyone else.  This will allow me to focus on how this is done in TS.

My first attempt at phantom types in TS will fail:

```JavaScript
type NotValidated = {type: "notvalidated"}
type Validated = {type: "validated"}

//Extra phantom type variable 'T' 
type Person<T> = {firstNm: string, lastNm: string}

//For simplicity this is just a string
type ValidationError = string

//Some way to validate person, this example assumes error thrown if validation fails
declare function validate(p: Person<NotValidated>):  ValidationError | Person<Validated> 

//Function to be used only if phantom 'T' is 'Validated' type 
declare function doSomethingValidated(p: Person<Validated>): void
```

this does not work in TS. The following code complies:

```JavaScript
function validatedOrNot<T>(p:Person<T>): void {
    doSomethingValidated(p)
}

function notValidated (p:Person<NotValidated>): void{
    doSomethingValidated(p)
}
```

The fix is to provide a value level information about 'T` in an optional property.  
This type definition replaces the one above:

```JavaScript
type Person<T> = {firstNm: string, lastNm: string, phantom?: T}  //adding value level representation `phantom?: T` 

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
// Argument of type 'Person<NotValidated>' is not assignable to parameter of type 'Person<Validated>'.
//   Type 'NotValidated' is not assignable to type 'Validated'.
//     Types of property 'type' are incompatible.
//       Type '"notvalidated"' is not assignable to type '"validated"'.ts(2345)
function notValidated (p: Person<NotValidated>): void {
    doSomethingValidated(p)
}
```

I believe phantom types are used by some FP libraries in TS, e.g. _fp-ts_, but using somewhat different techniques. 
My goal was to present the language ability to do phantom typing and use the simplest approach I can think of from the presentation standpoint.

Phantom typing can be used to do a lot of crazy type level stuff.  The most wild use I have seen is (this uses Haskell)
[ghosts of departed proofs](https://iohk.io/en/research/library/papers/ghosts-of-departed-proofsfunctional-pearls/).  
Here is an easy to understand example: think about a non-mutable list, your function accepts a list and does something with it, 
your code needs the list to be sorted to work.
You can encapsulate this and conservatively sort it just in case,  you can document your function by saying
that it is the caller responsibility to sort, ... or you can introduce a phantom type:

```JavaScript
//Sort status as a phantom type
type List<T, SortStatus> = ...

interface Comparator<T> {
    compare (o1: T, o2: T): number
}

declare function sortAscending <T extends Comparator<T>, AnyStatus> (list: List<T, AnyStatus>):  List<T, "ascending">

declare function doSomethingWithSortedList <T extends Comparator<T>> (list: List<T, "ascending">): void
```

I do not use if much in my non-TS work because of access to somewhat more powerful typing tools like GADTs. 


## Next Chapter

I want to talk about recursive types and type level programming.  It will be more of a review of TS capabilities in these areas.
I am having second thoughts about including a subtyping note
(we talked about subtyping in the previous installment already). 
It will take me longer, maybe a month to publish it. 












