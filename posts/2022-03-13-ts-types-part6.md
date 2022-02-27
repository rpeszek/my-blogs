---
title: Type Enthusiast's Notes about TypeScript. Part 6. Thinking with and about Types 
author: Robert Peszek
featured: true
summary:  TypeScript Types series, final thoughts and rants 
toc: true
tags: TypeScript-Notes
codestyle: ts
---

_Please Leave Feedback in: [_add_blank_target git discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)_

Previous post: [_add_blank_target Part 5. Advanced Types](2022-02-13-ts-types-part5.html).   
Back to the beginning post: [_add_blank_target Part 1. Typing in Anger](2021-12-12-ts-types-part1.html)

**DRAFT version** _(I am sorry about any misprints.
It seems I have goblins in my laptop that toy with me, remove or change words. 
When this note disappears, you will know that I gave up.)_   



## Nutshell

This post wraps up my series about types in TS.
In this series, we explored type-centric approaches to writing code and pushed TS to its limits, sometimes a little beyond its limits. 

In any mainstream programming language there is a small minority of users interested in using types. 
Similarly to the rest of the industry, this group is a very small subset of TS community.  Developers interested in types tend to be unappreciated and underutilized. 

Types in programming get very formal and are very interesting for mathematically inclined developers. 
Mathematical inclinations are probably a necessary condition for enjoying types. This partially explains why types are such a niche, but IMO there are other reasons I will try to discuss some in this post.

This post will discuss these aspects of types: types are

* About Clarity
* About Productivity 
* About Simplicity
* About Safety
* About Correctness
* About Maintainability
* Universal
* Unpopular

I will finish my series with a short rant about each of the bullet points.  This will allow me to revisit and summarize some of the things we have discussed in previous parts and mention a few things this 
series did not cover.
The topics are, obviously, more general that TS.  

Some readers may get upset or disagree when reading this post.  You may have valid reasons for disagreeing with me.  Please let me know what they are. 


## About Clarity

What are coding conventions and standards?  When I hear these terms being used, I know I will soon hear about code formatting and linting, importance of code comments, even things like readme files and git hygiene. 
However, I am unlikely to hear about types.  This phenomena has a term, it is called [Wadler's Law]() or [bikeshed]().  
It is not that types are not important, they are.  They are also harder to discuss.  

I have discussed using types to achieve code clarity in [referential transparency]() and [types as documentation]() sections 
of Part 2. Let's revisit the topic here. 

It is much harder to comprehend the whole program than it to comprehend its types.  Types provide a high level information 
about the program the way that theorems provide high level information about proofs in mathematics. 
Types can give a valid high level representation of the app.  Programs often can't, they often contain tedious details, performance
optimizations, lots of persisted developer's sweat.  

Computations have a formal structure and properties. Computations can (and should) compose. 
These aspects of coding are the focus of functional programming. Types have a synergy with FP. 
Types help express functional concepts clearly.  This series was not about FP but it was hard for me to completely stay 
away, the synergy is so deep. 

Advanced types come with a learning curve. 
It is important to acknowledge that clarity is subjective and can easily be replaced with confusion 
unless developers are familiar with the concepts. 

> _"WTFPM:  WTF Per Minute is an actual measurement for code value."_

I imagine some topics covered in [Part 5]() could have a high WTFPM number. 
IMO, types used in production projects should be accessible to the project contributors. This means pushing the envelope just a bit but not too far.  

The following subsections examine a few concepts related to clarity.

### Declare function return types

Why would you not do that?  Add return types it at least for the module exports.  

### Clarity vs encapsulation

Encapsulation does not help clarity. 
I consider encapsulation to be a very useful when designing micro-service APIs, not when designing programs. 
Encapsulation often means not expressive types.  Encapsulating is hiding things from the types. 
It often makes types simpler than they should be. 
To get benefit of types, we need to give them a chance.  Type checker will not type check what is opaque to it.

On the other hand, explicit types have a maintenance cost.  
I like to compare this to documentation.  If app functionality changes
you should change the documentation.  You are likely to do that in the most obvious places only. 
Explicit types are different, they create domino effects and you need to propagate the changes to all relevant places.  
This overhead is not always desirable and there are patterns and tools to minimize the cost. 
One such tool is ad-hoc inheritance (in TS this would be controversial one known as subtyping).

Explicit types document the app ([Types as Documentation in Part 3]()).  
Encapsulated code does not test well and often requires mocking frameworks.  You will know you are doing something
right when you stop using mocks for unit tests.


### Referential Transparency revisited

Part 2 has discussed it already [here]().  I want to return to this topic for a quick rant.  

Consider these versions of code that suppose to establish a WebSocket using some imaginary API 
(we are implementing a PetStore):

```JavaScript
//gets config from global place and globally stores WS connection
const init: (): void = ...

//gets config from global place
const connectWs: () => WsConnection = ...

//gets config from passed parameter
const connectWs: (conf: PetStoreConfig) => WsConnection = ...

//gets config from passed parameter (most likely will involve subtyping at usage point)
const connectWs: (conf: {loggerConf: LoggerConfig; wsUrl: Url}) => WsConnection = ...
```

None is really reverentially transparent but there is a clear difference in the amount of information provided by the types.
First and second are clearly not reverentially transparent, 
third is more reverentially transparent but not all `PetStoreConfig` is relevant.
Last is very explicit, subtyping is likely to be used at some point as `PetStoreConfig` probably will be passed to it. 
IMO last is the best.  However, the first approach is the most commonly used at large (possibly with a more explicit name).


One great thing about referentially transparent functions is that they have clear input and output types. 
A referentially transparent function that returns `void` can't really do anything. 
Referentially transparent computations have types that do not lie.

Each computation has input and output even if TypeScript / JavaScript code does its darnedest to hide it.
TS code can pull the inputs out of thin air (configuration, stuff stored on the window object, etc) and
sink the output by saving or sending it somewhere.  The above `init` is guilty of both of these felonies. Still there is a referentially transparent computation
hiding somewhere.  In the above example the last `connectWs` type describes the inputs and output within the heavily encapsulated `init`.  

Inputs and outputs are essential to the clarity. The idea is to at least try understand what these inputs and outputs are.  Ideally, the referentially transparent computation within can be factored out
and made explicit.  It is not just clarity, you are likely to find future uses for it (e.g. the last example above could be factored out of the PetStore and used in other apps).  And it will be easier to test.  

### Variables named `x`

Are variable names essential to clarity? 
It is a bit of a telltale.  One of the most common criticisms of languages that heavily use types (like Haskell) is:

> "Whats up with the variable names, why everything is `x,y,z`, `xs,ys,zs`, `f,g,h` or `a,b,c`?"

Part of the reason is that the code can be very general. If a variable can be literally anything, why not call it `x`?  The other reason is where the reader gets the information from:
variable names or types?  Personally, I believe in more explicit variable names when implementing a specific business logic and the implementation is long.  However, even in such cases, the info should be in the types.


### Enums

Another bit of a telltale.  Are enums clearer than literal types?  This code has 3 types ("foo", "bar" and the union):

```JavaScript
type FooBar = "foo" | "bar"
```

This code defines one type (`FooBar`) and is more verbose:

```JavaScript
enum FooBar {
  Foo: "foo",
  Bar: "bar",
}
```

what is the advantage of using this `enum`?  I do not see safety advantage, please let me know if I am missing something.
This does not add clarity or readability either.  String literals are much more readable.

I think `enum`-s are used because they are a familiar translation of a concept from other programming languages. 
I this series I am suggesting building up cross language prowess based on types not on habits developed in other languages.  This would suggest preferring the first example.



## About Productivity 

When I write code in an untyped language, I still think about types, 
the only difference is that I do not have any verification from the type checker.  
Not having a type checker or working with poorly designed types slows me down. 
Moving to a strongly typed functional language has made me much more effective, possibly 4x-5x times more effective (I am sure such stats are very personal and depend on many factors). 

IMO all developers, wherever they admit it or not, use types in their heads.  The question is: how effectively?

The following subsections examine some concepts related to productivity.

### A walk in the park 

Types can guide the process of writing code.  I can write code by 'following the types' if the API gives
me well designed types to follow. The analogy is following a path on a walk in the park.  

We have seen examples of this in [Part 2](), where I twisted _office.js_ arm to get the types right and was able to 
type predicate myself to a much faster to write and safer code. 

We have also seen this is [Part 4](), where types formed jigsaw puzzles allowing the computations to fit together in only certain ways. 

_side_note_start
There is a technique often called a _Hole Driven Development_ in which the developer interacts with type checker
to write code.  You can try to use the [type hole _()]() with mixed success in TS to accomplish some of it.  
The idea is that by examining the type of a still missing code (the hole) you should be able to figure out the right piece of the puzzle to fit in (replace that hole with it). The language that provides the best experience (and a lot of fun) doing this is Idris. 
You can implement certain functions by just using keyboard shortcuts to deconstruct, pattern match, search solution space for the right function in scope and insert it to you program[^idris-youtube].   
OK, TS does not do that, but you do not need such tooling to be effective in assembling jigsaw-ed types.
_side_note_end

[^idris-youtube]:  You can see some of it in this [youtuble]()


### Inference reversed and T(ype)DD

Types are not just there in the background. It is much more efficient to design the types and take a walk in the park to implement the program.  
This claim is true in any language with a decent support of types, that includes TS.  The rest of this section
is not as much TS related, but I think interesting to note.

Type inference allows a programming language to compute the types without needing the developer to specify them.  
Ideally, the future will bring tooling where the developer defines the types and the compiler computes the program.   

_side_note_start
A lot of this unicornish utopia is available today in some FP languages like Haskell.  Certain code is consider boilerplate and the tooling can derive it automatically.  Examples are: equality, ordering, JSON parsing/formatting, `map` functions for non-list types, folding/unfolding for non-list types, traversing non-list types, recursion scheme folds and unfolds, optics...  All of this boilerplate would be available for free for something like the `Json` grammar example from Part 1 and Part 5. 
Programming in Haskell often involves creating some involved custom type and automatically deriving a lot of boilerplate for it. 
The stronger the types, the more code generation is be possible (I have already mentioned interactive code development in the dependently typed Idris). 
  
The moral of this story is that you need powerful types to be able to eliminate certain boilerplate.  Powerful could mean lots of things, e.g. _algebra of types_, _higher kinded types_, all of these things are offered by niche programming languages only.  
_side_note_end

Starting with types and following on with programs (TDD with T meaning _type_) is a very productive way of writing code, that includes TypeScript. 


## About Simplicity

I consider the terms _simple_ and _easy_ to have different semantics.  Easy: The low barrier to entry (e.g. language).
Simple: Low effort to reason about (e.g. code written in that language). 
There is no free lunch, to get simplicity you need to accept that things will not be easy.  
IMO, the popularity of easy and the unpopularity of simple are a systemic problem in programming and elsewhere.  

Types do not have a good synergy with OO.  Sutyping was invented to formalize OO, but subtyping concepts are 
very complex.  Even language designers often get them wrong.   
Frankly, I was shocked abut the amount of inconsistency and arbitrary complexity I have found in TS when preparing this series (see [Part 3]()).  TS is obviously not alone in this. Java design is very complex, C++ is famous for its arbitrary complexity (and the list goes on).  I am concerned that TS complexity (high already) will keep increasing.  

Simplicity is about ability to reason about.  Simplicity is very much related to [clarity]() as it is _easier_ to reason
about clear definitions.  Simplicity is also related to productivity and code automation. 

One aspect critical to simplicity that is _easy_ to explain and one that we have not discussed yet is _totality_. 

### Partial vs Total

Another related term is non-termination.  Does the function return a value as expected? 
Bunch of things can go wrong:  function can throw exception, loop forever, have unbounded recursion, 
return unexpected null[^nullsafe].  Functions that return result for all inputs are called _total_ otherwise are called _partial_.

[^nullsafe]: In TS, of course, we have ability to configure compiler to verify null safety. 
In some languages (e.g. JS) you also get partial function by writing code that in certain cases simply does not return.
TS is good in preventing this situation, for JS mishaps like this it will return `undefined`.

Total is simple.  Reasoning about partial functions is much harder. Any non-termination bypasses type checker. 
_Using partial functions means types are misleading._  

_side_note_start 
In general case verifying termination is _undecidable_ (it is impossible for a compiler to do that for an arbitrary program).  This is the famous Turing counterexample to Hilbert's [decidability problem](https://en.wikipedia.org/wiki/Entscheidungsproblem). You may also know is as the [halting problem](https://en.wikipedia.org/wiki/Halting_problem). 
However, interesting things can be done and languages like Agda, COQ, Idris can guarantee totality[^proofs].  
Outside of this small set of dependently typed languages, the totality is something that developers need to try to enforce on their own (that obviously includes TS). 
_side_note_end 

[^proofs]:  These languages can be
used as proof assistants.  One could prove anything (including falsehoods) using, say, unbound recursion. 
Proof assistant that does not check totality would not be assisting well, would it?  
Side note: When using proof assistant, you are proving a type and you are proving it by implementing it.  
This equivalence has a name: Curry-Howard correspondence. "Propositions are types, proofs are programs". 

Exceptions seem to be the most frequent reason for non-termination. Developers who like types avoid throwing exceptions, they will favor TS union types instead[^rust].

[^rust]: I like what Rust has done, Rust uses union-like types to covey errors, equivalent of throwing exception is using
Rust's _panic_, but you can't recover well from a panic so you probably will not overuse it.

Let's think again about computations from the input-output prospective and consider conditional control flow of the program.   
TS's ternary can be viewed as a function.  _if-else_ not so much. _if_else_ does not have a type. 
It was designed for mutating things and in today's more immutable approach to programming it should feel antiquated. 
However, it is idiomatic to JS and TS and impossible to avoid.   
I use _if_else_ blocks only with return statements (with exception of `void` functions).  I do not use _if_ without the matching _else_ even if the code looks repetitive (again, with exception of `void` functions).  
If you think about the "referentially transparent computation within",  you will notice that _if_ without _else_ is partial.  Several programming languages offer _if_else_ syntax without the _if_ only option.

I fully expect some pushback on this.  This view is also opposite to what you can frequently find on the internet (_else_ is sometimes considered evil).  Developers consider a sequence of _if_-s better than _if_else_ chains.  
For simple control flows it should not matter.  For more complex code using partial approach is concerning.   
This is also another example relevant to WTFPM measure discussed above.  


## About Safety

Here are some interesting examples of safety that could be provided by types: safe routing in a single page app (no broken routes), safe use of environment configuration,  safe backend communication (imagine the same types in frontend and backend with safety prevening broken urls and payload parsing errors).    
Safety can be very interesting, we have seen some examples like [this]() in Part 4.  

_side_note_start
Here are some very sophisticated examples of safety (outside of TS scope): safe linear algebra (e.g. consistent sizes of matrices in matrix multiplication), safety preventing deadlocks, safe resource management (e.g. no memory leaks, type safety over which resources are used, etc.). 
One of the wildest type guarantees I have encountered was a guarantee for a linear computation cost. 
_side_note_end 

Safety is really needed and often missing.  Here are some examples outside of TS scope:  
I am encountering `NullPointerException` (Java) even when interacting with some banking sites. 
Resource management issues are another great example (C++): 
[Microsoft eyes Rust](https://visualstudiomagazine.com/articles/2019/07/18/microsoft-eyes-rust.aspx), 
[Security bugs in Chrome](https://www.zdnet.com/article/chrome-70-of-all-security-bugs-are-memory-safety-issues/).

To summarize what has been said:

*  Best programming practices are not good enough to avoid these problems
*  Partial solutions (like smart pointers in newer versions of C++) are not good enough either
*  Type safety: works

We are seeing a slow industry shift towards more sophisticated use of types,  IMO, TS plays a role in that shift.

### Monads

I have stayed away from the topic thinking that there are enough many monad tutorials already, but it is hard to not mention
this concept.  Monad types provide interesting safety: monads can control the ability to leave monadic computation.
A value can easily enter monad but once there it is hard to leave.  This is clearly interesting 
from the safety standpoint and can be used to achieve all kinds of interesting guarantees. 
Things get really very interesting in the puzzle building department with the addition of dependent types[^idris].

[^idris]: [TDD in Idris]() book contains some very interesting and accessible examples of monadic computations in dependently typed setting.

Monads allow for very imperative code. However, this requires some syntax sugar that the programming language needs to offer. 
This is called _do notation_ in several languages or _for comprehension_ in Scala.  TS does not offer it. 
That makes monadic computing far less accessible.  
I am not using monadic computing in my TS project (even though Haskell development is my main job function).  
Each project needs to decide on the level of abstraction it allows to make developers working in it productive.   

[_fp-ts_]() library provides support for monads and other functional types in TS. 
Thumbs up to all developers who use it or work on _fp-ts_. 

## About Correctness

This series is not about formal verification, types and correctness could be its own blog (or book) series, one I am not qualified to write. 
I will only point out that, gradual typing or not, in TS correctness and soundness are a baby thrown with the bath water. 
A programming language could do a lot of code generation and optimization if it can assume certain things are true. Soudness is needed for these assumptions to hold water.  
These goals may be too ambitious in a gradually typed setup. 

However, making things conceptually easy at the cost of correctness (e.g. incorrect variance, unclear narrowing semantics in TS) should not be on the table. 


## About Maintainability

Considering who is still reading this, I am preaching to the quire.
Software, like houses, need a solid foundation.  You cannot divorce maintenance cost from [correctness]() and [safety]().  
I attribute this quote to Paul Phillips (former Scala compiler contributor).  The quote is from memory:

> "Nothing here is obviously wrong.  What is wrong is not obvious."  

Are not obvious bugs that are less frequent and affect fewer users less costly?  

It is well known that types can prevent trivial errors (like using a string instead of object). It is hard to catch all such cases in tests and they do show up in production. This is the reasons, I believe, TS is used in most of its projects.

Types simplify refactoring. Good type design makes refactoring process very safe.  Developers can come to TS for trivial type
safety and can discover other benefits, like a much safer refactoring process.

Types can simplify adding new functionality.  If you think about app as a big union type of various requirements (this is oversimplification but let me keep going), then adding handling of a new piece of functionality to that union could give you compilation errors unless you fix all the relevant places. Think about TS-s [`switch`]() or
[_ts-pattern_]() library exhaustive checks[^product].

[^product]: It is good to note that this safety is unique to union types, you will not get the same safety when 
adding a property to an object.  It is interesting and telling that the industry is adding co-product types to programming
languages just now.


## Universal

Types are more fundamental than a programming language. 
For example, most FP languages are effectively a syntax sugar over some version of lambda calculus. Lambda calculi come with very well understood formalized type semantics.  
I am reminded about a great presentation [Phil Wadler on ]().  It makes a compelling and funny argument that the movie _Independence Day_ got it all wrong.  Aliens would not have used Java.  Java is being created by an engineering effort, types and LC are being discovered[^discovered].  Aliens would have discovered typed lambda calculi or have engineered something much different than C or Java.  
This is very philosophical, but it has a very pragmatic implication.  Discovered programs are, by definition, timeless.  If Wadler is right (and if we will keep programming in the future) that would be kinda amazing.  
In Part 4, I have referenced the [TAPL]() book, IMO, the best textbook to learn about types. This book is 20 years old. 
Recursion schemes (Part 5) are 20+ years old.  Rank-2 disguised in Part 4 was studied in 1980-ties and 90-ties.
Many language features we consider new and modern are really old ideas, some date back to 1970ties.


[^discovered]: A similar and relevant philosophical discussion has been happening in mathematics for centuries (see [wikipedia](https://en.wikipedia.org/wiki/Philosophy_of_mathematics#Mathematical_realism)). My opinion on this is that creative process tends to be iterative leaving historical evidence of iterations. Mathematics, for most part, has been additive.  There was rarely a need to rewire an old theory. As far as I know not in last 100 years. 


Robert Harper has coined a term [The Holly Trinity of CS](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/) and types are one of the three.  

Types are playing increasing role in foundations of mathematics, the relatively new and "hot" topic is [HoTT]().

This series was written by a TypeScript newb.  I am using TS since November 2021 and only on one project.  
We have covered a lot of ground that probably is not well known to many seasoned TypeScripters.
I think the existence of this series provides a good verification for my claim:  it is not about knowing a programming language, it is about knowing the types.

Types could unify how we think and talk about programs. Everyone knows that development teams need to be small.  Somewhere around 4-5 is the threshold.  Why is that?  I had worked once inside a team of 8 (two teams with different core competencies were merged to work on a new project).  Design meetings, OMG, we had hard time agreeing on anything.  There seems to be lack of agreement on what is important.   
Nobody disputes that natural numbers satisfy `2 + 2 = 4`, and well that is a type. 
One of my goals in this series was to sell the idea that types are fundamental to programming and are mostly not something open to endless debates.  Types could help facilitate agreement.

### Advanced Types as Patterns

Advanced types are worth learning even if TypeScript is not able to support them.  Advanced uses of types often come with 
very well behaving principled computations.  TypeScript may not be able to express such types in full generality, but it is
often possible to use the principled approach as a pattern.  Example is the [Part 5 Recursion Scheme](). 
Readers familiar with the concept may notice that _lenses_ are another great candidate for a principled pattern (instead of using a lens library code lenses by hand).  `map` is being added to all kinds of types as a pattern.  
Monads are used as pattern too. Language level _async_, _await_ adopts more general monadic computing, it is special cased to async computations. _fast_check_ library uses some monadic computing as a pattern to accomplish randomized property testing. 


## Unpopular

There are two directions to writing high quality low defect rate software.
Both approaches complement each other.

1. Increase project effort and cost (e.g. testing)
2. Increase effort / cost outside of project scope (e.g. learning types)

In the industry focused on short term goals 2 will be unpopular even if benefits of 2 are significant.  
The ramp up time for any new project needs to be short. 
This explains why all mainstream languages look and feel alike. As far as programming languages go, software industry is not innovation friendly. Any progress needs to be very gradual. Developers need to be able to "hit the ground running" when
using a new language.  

Phil Wadler made an interesting observation in the context of language design called [Wadler's Law]().  This observation is also known in a more general form as [bikeshead]() and I have mentioned these terms already.  Types are about semantics.  That puts them at the far end of the popularity ranking scale.  
I have mentioned the easy vs simple dilemma.  Simple is less popular.  
Types are theoretical,  that makes them less popular as well.

I can think about 2 approaches to becoming a better coder: hands on programming (project work) and linear learning (e.g. reading textbooks). Ideally both approaches are combined and feed into each other. A mastery of types needs both. 
This is not what is happening, the second element is missing. Did you work in a place that encouraged developers to read a
theoretical book?

Let's look at what the type friendly job market looks like. 
The job market for typed functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust top the list for the weekend use based on stackoverflow surveys[^weekend].  
How are both possible?  One issue is that only a small minority of programmers interested in the more
principled methods of writing code. 
There needs to be a critical mass of enthusiasts and there isn't one.  Managers will consider use of an FP language risky. 
You do not get fired or criticized for selecting Java.   
The other issue is the correlation between interest in things like types with interest in mathematics. 
Current rush towards machine learning sways the precious few mathematically inclined CS students towards well paying datascience.   
Yet another issue is education and how mathematics and CS are being taught. 
  
[^weekend]: I remember, Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. I found this link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  These stats are hard to find but I also found [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/). In 2019 Rust moved ahead of Haskell.  
At the same time job ranking (based on UK's [_add_blank_target ITJobsWatch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do),  I have
not found similar ranking for US.)  puts Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017.


__Let's take a bit more controversial take on this.__ 
A stronger version of "Someone is wrong on the internet" is _Everybody is wrong most of the time about everything._ 
If you assume this to be true, you can view the progress as a process of being less and less wrong.   

A milder version of it is: _Lack of popularity is a necessary but not sufficient condition of doing something right._  
"Popular => wrong" is a law of life that dates back to at least Socrates.  
So what are some good examples of something that we know is right?  Here are some candidates:

_2 + 2 equals 4 (for integers)_   
_"hello" is not a number_   

Do we agree that these are true?  Hold on what are these things.  The possibilities are:

1. Mathematical statements
2. Statements about programming
3. Statements about types
4. Types 

The only one in this bunch that is popular is the number 2.  Popular programming languages (like TS) will allow me to write code that invalidates these statements.  Sound type system implies a niche programming language. 
Types, soundess, these are all nerdy things.   
You probably still think of this as to hyperbolic. If I am wrong we will question and push on status quo and it want budge, if I am right we have a chance of learn something. 

Lack of popularity can translate to some __frustration__ when using types. The frustration comes in the form of rejected designs, rejected pool requests. I heard stories and experienced some of it first hand. 
That is just part of life, the criticism can have validity as more advanced programming techniques could make the project
confusing and not accessible to its contributors.  
It also should be expected. One comment about this series said "this stuff is different than what we do". 
Different could imply worth pursuing but unavoidably will, at some point. result in a confrontation. 
Types lack the critical mass of acceptance to become disruptive, they work well on a small scale and/or when applied in a very gradual way.  

Thumbs up to projects and developer teams who learn types and select the unpopular!


### Gradual Progress

There is gradual progress. Mainstream languages are introducing a bit of types and FP.  
`async`-`await` is now supported by many languages. 
_Sum/variant types_ are supported in some way by many languages (TS's union types stand out for their readability).
Record types are being introduced as well (e.g. Java 14 _records_, C# 10 record struts, ...). 
C# has type safe `equals`.  Advanced types in TS we have discussed in
[Part 4]() and [Part 5]().  The list goes on...   
Mainstream is slowly incorporating some types and some FP.   


## Final words

TS does a poor job implementing types.  However, it has types and it even allows to do some advanced things with them. 
The last two installments ([Part 4]() and [Part 5]()) allowed me to go places I would not be able to reach in most mainstream languages.  

If developers start using types, the languages will expand support for them.  
This will feed some gradual change. 
The hope for existence of such a feedback loop is what prompted me to write this series.  
My hope is that mainstream programming will eventually transition itself to more principled programming approaches.  

__Should more advanced types be used in a project?__  Ideally (and IMO) that decision should made by the developers.  I have presented plenty of _pros_.  The biggest obstacle is the learning curve.  I am afraid this learning needs to happen outside of the project work.  In reality, this means that the 
decision has to be made based on what the team knows already.  So the answer for some teams could _yes_ today, for some could be _no_ today but a _yes_ in a future.  
My personal approach is to make sure that TS code is approachable and my goal is to make it principled within this constraint.  It is good to be able to scratch the itch somewhere, I have my backend work to do that, lots of people can't do that.

Thank you for reading.  Please let me know your thoughts.  
Good luck with your projects, I hope you will use types!
