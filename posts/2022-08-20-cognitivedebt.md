---
title: Cognitive Load and "Debt" in Software
author: Robert Peszek
lastmodified: Aug 20, 2022
featured: true
summary: TODO. 
toc: true
tags: Haskell
---

In this post we will look at software development through a lens of cognitive load.

This post presents thoughts on how programming projects accumulate cognitive load.  How a cognitive overload (we call it a bug) happens. I will also talk a little about stress, negativity, and other impacts of professional programming on our mental state.  
I will talk about cognitive load of functional programming.

I want to better understand other programmers. 
The discourse between practitioners of functional and imperative programming is not always friendly and I am trying to understand that as well. 

This post is a high level rant discussing programming across the industry spectrum from JavaScript to Haskell. 
The topic is not straightforward and even counter intuitive.  Some of the things we will consider feel outlandish even 
to me. 

I am not a physiologist, these are observations of a coder.


## Motivation

I am perusing thousands of lines in Infrastructure as Code (IAC) yaml files. I am looking at a refactored and improved version. It is tons of templated _yaml_ of k8s configuration at my work.     
Would something like _Dhall_ be a better choice than _yaml_?
You can learn _yaml_ grammar in a few minutes, _Dhall_ will have a taller learning curve, especially if you are new to FP. 
Are thousands of lines of templated yaml with lot of code duplication and no type safety simple to grasp?  Clearly not.     
I can argue that the overall complexity of simple yaml files outweighs the cost of learning a nice configuration language like Dhall. 

But is this that simple?  IAC configuration needs to be accessible to all developers in the team.  
The term "code readability" comes to mind.  This term is typically used as a `const boolean` as if code is either readable or not.  
In reality, readability depends on the reader.  For some developers Dhall is much more readable than yaml.
For others Dhall is not accessible.  So all of this is a bit tricky and has many dimensions. 

The term _Cognitive Debt_ is intended as a pun on _Technical Debt_.  I intended it to mean:

>  &emsp; _Negative impact on the team's ability to understand and reason about the code_

This term also has a clinical meaning.  Cognitive Debt is a psychological term associated with _repetitive negative thinking (RNT)_.
Cognitive Debt and RNT are hypothesized to have some very negative health consequences that can lead to depression or even dementia. 
RNT is described as “excessive and repetitive thinking about current concerns, problems, past experiences or worries about the future”[^RNT].  
I do not claim to know a lot about clinical psychology but the definition clearly is very relevant to programmers and could explain why programmers are often unhappy, why programming discussion groups (e.g. on reddit) are often very negative, etc.   
Sadly, RNT seems to be the psychological condition that really good programmers are likely to experience.  Good programmers think about rainy day scenarios, notice design flaws, can anticipate program issues.  
 
This is, obviously, not good.  It is important that we talk about it and find ways to deal with work induced stress and mental state issues.

[^RNT]: E.g. this article in [BMC Psychology 9](https://bmcpsychology.biomedcentral.com/articles/10.1186/s40359-021-00696-2).  

RNT is not the only thing that can negatively impact our mental state, I doubt it is event one of the top contributors.  Cognitive load can generate stress, communication barriers are sure to generate both stress and conflict, programming job can lead to cognitive dissonance, multiplicity of other factors like bureaucracy, inadequate resources, unrealistic timeliness will add to the mix.   

[^unhappy]: Interesting youtube: [Why Do So Many Programmers Lose Hope?](youtube.com/watch?v=NdA6aQR-s4U)

Cognitive load seems to be the most straightforward of the bunch.  Is cognitive load considered in software projects?  Let me ask the question differently.
Did you ever hear this about a coding task?

> &emsp; _"Yeah, I can do it but it will be complicated so I think we should not go there."_

Probably not ever.  But again, this is more complicated.  Imagine it was about changing these dreaded _yaml_  to _Dhall_?
How likely would that be considered too much?  In many teams, very likely.  You can argue that there is a difference in saying no to the product owner who wants impossible functionality and no to a developer who wants _Dhall_. 
True, but this can't be the whole story. 
So, let me start the conversation. 

## Types of Cognitive Loads 

Cognitive psychology defines cognitive load as the amount of information that working memory holds at one time. 
The idea is that the human brain is limited in that capacity. 
If you are into certain technical sports like skiing, swimming, golf, 
you may know first hand how hard it is to control 2 or 3 aspects of your body movement at the same time. This space appears to be quite limited. 

Sometimes very similar concepts can surface independently in different fields.  It is very intersting when that happens.  IMO this is one of these rare situations and the main terms in cognitive psychology map very well to terms programmers are already are familiar with. 
Psychology considers 3 types of cognitive load: _Intrinsic, Extraneous, Germane_.   

* _Intrinsic cognitive load_ is the inherent level of difficulty associated with a specific topic.  A rough moral equivalent known to software developers is _essential complexity_ (things are complex and there is nothing we can do about it).  

* _Extraneous cognitive load_ is generated by the manner in which information is presented.  It is is high, the rough moral equivalent in programming lingo would be 
the _accidental complexity_[^accidental] (things are complex because we made them so). 

* Germane cognitive load refers to the work that is put into constructing a long-lasting store of knowledge or schema.  Schema is a pattern of thought or behavior that organizes categories of information and the relationships among them.  As a programmer I like to think about this as the cost of learning _abstractions_. 
For OO programmer this could be learning the Design Patterns.
For an FP this could include understanding programming principles, types (my main interest), category theory, formal semantics, lambda calculi.  Other topics could include learning to reason about concurrency, algorithms, 
mastering agile process, devopsy practices.  UX, code design, architecture all have a lot of germane load.
This list is not exhaustive. 

[^accidental]:  Accidental and essential complexity have been introduced in [No Silver Bullet](https://en.wikipedia.org/wiki/No_Silver_Bullet)

Cognitive load theory thesis is about reducing extraneous cognitive load redirecting it towards germane load.


## Simple vs Easy 

My readers who looked at my [TS types]() series have seen me write about it before. 
I do not claim that my definitions are the only correct way that these terms should be interpreted.  However, I have seen other programmers use a similar disambiguation so I am including it here.  

I consider the terms simple and easy to have a different meaning. 

Easy: The low barrier to entry (e.g. easy programming concepts). Hard is the opposite of easy and implies a high learning curve.   
Simple: Low effort to *correctly* reason about (e.g. code written using learned concepts). Complex is the opposite of simple (e.g. code that is hard to understand).  The term "arbitrary complexity" fits this definition very well. 

Easy means low germane load, hard means high germane load. 
Simple means low extraneous load, complex means high extraneous load. 

This differentiation could be expressed as: 

> &emsp;  _Easy means low cost of creation, simple means low cost of consumption_

except, in this post my interest is the cognitive cost only not the total cost.    

Achieving _simplicity_ on a larger scale projects is not _easy_. Easy does not scale well.   
There appears to be no free lunch, cognitive load needs to be somewhere.
My big preference is trying to achieve _hard and simple_ rather than _easy and complex_. 
Another words, I prefer germane load over extraneous load. 
This, I am pleased to note, is inline with cognitive psychology.   
Again, the advice from cognitive psychology is to reduce extraneous cognitive load redirecting it towards germane load.
To put it using the proposed terms this translates to 

> &emsp; _Move from complex to hard_

An interesting way to look at easy vs simple is to think about a creative process like writing a book. 
Easy to write is clearly very different from simple to read.  
In programming these two are bundled together, a program created by one developer need to be consumed by another dev
who needs to modify it or interact with it. 
The term "readable code" comes again to mind. I consider it different form simple.  E.g. readable code does not mean no subtle bugs. Message is readable if you know what it conveys but what it conveys could be complex or even incorrect. 

IMO, the popularity of easy and the unpopularity of simple are a systemic problem in today’s programming and elsewhere.

You may disagree about my use of easy vs simple terminology. That is fine, however, it is important to distinguish between the cost of learning programming concepts (germane load) vs learning the codebase that uses these concepts (extraneous load). 


## Extraneous load that grows 

I was recently involved in a big rewrite of a JS application. 
It is one of these apps that can be described as: _was easy to write, is hard to maintain or understand_. 
I am sure very few readers will be surprised by existence of a hard to maintain JS application ...but let's put taking about it aside for now. 
Is writing "easy" code the same as generating excessive cognitive load for the maintainers?
I think it often can be, it is not that hard to incrementally develop a non penetrable maze. 
In fact this is what will happen by default. 
Maintaining some code structure to manage the cognitive load is not "easy".  
I am not a big fan of bureaucratic processes, but scrum teams could help here. If several developers work on the same code, PRs could help in establishing some base level of sanity.  But, IMO, that is not enough.    
Note, this is a different situation from the IAC example, the rationale for keeping yaml configuration was accessibility.
Here the accessibility was lost.   

Software is made out of many interacting pieces (think statements, atomic computations, even lines of code).
This collection will not be easy to comprehend just because the pieces are easy to comprehend. 
The biggest contributor to the overall complexity is the interaction between the pieces and organization of the pieces 
and not the pieces themselves.   

Mutating state is well known to be a terrible way to accomplish communication between parts of the code, keeping the state encapsulated inside objects (OOP idea) panned out to be not ideal either as encapsulation is just hidding the issue. 
Components encapsulating the state and interacting by sending messages[^msgs] scale much better.  FP is based on immutability and composability and it scales well too.  
My career worst in the mutation department was a Java Struts 1 code where a class had over 200 of mutating instance variables[^inheritance].  The mutating shared state was used religiously even if local variables would have worked just fine.  Changing order of 2 lines in this code was almost guaranteed to create a (typically intermittent) bug.     
This code used no advanced concepts, all ingredients were simple: control statements, instance variables, lots of protected methods with typically no arguments and void returns that read and mutated the instance variables.  I consider it one of the most complex programs I worked with in my 27 years of professional programming.  
This code become infamous for is complexity very fast.  Interestingly, Struts were blamed, not the needless overuse of mutable state.    
Mutating variables are OK in a simple app with just a few of them. We can tediously reason about correctness of such code.  This approach does not scale to more involved apps. 
Immutability is something that comes with a cognitive cost (germane load) but allows for project complexity to scale up. 


[^msgs]:  I believe this was the original idea behind OOP

[^inheritance]: actually, there were 2 classes with about 100 vars each, you got to use inheritance!

I call code without adequate abstraction a _brute force_. 
Brute-force is a code designed by the most prolific software designed in the industry called _Fate_. 
Such code might do a lot of things right, have no mutable state at all, only pure referentially transparent computations, but is is just a lot of pieces that strung themselves together overtime.  
Improving types can play a major role in making brute-force more attainable. 
This could mean a lot of things:
introducing type variables, identifying Functors, Foldables, Traversables, Monoids, ... (if this is FP), using generic programming (automatically deriving code from types if the tooling supports that), 
defining unique types, improving type safety in various ways.  
At the end the code will have a lot of explicit types to look at and reason about. 
I wrote about creating explicit types in the context of TS [here]().  
Still that could be not enough, programs benefit from some level of consistency and from using established concepts (e.g. maybe a streaming library?). Types can help uncover what abstractions will do the trick.  So here we go again:  Abstractions come with cognitive cost (germane load), this allows to control an otherwise wild overgrowth of code complexity.   

To summarize, when left to its own devices, code complexity will grow very fast.  
_Fate_ does not care about keeping extraneous load in check. 
We all know what to do to to fix it: we refactor often, ideally we refactor before the code gets unruly. 
But, refactor to what?  Where does the design come from?  This is mostly the germane load that already should have happened, 
probably outside of the project work.


## Hidden Complexity

Programming languages, programming frameworks and libraries often come with hidden complexity. 
It is a cognitive cost we often do not know exists.  
Programming tools can present an illusion of simplicity.  We think we understand them but we really do not. 
The term typically used to describe this is "gotcha". 
Gotchas are very hard to troubleshoot causing "mysterious" bugs, bugs resolved by using workarounds rather then identifying the root cause. 
These problems have high extraneous load and high complexity.   

TypeScript is very high up on the "Most Loved" list in the [2022 stack overlflow survey](https://survey.stackoverflow.co/2022/#technology-most-loved-dreaded-and-wanted). Surprisingly OCaml is more dreaded than loved and Haskell is just slightly more loved than dreaded. 
TypeScript is full of gotchas and unsound design choices. It has been described as ["powerful, interesting and messy"](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html) by its creators. 
I wrote a blog series [TypeScript Types](/tags/TypeScript-Notes.html) presenting a lot of complexities and gotchas that probably surprise experienced TS developers. 
Why do we love TS? 
TS has a relatively low initial learning cost if you know JS.  ...But wait, we are talking JavaScript here, the language
where `[] !== []` and `[] != []` <nobr>but `[] == ""`</nobr>.  
Does "know JS" imply the full understanding of JS idiosyncratic design?  Is that even possible?  IMO, _we operate with a lax definition of "know"_.  
TS and JS gotchas are real, they make understanding of some of the code hard if you know them and create false sense of understanding if you don't.   

The issue goes far beyond TS and JS.  We intuitively assume that the industrial strength tools we use (programming languages, major libraries) got things figure out.  This, unfortunately, is not exactly true and these tools come with not trivial booby traps. 
A long time ago I wrote a series of posts [I don't like Hibernate/Grails](http://rpeszek.blogspot.com/search/label/GRAILS) showing how a more involved Grails project exposed very surprising, often buggy behavior of these tools. 
Hibernate is a popular tool used in banking software. How do they prevent this [concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html)?  

An example that is guaranteed to confuse everyone is a Java code where `a.equals(b)` gives different result than `b.equals(a)` (`a, b` are not `null`).  Try to think about the direct consequences of such a thing on your code? 
Do I need to think about the order of equality checks?  
What would the the impacts for, say, retrieving data from a hashmap be if equals failes to be symmetric on the keys?   
Now, what if I told you this can happen with classes that come with the Java standard library?  You will get no deprecation warnings either.   
Are such issues well known among Java devs?  I am not including an example, it is an interesting experiment to try to search for more details to figure this out.  This issue is documented and (similarly to other gotchas) is not considered a bug.   

OOP creates a very high cognitive load, to a point that even compiler writers mess it up all the time[^rust]. 
I started my programming career as an OOP enthusiast and evangelist. OO programming has an appeal of simplicity and I was seduced by it for many years.  It took me a long time to realize that OOP is not simple at all. 
Let's talk OOP a little. Pick a random OOP training. You will probably learn that _Cat_ *is a*n _Animal_ and that everything is intuitive.   
You will not learn if any of these less obvious are (or should be) true:  
&emsp; function accepting a _Cat_ *is a* function accepting an _Animal_  
&emsp; array of _Cats_ *is a*n array of _Animals_[^array]    
&emsp; function with no parameters *is a* function with one parameter (does this even sound logical to you?)[^function].  
You will not learn about reduced type safety that comes with widening to a superclass. 
I do not even want to start on subtyping gotchas of variant (union and sum) types. You rarely can opt out of subtyping, this complexities are there whether you want to use OOP features or not[^optout].   
OOP is aproachable to learn only because we hide the complex bits from the students[^ts-variance].
_Do we purposefully avoid teaching gotchas and complexities?_ 

[^array]:  Keeping things easy, arrays are mutable. Sadly, you can explore the answer on your own by asking a mainstream language compiler like Java or TS 
and the answer will, unfortunately be the incorrect _yes_.

[^optout]: It is typically impossible or very hard to prevent type inference from widening to a supertype (e.g. to the top type).  One exception is Flow with _exact_ types. Scalaz users do not need to worry about OOP either. In most cases, however, removing subtyping from an OOP language is a losing battle. E.g. here is how that could be done in [TS](2022-01-09-ts-types-part4.html#safety-preventing-subtyping) and note how cumbersome this is. 

[^function]:  In TS function and JS the answer is yes. In TS this is a subtyping rule.
 
[^ts-variance]:  For example, it is hard to imagine than the incorrect implementation of variance in a language like TS was accidental.  It must have been a decision to keep things easy.

[^rust]: "even compiler writers mess it up all the time" is a quote from ([Rust Subtyping Documentation](https://doc.rust-lang.org/nomicon/subtyping.html)) 


Types add a lot of clarity and can reduce cognitive load of understanding the code.  This is very good, except for situations where  the type checker is circumvented.
I am not just talking about casting or unsafe type coercion. 
It is not that hard to write a code in most mainstream (statically strongly typed) languages that (accidentally) puts a _string_ into a _number_.  To trust the types the type system needs to have some reasonable level of soundness.  
The elephant in the room that impacts even Haskell is non-termination. Very few languages can statically reason about totality, you would need to look at some dependently typed proof assistant (like Idris) to have that[^nonterm].  Everywhere else non-termination bypassed the type checker. 

[^nonterm]:  Proof assistant has to be able to check for totality. You could use a non-terminating program can prove any proposition.  Static reasoning about non-termination is obviously an undecidable problem in a Turing complete language, that does not mean static analysis cannot verify a subset of programs. 

The most common non-termination is caused by raising exceptions and exceptions seem to often get overlooked when reasoning about code.  
My personal experience seems to validate this concern.  After many years of programming in languages where programmers do not really look at the types much (Java, C++, C#,...) I transitioned to Haskell where 
types are used extensively and are trusted.  I see a much higher number of missed `catch`-es in Haskell code (again, this is purely based on my personal experience only, you experience may differ).   
It is interesting to note that Java _checked exceptions_ have been vastly unpopular.  More interestingly, I have noticed that old Java programmers are more likely to think about exceptions than other developers, seemingly Java checked exceptions have aided some germane learning process...   
I really like what Rust has done in this regard, you can _panic_ but it is hard to recover if you do, otherwise errors are handled in an `Either`-like sum type called `Result`. 

What is the hidden complexity of Haskell?   Haskell dedicates a significant effort to soundness. For example, it comes with coherence features that are unique to it (see
this Edward Kmett's presentation [Type Classes vs. the World](https://www.youtube.com/watch?v=hIZxTQP1ifo) or this Edward Yang post 
[Type classes: confluence, coherence and global uniqueness](http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/).  
Not everything is perfect, 
as mentioned above, Haskell allows for easy to abuse error non-termination (`error:: String -> a`), it also comes with a very useful `seq` combinator that has a somewhat questionable semantics[^hask].  But overall Haskell language comes with much fewer surprises if compared to the mainstream.    
Haskell ecosystem (including standard library) are more lax than the language itself.  Michael Snoyman's [Haskell Bad Parts](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/) is a great series on this topic. 
The most recent surprise for me is how _Aeson_ (the most popular Haskell library for dealing with JSON)
[generic instances work](https://github.com/haskell/aeson/issues/961). 

[^hask]: `seq` and `error` combined violate category laws. See [Hask is not a category](http://math.andrej.com/2016/08/06/hask-is-not-a-category/)
  
Is it even possible for a human to design computing environment without logical surprises?  Is the task just too complex for us?  
Let's phrase this question differently: are there any
logically sound programming languages?    
Proof assistants immediately come to mind. The answer appears to be that the only 
way to avoid logical surprises in a programming environment is to start by defining a formal semantics and then build the implementation using it. Formalism is possibly the only way to design complex software without gotchas.

In my experience, developers are not expected to know the gotchas and rarely know them or think about them.   
There are several very good reasons for why this is the case: gotchas are considered unexpected behavior (thus, are not expected to happen), developers may not have much to say about which tools are used (thus, accept the tooling as-is), it is often easier to see issues with the tool if you step outside of it (and this type of learning is considered 
not job related).

There is yet another way to look at why developers do not know the gotchas. You may had _repetitive negative thinking_ on your mind when reading this section, I did when writing it. 
Does an ability to notice issues like these imply a certain mindset?  I think it does. 
Is this it: _a healthy mental defense mechanism makes most of the developers ignore the gotchas?_   
I am not saying this is not an outlandish hogwash but I have witnessed enough to make me wonder...    

This outlandishness has the other side:  for some developers not being able to trust the tools they use must be somewhat stressful.  This situation creates cognitive dissonance where you know the tool is off but you still use it.  
It is something, IMO many developers who learned FP but have to use mainstream language like Java at work experience.  
Some cope well with this, some do not.


## Lots of proprietary stuff

Some programming languages (Haskell is a good example of this) suffer from what some people call the _Lisp curse_.  Instead of using established libraries everyone writes their own one-off tools.  In my current work we maintain our own effect system, our own _vinyl_-like extensible types library, our own logger library and much more.  We have been trying to migrate to the available open source equivalents, 
but there are sometimes benefits to internally grown proprietary approaches and such migration is costly.  This state of affairs is also a testimony to how good these languages are: one-off solutions are easy to develop.  
The other source of this phenomenon is developers who like the germane load. 
It takes time before a robust library supporting a new programming concepts becomes available. 
This leads to internal tools being developed and these tools are hard to replace later. 
Well, _proprietary_ adds to the cognitive (extraneous) debt.    

One can argue to the contrary too, having to know several similar tools and having to maintain one makes for a much
deeper understanding. 

## Cognitive cost of abstraction

FP comes with undeniable love of abstraction and (sometimes) also formalism.  I share these infatuations and always have argued for both.  
But abstraction does have a cognitive cost and tradeoffs.  The idea of lowering the learning curve has been floated around by some Haskell bloggers (e.g. TODO)
and became one of the goals for some Haskell projects (TODO).  The question is how much of abstraction is adequate and how much is too much.  I have seen very abstract code where the abstraction was like trees preventing developer from noticing the forest.  One source of such examples is error handling.  
Mathematics rarely things about error messages.  I have blogged about it in my posts about [Maybe Overuse](2021-01-17-maybe-overuse.html) and [Alternative and errors](2021-02-13-alternative.html).   
Maybe not surprisingly, these were rather negatively received, heavily down-voted posts.  The topic itself is very much a _repetitive negative thinking_.   
One simple to explain and not very abstract example still in this category is the `guard` combinator in Haskell.
I see it used and I also scratch my head when, say, JSON parser error says only `"mempty"`.  
Some of us really dig abstractions and are arguably very good at them.  I consider myself in that group.  
But we are kidding ourselves if we do not acknowledge the cognitive cost that comes with these.  We also kid ourselves if we do not notice that abstractions can blind us. 

Abstractions are essential to enable understanding of code.  To me, abstraction is what allows code to scale up without an
explosion of complexity.  It is not about avoiding abstractions, it is about getting better at understanding our cognitive limitations. 

## Bugs are a cognitive overload

Not all bugs are accidental.  I have seen bugs that can be described by this (C-style) comment:

```Java
// FIX-ME: this is a temporary hack that has potential of going very wrong
```

that found their way to production and stay hibernated for years before exploding.  
In the same vain, some unsound language design choices (e.g. incorrect implementation of variance in a mainstream OO language) were probably intentional to keep things "easy" instead of correct. 

But in all fairness most bugs are not intentional.  
That means that the programmer missed, got confused about, did not understand something. 
But it is out job to figure these things out.  Our inability to do so means that the task at hand it simply 
too much for us.  We are facing a cognitive overload. 

How about the typos, trivial overlooks that are sometimes so hard to spot?  I consider these to be caused by the extraneous load too.  Often it is an extraneous load we have not figured how to remove yet.  
For example, static checks for exhaustive pattern matching (available even in some mainstream languages) can prevent a lot of bugs in this category.  We have not figured out yet how to do the opposite and check output surjectivity in some way.  
Here is an example that keeps popping in my mind when thinking about trivial errors.  I have seen many stack overlflow errors in my life, I have seen only 2 or 3 since I moved to Haskell.  They all were caused by Haskell allowing this lambda expression:

```Haskell
let blah = blah 
in blah
```

This to me is a good example of extraneous complexity allowed by the compiler.  Many languages (e.g. anything in ML groups like OCaml, Reason will not allow such code). 
Here is a relevant discussion on reddit [NoRecursiveLet](https://www.reddit.com/r/haskell/comments/lxnbrl/ghc_proposal_norecursivelet_prevent_accidental/).
Thinking about reducing extraneous load could impact such discussion (in this case, by supporting the proposal). 


Bugs can be caused by to much of intrinsic load, to much of extraneous load, or to much of germane abstraction. 
To avoid bugs we want to manage the overall cognitive load.  We can't do much about intrinsic load.
We can impact extraneous and germane loads only. 
Cognitive psychology advice is to reduce extraneous cognitive load redirecting it with germane load.  
So here we go, if we follow cognitive psychology, we need to replace
_complex_ with _hard_ to reduce bugs. 


## The when of germane load

Let's think about a price tag on the things we discussed:

1. Improve project understanding increases per project cost (e.g. learning how untyped JS code is structured or maybe memorizing how it is not structured)
2. Improve programming skills introduces cost outside of project scope (e.g. learning what are types)

In the industry focused on short term goals 2 will be unpopular even if benefits of 2 are significant.
The ramp up time for the projects needs to be short. This explains why all mainstream languages look and feel alike. Developers need to be able to “hit the ground running” when using a new language.  
There are other angles to look at this issue, I wrote about it in the context of types being unpopular [here](TODO).

Let's dream a bit. In this unicornian reality we have a lot of time to indulge in germane load. 
The goals of this investment are: 

1. Effort outside of the project
2. Reduced cognitive cost per project

Kinda like code reuse, only this reuses understanding.  The discussion so far provided a lot of reasons why 
this could be cost effective and work very well.
I will examine this unicornian reality in the next section talking about FP.

## Cognitive cost of FP

I am not suggesting that FP == Haskell, I use Haskell here because that is what I am familiar with.  

I learned FP while working as a Java / Groovy developer. 
It took me 8 years, I estimated about 7000 hours.  To clarify the scope, this effort included PLT, Category Theory, programming in bunch of languages like Idris, Elm, Purescript, ScalaZ, Coq, not just Haskell.   
Given typical consulting rates for a senior Java dev that is a million dollar investment. And, I still had to learn a lot when I started my actual Haskell job.
Now, I probably have convinced you that either I am slow on the uptake of FP is just too hard.  My point is not that FP cannot be learned and applied incrementally, rather that there is a lot to learn and doing so within project timelines is not going to work well. 

How many programmers or how many CS college graduates, do you think, will understand how the following (mid-school?) formulas apply to programming?:

> $a^{(b + c)} = a ^b * a ^c$    
> $a^{(b * c)} = (a ^ b) ^ c$

Is understanding of of pattern match and currying formulas more or less important than knowing, say, the `kubectr` command?  The answer I recommend is: both are important.    
To finish your assignment you have to know `kubectr`, to finish it well you would benefit from knowing the principles.

Given limited resources "have to" wins over "benefit from" every time. 
Learning, especially learning the principles has to happen outside of the project work. 

There are 2 reasons why FP is hard.  One: it is simply hard (has a decent surface area but is also deep), two: it is different.  
It required shift in how developer thinks.  This shift is especially hard if the developer can only practice imperative 
skills at work. The tools we use impact our cognitive function. 

> &emsp; "It is not only the violin that shapes the violinist, we are all shaped by the tools we train ourselves to use, and in this respect programming languages have a devious influence: they shape our thinking habits."

The quote is from [Dijkstra letter to The University of Texas](https://chrisdone.com/posts/dijkstra-haskell-java/) protesting their Haskell -> Java curriculum change.  If you into technical sports, you may have the term "muscle memory".  
It is often harder to unlearn or adjust a body movement then learn a new one from scratch.  

The required mental shift is the source of kinds of problems associated with FP.  It forms a communication barrier, it divides the community and teams.  

Let's come back to the topic of learning FP so I can dig my hole a little deeper, here is one example. 
There is one line of code that made a huge impact on me (it is called the _Free Monad_ and is in Haskell):

```Haskell
data Free f a = MkFree (f (Free f a)) | Pure a 
```
 
I decided to dedicate a full summer to learning this one and it took me longer than that.  There is actually quite a bit to learn here!  
For example, how does it relate to this line (looks very similar, just replace `Free` with `Fix`):

```Haskell
newtype Fix f a = MkFix (f (Fix f a))
```

Or, what does _free_ mean?  Can other things than monads be _free_ (arrows, comonads, monoids, ...)?  Can `Free`-s with different `f`-s be combined?  If so are there easier and harder ways of combining them.  What is _freer_?  
Also, how do I use it?  What are the available libraries (there were not that many back then)?  How to I use it DIY style?   
Effect systems are a very powerful programming tool, they can add a lot of structure and cognitive simplicity to the code.  
I use 2 of them at work. 
Effect systems allow to organize code into DSLs and interpreters.  This approach creates very high level of code reuse, testability, defines very explicit, self-documenting types.   
But, is it realistic to learn the concepts in a day or two when starting a new project?  Imagine a programmer who uses Java for work exploring this knowledge.   

So what are my points? 
The learning process needs to be gradual and independent of current project work. 
It also needs to come with a promise that the acquired knowledge will be applicable to a future work ideally with some ability to practice early on.
But it also can be incremental, understanding is not all or nothing game.  E.g. in FP setting the team will benefit from understanding what a monad means before learing free monads.  
Learning FP while programming Java is super hard,  however there are some steps one can take to move forward, e.g. introduce 
FP-like library form the Java ecosystem (like vavr).  With that said there is no substitute for the real thing.  
The biggest obstacle could be the team dynamics and the willingness of other members to go on the journey. 


_side_note_start
Effects Side Note:  Since I wrote myself into a corner here, what are some extraneous cognitive loads associated with effects?   
I will mention just one related to consuming of DSLs:  
I sometimes see a single DSL instruction interpreted directly to IO (more Haskell terminology here, IO is what we call a sin-bin) and used in a brute-force IO code.  That is arguably worse than defining a set of reusable IO functions. The whole idea
of effects is to build the app by combining a several DSLs and choosing and composing their interpreters.   
So here we go again, a sophisticated tool designed to add structure to your code used in a way that just adds complexity.  
_side_note_end


## Communication between programmers from different platents

Programmers do not think about programming in the same way. I will focus on one aspect only here, 
one I gave a lot of though to: formalism vs pragmatism.   
This discourse in programming is very old.  It has picked up a recently as functional programmers tend to be more
formalists and the overwhelming majority of programmers are pragmatists. 

Consider this fictional conversation:   
Alice: "Hey, we have a logical flaw in our code, we have to change things" could solicit one of these replies: 
Bob: "are you talking about a production issue, a failing test, or is it purely theoretical?"   
or Carol: "we have done it like this before and everything was fine".   
The conversation now is in an impasse, a little trenches were digged.  
  
Alice is clearly using a fake name, her real name is negative Nancy:).
Bob and Carol responses sound more pragmatic, positive, and common sense than the issue raised by Alice.  
Bob and Carol's approach is the prevalent one and often gets the job done "well enough".   
Well, Alice clearly noticed something that Bob and Carol do not understand but Alice inquiry has effectively been shut down.
Bob and Carol's approach reminds me of Ted Sandyman:

> &emsp;  _"Keep your nose out of trouble and no trouble will come to you."_ — Ted Sandyman a pragmatist programmer in Lord of the Rings.

Also, what Alice wants is better ability to understand the code, to a mind trained in formal thinking any logical flaw 
is a big spiky thorn, it kinda invalidates everything. Cognitive dissonance is often experienced as stress.  
Bob and Carol may be frustrated with Alice approach to work too. 

If only ...

Let's change it a bit:
Alice: "I am concerned that our code is a little too brittle, there is a design flaw that could result in rare intermittent problems" or
Alice: "I learned this new testing framework, it found a very subtle bug that concerns me".


TODO explain how these are better.
It is important that the response does not shut down the requestor: "I trust that you are correct, it would be good to know how prevalent this issue is, maybe is not very critical?". 





## Low Code

This post operated on the assumption that there is no free lunch, the cognitive load needs to be somewhere. 
Is this a valid assumption? 

What is _low code_?  At work I am working on a Haskell infrastructure that supports "low code" Python development. 
I have been around the block for a long time and it is hard not to see _low code_ as part of a reoccurring pattern (RAD e.g. Borland C++ or Powerbuilder, frameworks like Ruby on RAILS or Grails). The industry keeps trying to simplify software development but there must be takeoffs.
_No code_ seems like the lowest possible cognitive debt ever.  It might be!  Unless, of cause, it produces incorrect results or not the outcome you want.  

Low code typically implies a very simplified opinionated development that tries to remove coding out of ...well coding.  
Low-code is more "done for you" rather than "do it yourself".
It can be great if you can live with the opinions.  But what if your needs are even slightly different that what the low-code designers have envisioned?

I look at low-code/no-code as evolution of frameworks like Ruby on Rails or Grails. 
With a framework like Grails you can interactively scaffold a simple CRUD app that is ready to use!  But what happens if your needs grow
and the app has to become more involved.  Hey, you still have access to the generated code and can do whatever you want, you do not even need to scaffold to start.  I worked on 3 Grails projects, 2 were not a cookie cutter. These 2 were very hard to maintain.    
How can one remove coding out of app development, yet provide ability to do arbitrary customizations?
Arbitrary customization benefits from access to the code and from no opinions. 

The other issue is that is is hard to automate a rainy day.  The designer of low code needs to be able to think about the rainy day to start with.
Returning to my Grails experience: using Grails, for means, for example, that you need to accept a serious [concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html) I mentioned above.  

AI solutions look interesting in this space but have the same (and probably amplified) concerns. 

In the context of cognitive load low-code looks like a winner if it does what you want and if it does it well. 
The big question is what do you do if requirements change in a way not foreseen by the low-code design?  

IMO, low-code can be a great choice if you can work closely with the team of devs who implemented and maintain it.  
Low-code could be viewed as an architecture that manages cognitive loads in an interesting new way.



## Coping with cognitive load. Communication.

Extraneous loads can cause stress and frustration. 

Maintaining messy code can be stressful. You may have to work in code that defies understanding. 
Fortunately, projects like these become "infamous" very fast, and you get moral support from other team members.  That really helps. 
My advice is: be a source of such support if your colleagues end up working in messy code.  Few words of encouragement and acknowledgment of that hardship can go a long way.  
Also the information will slowly percolate up and the management may become more receptive to accept the cost of a big refactor or even a complete re-write. 

This post has advocated for code simplicity over ease of development. Knowing how to write simple code and not being allowed to do so can be very frustrating.  Sometimes there is a good reasons why the code is kept in a certain way. 
One common reason is that the code needs to be accessible do developers who contribute to it.
Understanding why things are the way they are is often good enough to create acceptance and alleviate frustration.  However, examples like [How to stop functional programming](https://brianmckenna.org/blog/howtostopfp) come to mind.  I believe that the industry should try to strive a balance between accessibility and simplicity better than this.  
With micro-services being so popular one would expect more opportunities for some divide and concur where at least some of the code strives to be hard and simple.  Whats really hard is finding a places like that.  The job market for functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust top the weekend use stats based on stackoverflow surveys[^weekend].  There must be quite a few frustrated programmers out there.  I have been in that position and I know it is mentally hard. 

[^weekend]: Repeating some of what I wrote [here](2022-03-13-ts-types-part6.html#about-simplicity):  Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. I found this link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  These stats are hard to find but I also found this one: [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/). In 2019 Rust moved ahead of Haskell.  
At the same time, the job ranking (based on the UK's [_add_blank_target IT Jobs Watch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do),  I have
not found a similar ranking for the US.) puts Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017.  
This ranking is possibly exaggerated too, lots of jobs list Haskell and good to have but will have you code in PHP.  This bias exist
in any language but is stronger for something like Haskell than say COBOL. 

  



How do you cope with problems you cannot do anything about?  You have to find some way to stay positive.  
The big helpers are openness, acceptance, and empathy.  

The germane load should be much simpler to deal with. 
We are (well I am) still learning and there should be no shame in not knowing everything.  
It is OK to say "I do not understand this", even more than that, such statement should be encouraged.   
By saying it we do 3 things: accept our own limitations, say that we accept limitations of other team members, and move towards a more open team environment.  We also allow others to help,  helping others is a very positive and meaningful experience that, 
among other things,
can reduce stress. 

Creating an environment where it is OK to not know everything could lead to changes in how the group works.  
Maybe the team will assess more time for PR reviews, pair program more, help each other out more.




## There is much more to it

This post was not intended to be an exhaustive theses on cognitive load in programming. 

Cognitive psychology big focus is education.  This post considers cognitive load as part of software development process in the
industrial setting.  I have not discussed the learning process itself. 

Performance cost is a hidden complexity. 
It is not obvious how performant the program will be from just looking at it[^complexitytypes]. 


[^complexitytypes]: To contradict myself here is a [presentation](https://www.youtube.com/watch?v=BW3ZDtjD_Yw) and a 
[paper](https://dl.acm.org/doi/abs/10.1145/3498670) showing computational cost at type level, slow programs will not compile!

Cognitive load should be viewed as a resource problem, one that does not scale very well, and one that is not well understood. 
Cognitive load is greatly impacted by turn over rates, switching of code ownership, or installed processes. 
Context switching is very expensive, the programmer inability to find contiguous blocks of time to focus could be viewed as an indication of an under-resourced project.  

Documentation: does documentation reduce or increase the cognitive loads?


This post did not run out of topics, rather I have run out of steam.  I hope you have found it interesting and 
thought provoking.  Thank you for reading. 



-- 
Developing calluses.
