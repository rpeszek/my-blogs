---
title:  Cognitive Loads in Programming 
lastmodified: Aug 30, 2022
featured: true
summary: About cognitive aspects of bugs, abstractions, types, PL ecosystems.   
toc: true
tags: patterns-of-erroneous-code
---

> > &emsp; _"My brain hurts", a quote from a code review_ 

**DRAFT version:** _This post is a work-in-progress. I am publishing it to solicit early feedback._ 

This long post presents programming in a different light than what is commonly considered.  We will look at cognitive aspects of interacting with code.  

We will examine cognitive effort that goes into the implementation and cognitive loads on these poor souls who need to work on that code later.  We will consider the programming language, its libraries, and implemented programs as 
_instructional materials_.  We will view the developer as both an _instructional designer_ and a _learner_. 
We will think about bugs as cognitive overload and a missed learning opportunity.  We will discuss the cognitive impact of abstractions, types, and programming principles.  
  
Cognitive load of working with code is rarely considered in actual project work. We ask "How long will it take?" (in fibonacci numbers, of course), we do not ask "How will it impact the overall complexity?".  
I had quite a few eye opening moments when thinking about these topics. 
This is the main reason I decided to write and share my thoughts.
This post will be a high level rant discussing programming across the industry spectrum from JavaScript to Haskell. 
It is written as a set of loose notes about various cognitive aspects related to working with code. The main goals are 
to: 

* show how considering _cognitive loads_ in context of programming projects provides valuable insights 
* present some useful terminology for reasoning about code complexity.


I will try to explain psychological terminology but this post assumes readers' (high level) familiarity with concepts of FP and OOP.

My pet peeve is identifying specific [_add_blank_target patterns of erroneous code](/tags/patterns-of-erroneous-code.html) and what could be causing them, there is a human factor and a technical part to these patterns.   
Mental processes involved in writing code are such a fascinating and broad subject. 
I am taking only a very narrow path through it.   
I am planning another high level post to discuss programming from a different but relevant angle,
it will be about empirical and deductive aspects of working with code. 
I believe these 2 aspects impact our cognitive loads in interesting ways. 
So, this post will focus on cognitive challenges caused by code.  The next post will focus more on the human aspect. 

This post reflects on my personal observations accumulated over 27 years of professional programming work, augmented by a few years of academic teaching.   
I am not a psychologist, these are observations of a coder.

## That dreaded YAML

I am perusing thousands of lines in Infrastructure as Code (IAC) yaml files. I am looking at an already refactored and improved version. It is a lot of templated _YAML_ of k8s configuration at my work.  The underlying reason for the complexity is the PL itself[^yaml].  Did the refactor break things?  Of course it did.  Complexity has consequences.  With some effort, the issues were fixed. This is how things are, there is nothing we can do about it.  There isn't?

I want to contrast _YAML_ with a configuration language called [_add_blank_target Dhall](https://dhall-lang.org/) (one of my favorites). 
To use _Dhall_ you may need to adjust to a Haskell-like syntax, maybe learn a few new concepts (like ADTs), think about configuration that uses lambda expressions.  The return on the investment are Dhall safety features. 
Dhall even makes the process of refactoring safe, 
you can compare the previous configuration against the new and Dhall will tell you if both are equivalent or why not. 

[^yaml]: E.g. see [_add_blank_target Every Simple Language Will Eventually End Up Turing Complete](https://solutionspace.blog/2021/12/04/every-simple-language-will-eventually-end-up-turing-complete/)

_Dhall_ and _YAML_ come with very different cognitive challenges. 

## Cognitive psychology

Cognitive load theory defines cognitive load as the amount of information that working memory holds at one time. 
The idea is that the human brain is limited in that capacity.  Psychologists have identified the load to be about 3-5[^correct1] "units of information" (also called "chunks"). This space appears to be quite limited.   
I imagine the magic number is small in programming. However, I expect it to vary between 
individuals.  
If we can load only a limited number of "chunks" into working memory, how big can these chunks be? 
The answer is interesting: it seems that it does not matter![^chunking]  
In some situations, the magic number appears to be 3 (the concept + 2 constituent chunks)[^magic3].   
Notice, it would be hard to enumerate chunks involved in a classic imperative program, but that number will be >> 5.  

[^correct1]: Corrected from 7 to 3-5 based on feedback from [_add_blank_target u/Fereydoon37](https://www.reddit.com/r/haskell/comments/x2du9d/comment/imjc87f/?utm_source=share&utm_medium=web2x&context=3). The number 7 came from earlier studies which have been observing higher levels of "chunkability", 3-5 seems more relevant.  I have also removed my use of technical sports as an analogy (motor skills seem not very relevant to our discussion). 

[^chunking]: See this wiki page: [_add_blank_target Chunking](https://psychology.fandom.com/wiki/Chunking).  For me, thinking about a big chunk without a context, e.g. _OOP_ or _Geometry_ triggers some high level information plus a seemingly random example, thinking more causes my brain to wander down some path. "Tell me everything you know about ..." is not something I am capable of. 
So I do not think we load a whole huge chunk into working memory, but we can operate using chunks of seemingly unlimited size. 


[^magic3]: Examples of concepts in programming that come with 2 chunks: assignment statement, function (input and output), function application (function and input), function composition. 
This pattern allows for very big chunks, here are examples with 2 big chunks I have worked a lot with in the past:
algebraic topology, compensated compactness (using measure theory to study PDEs).  Moving closer to programming, good big chunk examples are: computation laws, computation properties, operational semantics rules, Curry-Howard correspondence.  Mathematical theorems mostly follow this pattern (e.g. in the context of programming "Simply typed lambda calculus is strongly normalizing").  A candy: "Dhall is strongly normalizing".   
It appears that we can, indeed,  do great things focusing on 2 chunks, However, I have not found much psychological research on 3 being the magic number.   

The idea of decomposing the program into fewer (but bigger) "chunks" that interact in a clear way has been around for as long as I can remember.  We will examine this idea in terms of _Cognitive Load Theory_.

Cognitive Load Theory is concerned with _instructional design_ and improving how _information is presented_ to a _learner_. 
Controlling the learner's cognitive loads is an essential part of this work. 

Continuous learning is a part of what programmers do, but implementing and modifying project code is by far the biggest cognitive effort that programmers face.   
I look at this as: the code itself is a very important _instructional material_, programmers are _learners_ and _instructional designers_ at the same time.    
Programs are where the _presentation of information_ happens. 
The concepts and findings of cognitive load theory seem still relevant after this adjustment.  
  
Cognitive psychology considers 3 types of cognitive load: _Intrinsic, Extraneous, Germane_. 
All are related to _information presentation_ and we will think about them in the context of code.  

* _Intrinsic cognitive load_ is the inherent level of difficulty associated with a specific (instructional) topic. 
Thinking about code, requirements are a good choice for a topic. 
A rough moral equivalent known to software developers is _essential complexity_ (things are complex because they are, to reduce this load requirements would need to change).  

* _Extraneous cognitive load_ is generated by the manner in which information is presented to learners and is under the control of instructional designers.  This term is often used to describe unnecessary (artificially induced) cognitive load.  Thinking about code, a rough moral equivalent of high extraneous load is 
_accidental complexity_[^accidental] (things are complex because the program made it so). 

* Germane cognitive load refers to the work that is put into constructing a long-lasting store of knowledge or schema.  Schema is a pattern of thought or behavior that organizes categories of information and the relationships among them.  Psychologists also use the term 
_"chunk"_ and schema construction is the process of creating these chunks in memory.  
Thinking about code, this roughly translates to using _abstractions_, higher level concepts, types, programming principles.  An _OO_ programmer may try to define intuitive object hierarchies, employ design patterns to model the business domain. 
An FP-er may use denotational[^denotational] approach, look at how things compose (think about categories), design DSLs, blue-print the design using types... 

> &emsp; _Cognitive load theory thesis is about reducing extraneous cognitive load redirecting it towards germane load._

Cognitive theory considers intrinsic load to be not movable, obviously requirements can be changed. 

I need to emphasize that the _information presentation_ under consideration facilitates understanding of the code itself and not so much the concepts (e.g. abstractions) used to create it. 
Knowledge of these concepts is a prerequisite.  Prerequisites are an important caveat and one that ends up being contentious.  

_side_note_start
**Prerequisites:** Working on a project code will reinforce knowledge of programming concepts (psychologists call something similar a [_add_blank_target worked-example effect](https://en.wikipedia.org/wiki/Worked-example_effect)) but, for a working programmer, learning new concepts ideally needs to happen outside of project work. In reality, there is no time for this. 
Also, available programming concepts are limited not only by what the developer and the team know, but also by what is supported by the PL (programming language).  Developer backgrounds and 
what is supported in a PL vary a great deal.  Thus, the list of prerequisites that can go into a programming project is limited. 
_side_note_end


[^denotational]: Denotational approach means mapping requirements to mathematical concepts such as monoids, identifying categorical structures (things that compose), etc.  

[^accidental]:  Terms _accidental_ and _essential complexity_ come from [_add_blank_target No Silver Bullet](https://en.wikipedia.org/wiki/No_Silver_Bullet) paper.


This sets the stage for what I want to discuss, but before continuing let me briefly review a ***few more relevant concepts.***  

_Cognitive overload_ happens when working memory is overwhelmed by the 3 cognitive loads we have described, IMO, bugs are evidence of a cognitive overload.   
However, psychology is not a simple arithmetic, some programmers learn how to process large cognitive loads
sequentially, a few chunks of information at the time and get good at it.  However, high cognitive loads will overwhelm even the most diligent among us, There is even a trivial combinatorial complexity to this (does working memory go through a binomial number reloads?). 

I wanted to use _cognitive debt_ in the title, intending it as a pun on "technical debt" because I am interested in discussing
negative impacts on the team's ability to understand and reason about the code. 
However, this term turns out to have a clinical meaning and I decided against using it.   
_Cognitive debt_ is a psychological term associated with _repetitive negative thinking (RNT)_.
_Cognitive debt_ and RNT are hypothesized to have some very negative health consequences that can lead to depression or even dementia. 
RNT is described as 

> &emsp;  _“excessive and repetitive thinking about current concerns, problems, past experiences or worries about the future”_

I do not claim to know a lot about clinical psychology but the definition clearly is very relevant to programmers and could partially explain why programmers are often unhappy[^unhappy], or why programming discussion groups are often very negative.   
Sadly, RNT seems to be a condition that really good programmers are likely to experience.  Good programmers think about rainy day scenarios, notice design flaws, can anticipate program issues...  My pet peeve, [_add_blank_target patterns of erroneous code](/tags/patterns-of-erroneous-code.html), is an RNT.
It seems important that we talk about RNT.   

[^unhappy]: Interesting youtube: [_add_blank_target Why Do So Many Programmers Lose Hope?](youtube.com/watch?v=NdA6aQR-s4U)

You may want to think about _working memory_ and _cognitive loads_ as something akin to _RAM_ in a computer.  
What is the equivalent of a CPU cost?   In this post I use _cognitive effort_, the usage of this term is not very consistent[^effort] in the literature.  

[^effort]: See the discussion in [_add_blank_target A Computational Analysis of Cognitive Eﬀort](https://www.researchgate.net/publication/220963754_A_Computational_Analysis_of_Cognitive_Effort). The term _cognitive cost_ is typically used to mean a negative impact on a cognitive function induced by stress and the usage is also not very consistent, I am avoiding its use.

These were cliff notes written by a non expert.  There are many tricky and relevant bits like _information retrieval from long term memory_.  Things I am progressively less and less component to describe in psychological context.  



## Easy vs Simple 

If you have looked at my [_add_blank_target TypeScript types](/tags/TypeScript-Notes.html) series, you have seen me [_add_blank_target write about it already](2022-03-13-ts-types-part6.html#about-simplicity). 
I do not claim that my definitions are the only correct way that these terms should be interpreted.  However, I have seen other programmers use a similar disambiguation so I am including it here.  

I consider the terms simple and easy to have a different meaning.   
Easy: A low barrier to entry (e.g. easy programming concepts). Hard is the opposite of easy and implies a high learning curve.   
Simple: Low effort to *correctly* reason about (e.g. code written using learned concepts). Complex is the opposite of simple (e.g. code that is hard to understand).  The term "arbitrary complexity" fits this definition very well. 

Easy means fewer prerequisites and implies low germane load, hard means many prerequisites.   
Simple means low extraneous load, complex means high extraneous load. 

This differentiation could also be expressed as: 

> &emsp;  _Easy means low cost of creation, simple means low cost of consumption_

except, in this post my interest is the cognitive effort only not the total cost[^disambiguation].

[^disambiguation]: Note that these terms imply some context.  E.g. _simple to reason about correctness_ could be very different from _simple to reason about performance_.  The most popular context is "this code needs to work" with a somewhat relaxed definition of what "works" means, typically implying a reasonable level of correctness.  This was pointed out to me
in the conversation with [_add_blank_target hasufell in Haskell discourse](https://discourse.haskell.org/t/cognitive-loads-in-programming/4994/12?u=rpeszek)

Achieving _simplicity_ on a larger project is not _easy_. Easy does not scale well. 
There appears to be no free lunch, cognitive load needs to be somewhere.
My big preference is trying to achieve _hard and simple_ rather than _easy and complex_. 
In other words, I prefer to spend my cognitive bandwidth on germane load over extraneous load. 
This, I am pleased to note, is aligned with cognitive psychology.   

Recall the advice from cognitive psychologists is to reduce extraneous load redirecting it towards germane load.
This translates to:

> &emsp; _Move from complex to hard_

An interesting way to look at easy vs simple is to think about a creative process like writing a book. 
Easy to write is clearly very different from simple to read. 
In programming these two are bundled together, a program created by one developer needs to be consumed by another dev
who needs to modify it or interact with it. 
The term "readable code" comes to mind. I consider it different from simple.  E.g. readable code does not mean no subtle bugs. Message is readable if you know what it conveys, but what it conveys could be complex or even misleading. 

IMO, the popularity of easy and the unpopularity of simple are a systemic problem in today’s programming and elsewhere.

Next section discusses examples of code which was intended to be easy and ended up complex.


## Extraneous loads that grow 

What was the most complex code you've worked on?  
I can think about a number of contenders, but my answer will be very unimpressive: I had to maintain a web page (just one page), it was implemented using Java Struts 1.  This code used no advanced concepts, all ingredients were easy: control statements, instance variables, lots of protected methods with typically no arguments and void returns that read and set the instance variables.   
The Java class behind it had about 200 mutating instance variables.  Changing the order of 2 lines in this code was almost guaranteed to create an (often intermittent) bug, it was hard to even make sure that variables were set before they were read.  
This code became infamous for its complexity very fast.  Interestingly, Struts were blamed, not the needless overuse of mutable state.  
_I want you to channel your inner psychologist and answer this question: what is going to happen when a new functionality is added to a Java class with 200 instance variables?  Right, I agree,  we will have 201 instance variables._   
This piece of code was eventually refactored. If I remember correctly, about 12 instance variables were kept, they were actually needed by Struts.   

This experience seems to me a good example of a big extraneous load, I had to deal with a load of 200 coupled "chunks".    
Let's think about such code as an instructional material.  I can attest, it was virtually impossible to even know what this code is supposed to do from looking at it.  
Ability to program using clear inputs and outputs (rather than void methods with no input parameters) requires a learning effort, I submit to you that this prerequisite is easier than the cognitive effort of maintaining such code.  
Thinking about this as instructional material, clear inputs and outputs are great learning objectives.  You know the app if you understand its inputs and outputs. 

_side_note_start
Maintaining messy code can be stressful. Fortunately, projects like these become "infamous" very fast and you get moral support from other team members.  That really helps. 
Be a source of such support for your teammates as well.  Few words of encouragement and acknowledgment of the hardship go a long way. 
Also, the information will slowly percolate up and the management may become more receptive to accept the cost of a big refactor or even a complete rewrite.  This is what happened in my Java Struts example.
_side_note_end


My second example is something that happened more recently. I worked on reimplementing a JS application. 
It was one of these apps that can be described as: _was easy to write, is hard to maintain or understand_. 
I am sure very few readers will be surprised by the existence of a hard to maintain JS application, but let's put talking about this aspect aside. 
Is writing "easy" code the same as generating excessive cognitive load for the maintainers?
I think it typically is, it is not that hard to incrementally develop a non penetrable maze. 
Maintaining some code structure to manage the cognitive load is not "easy".   
The new version is still close to JS (it uses TypeScript and vanilla React) but tries to enforce these 3 principles: [_add_blank_target referential transparency](2022-03-13-ts-types-part6.html#referential-transparency-purity-and-explicit-types), [_add_blank_target clear, explicit](2022-03-13-ts-types-part6.html#about-clarity) types that also [_add_blank_target work as documentation](2021-12-24-ts-types-part2.html#types-as-documentation), and _async/await_ abstraction to avoid callback hell.   
Referential transparency is an interesting dichotomy.  Experiencing different results every time the code is executed typically causes surprise, in my experience developers rarely think about this during implementation. 
Thus, the code may feel weird and opinionated (e.g. React components avoid using hooks) but it remains accessible.  

> &emsp; *IMO, high quality code shifts cognitive load from maintainer to implementer*   

This works great even if both are the same person.    
Let's consider the new JS app as an instructional material.  Referential transparency creates learning objectives (inputs and outputs can be learned if outputs are predictable) while explicit types are an instructional material in itself (a blueprint). 
The biggest prerequisite for the implementers was knowledge about what to avoid. 

Besides some common sense principles (feel free to add more), what else can we do to control extraneous load? 
Things are about to get more tricky.  
Human cognitive load is limited but we can do abstract reasoning.  It is simpler for us to deal with a few generalized abstractions than with a multiplicity of concretes[^concrete].  And, as we know, abstractions are a better use of our working memory _chunk_ space.  
This suggests exploring the space of programming abstractions.
Unfortunately, programming abstractions are nontrivial.  That makes them hard to learn, but what is worse is that developers and language designers sometimes (if not often) mess them up.  Instead of decreasing, this increases (or even explodes) the cognitive load.  We will explore this topic in [Extraneous nature of abstraction](#extraneous-nature-of-abstraction).   


[^concrete]: As a side note, concrete thinking is not always bad.  An interesting article on this in a broader context: [_add_blank_target Concrete Thinking: Building Block, Stumbling Block, or Both?](https://www.healthline.com/health/concrete-thinking).    


Types are, obviously, an important tool in controlling cognitive load.  Types _offload_ many code verification tasks from the developer. This is significant, developers can ignore a potentially high extraneous load of a program by trusting its type. 
As I already mentioned, types can be an _instructional material_, a blueprint. 
Using a type checker can, in itself, be an interactive learning process (e.g. using REPL to ask type questions about the code).  
However, types are subject to similar limitations as abstractions: a learning curve, PL limitations, correctness issues (only, we call it _soundness_ if types are involved).  

Reducing cognitive load using abstractions and types is doable but requires navigating some tricky waters. 

_side_note_start
**In a unicorn universe,** projects are not allowed to exceed certain thresholds of cognitive load. 
When the threshold is reached abstractions that lower the load are identified, learned, and respectfully applied. 
If that is not possible, requirements are reexamined.  Unicorn managers are automatically beatified. 
&#129412; 
_side_note_end


## Bugs and metacognition

Let's define a bug as an unintended program defect. That removes all the temporary hacks from consideration.  But it is the programmer's job to figure these things out. A bug implies some issue in the mental process. 

I consider cognitive overload to be the main cause of bugs. 
_Metacognition_ is an important concept in cognitive psychology. It is about knowing strengths and weaknesses in our own cognitive process.   
I started analyzing and recording all defects I encounter at my work.  My goal is to understand better what has caused and what could have prevented each issue.  My records suggest that bugs _uncover_ extraneous complexity.  In other words, it is a good idea to ask this question: What is the underlying complexity that caused the developer to create this bug? 
The idea is to learn from bugs.       
Types, obviously, can be very helpful in bug prevention.  Programmers who start using a PL with powerful types (e.g. Idris, Haskell) experience this first hand: a lot of compilation errors, many uncovering an issue in the program.  Notice, this is a very interactive process and an interactive learning experience in which developers can observe how and why they failed. Developers also observe what PL features prevent the bug from escaping. 

> &emsp; _Programming is an interactive process of finding and fixing bugs.    
  &emsp; IMO, programming should be an interactive process of identifying and resolving the underlying causes of bugs._

“Insanity is doing the same thing over and over and expecting different results". I promise you 2 things:
When you start analyzing bugs, you will start seeing patterns (similar to [_add_blank_target patterns of erroneous code](/tags/patterns-of-erroneous-code.html)).  Unfortunately, you will likely have problems in communicating these patterns to developers who do not go through a similar process.  I found that a code review session showing the same issue in a few places works better than trying to explain this without a concrete context (oops). 

How about typos, trivial overlooks that are sometimes so hard to spot?  That mysterious brain of ours is good at creating these. 
A great reading on this, in the context of (non-programming) typos, is 
[_add_blank_target WUWT, Why It's So Hard to Catch Your Own Typos](https://www.wired.com/2014/08/wuwt-typos/).  
Human brain has an ability to fill in gaps, self-correct things.  Human brain is better at focusing on 
high level ideas and is perfectly happy skipping over minute details.  This characteristic seems even stronger if we are on board with the big idea, and it seems fair to assume that programmers are on board with the features they are implementing. 
The main point is that our brain is not well designed to work at the level of statements and lexical tokens, it wants to work on big picture items. 

_side_note_start
Side note:  This line of thought could also partially explain why programmers seem to be at home in the code they wrote even if other programmers consider it a complete mess. Sometimes just changing font or background color allows us to spot issues we have overlooked before. Our perception changes if what we interact with feels foreign (interestingly this should increase the cognitive load). 
It appears that some mental reset is sometimes needed. 
_side_note_end

Error proneness of programming at the level of PL statements is also consistent with the cognitive load theory.  At this level a programmer needs to consider a multitude of details, most likely overwhelming the working memory limits.   
An interesting piece of trivia is that Turing's original paper (the one about universal machines and halting problem) had several bugs in it.
_If Turing could not get it right, what chance do we have?_[^scott]  

[^scott]:  “If Turing could not get it right, what chance do we have?”- is a phrase I remember from a Dana Scott lecture.  
The bugs were only discovered during the actual implementation work (see [_add_blank_target Alan Turing](https://blog.wolframalpha.com/2010/06/23/happy-birthday-alan-turing/)).  Church's [_add_blank_target take on undecidability](https://plato.stanford.edu/entries/church/supplementA.html) followed shortly after Turing's. As far as I know nobody found any issues with it (even if untyped lambda calculus is rather unruly and the topic itself was hotly debated).  

Static compilation can prevent a lot of trivial errors and hopefully the prevented list will grow, but that list is not exhaustive.  

**Section Summary**  

My first point is that programmers should start considering cognitive aspects when thinking about bugs. 

What is that we do when we discover a bug?  We write a test, right?  Does this reduce the cognitive load?  Of course it does not.  IMO, it is more important to spend time on some intro- and retrospection and look for ways to lower the extraneous load or build some type safety.  If that is not possible, improving test coverage becomes important.  I want to learn from bugs.  _Fixing bugs is the least important part of the process._  I should also mention that this is, unfortunately, a _repetitive negative thinking_ territory. 

Here is an example that keeps popping into my mind when thinking about trivial errors.  I have seen many stack overflow errors in my life, I have seen only 2 or 3 since I moved to Haskell but they were not easy to find. 
They all were caused by Haskell allowing this lambda expression:

```Haskell
let blah = blah 
in blah
```

This to me is a good example of extraneous complexity that could be prevented by the compiler.  Many PLs (e.g. anything in ML groups like OCaml, Reason will not allow such code). 
Here is a relevant discussion on reddit: [_add_blank_target NoRecursiveLet](https://www.reddit.com/r/haskell/comments/lxnbrl/ghc_proposal_norecursivelet_prevent_accidental/).
Thinking about reducing extraneous load could impact such discussion (in this case, by supporting the proposal). 

My second point is the recurring one, types and abstractions can play a big role in reducing bugs.
Hopefully types and abstractions themselves are bug free!  

_side_note_start
**There is an alien civilization** of programmers who analyze their bugs, Alien PL designers consider these analyses to decide which features are in and which are out. Their PLs do not even have strings. You can do a lot
of harm with just strings. 
_side_note_end

## Extraneous nature of abstraction

Summary of previous sections:  Our cognitive load is limited but we are capable of abstract reasoning and can work with big _chunks_ of knowledge. Abstractions seem like our best hope in reducing the overall code complexity. But ...there are a few caveats.   
Programming abstractions are known for their germane load (for being _hard_ to learn and for being less straightforward than imperative) but not so much for their extraneous nature (for being needlessly _complex_), the second aspect is much more interesting so let's discuss it.  

**Poorly implemented abstractions**

You spotted an intermittent malfunction in a code you maintain. Luckily, you see only one commit in recent history and you have a strong hunch something is wrong with that commit. Only some 50 code changes. 
The one that caused the issue is: `var1 == var2` changed to `var2 == var1`.  Would you be able to spot it? 
I call this type of issue a "gotcha".   
How about: your _finder_ function seems to be not finding stuff, only that sounds too far fetched, the function looks correct, so you just ignore this as a possible explanation.  The underlying issue is
that sometimes `x =! x` and you have used an equality check to find things. 

I like to think about this paraphrasing Gimli: 

> &emsp;  _"Computation Laws are upon you, whether you would risk them or not."_ 

Equality is an example of an abstraction developers implement and use, but not think much about.
However, the list of surprising behaviors like these is quite long affecting all kinds of abstractions. 
Gochas create chaos in the cognitive process. Gotchas often become mystery bugs and are resolved using workarounds.  
For abstractions to work as a cognitive load reducer, they need to be treated seriously by the implementer.   

Developers I talked to often responded to such examples by saying something like: "This is just bad code, whoever implemented it should have been more careful". 
Except, I can point to examples in standard libraries of popular mainstream PLs or popular frameworks[^gotchas1].
The issues come with no deprecation warning and, if documented, are considered a 'feature'.   
Are questions like "does a developer have a fighting chance of troubleshooting this feature?" even being asked? 

[^gotchas1]: Example of non-symmetric equals is `java.sql.Timestamp` used with `java.sql.Date` or `java.util.Date`, these remain used as standard JDBC mappings for DB columns, the usage will show no deprecation warning.  `[] !== []` and
`[] != []` in JS (incidentally `[] == ""`), working in JS often feels like explosives engineering.   
I wrote a blog series about [_add_blank_target TypeScript Types](/tags/TypeScript-Notes.html) and ended up presenting a lot of complexities and gotchas that probably surprise any TS developer.  
How do Hibernate users prevent this [_add_blank_target concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html)?  
I remember Grails as a land mine of issues, I wrote an 11 part [_add_blank_target blog series](http://rpeszek.blogspot.com/2014/10/i-dont-like-hibernategrails-part-11_31.html) about it back in 2014.  
Java Streams have a very interesting take on referential transparency: 
if you execute a stream twice the second attempt will fail. This is probably the first and only attempt at dynamically typed linear types &#128578; and is bound to create interesting intermittent issues.


**Abstractions themselves causing cognitive issues**
 
OOP creates a very high cognitive load, to a point that even compiler writers mess it up all the time[^rust]. 
I started my programming career as an OOP enthusiast and evangelist. OO programming has an appeal of simplicity and I was seduced by it for many years.  It took me a long time to realize that OOP is not simple at all. 
Let's talk OOP a little. Pick random training. You will probably learn that _Cat_ *is a*n _Animal_ and that everything is intuitive.   
You will not learn if any of these less obvious are (or should be) true:  
&emsp; function accepting a _Cat_ *is a* function accepting an _Animal_  
&emsp; array of _Cats_ *is a*n array of _Animals_[^array]    
&emsp; function with no parameters *is a* function with one parameter[^function].  
You will not learn about reduced type safety that comes with widening to a superclass[^widening]. 
I don't even want to start on subtyping gotchas of variant (union and sum) types. 
OOP is approachable only because we hide the complex bits from the learners[^ts-variance]. 
A relevant psychological concept is a cognitive bias called _framing effect_. 

[^array]:  Keeping things easy, arrays are mutable. Sadly, you can explore the answer on your own by asking a mainstream compiler like Java or TS 
and the answer will, unfortunately, be the incorrect _yes_.

[^function]:  In TS and JS the answer is yes. In TS this is a subtyping rule.

[^widening]:  This is a gotcha generator especially in OOP languages that have some level of type inference. 
E.g. here are some gotchas in [_add_blank_target TS](2021-12-12-ts-types-part1.html#compilation-bloopers) that involve widening to  `unknown` which is a top type in TS, here is a discussion about these in [_add_blank_target Scala](http://rpeszek.blogspot.com/2017/07/scala-whats-wrong-with-you_29.html).

[^ts-variance]:  For example, it is hard to imagine that the unsound implementation of variance in a PL like TS was accidental.  It must have been a decision to keep things easy.

[^rust]: "even compiler writers mess it up all the time" is a quote from ([_add_blank_target Rust Subtyping Documentation](https://doc.rust-lang.org/nomicon/subtyping.html)) 

The concept of exception (i.e. `throw` and `catch` game) is another example of a risky complexity that impacts even Haskell[^non-termination].    
Types can reduce cognitive load of understanding the code, except exceptions provide a very accessible and virally used way to bypass the types. 
Other "bottom" types like `null` are in the same boat.   In my experience, many developers turn a blind eye on error handling in general. This seems akin to _omission neglect_ (psychological concept loosely described by this popular phrase: out of sight out of mind) and some _optimism bias_ (focus on sunny day scenarios only).   
I really like what Rust has done in this regard, you can _panic_ but it is hard to recover if you do, otherwise errors are handled in an `Either`-like sum type called `Result`.   
Hopefully we will see more of this pragmatic approach in future PLs. 

You may notice that the examples of _gotchas_ I am coming up with have something in common. These issues can be classified under: _not trustworthy types_.  Misleading types will confuse any developer, that includes developers who work in dynamically typed languages and may not think about types explicitly.   
_We think in types more than we realize._ 

Are there any "gotcha" free environments?  Haskell comes close but is not perfect[^haskell]. 
Proof assistants like Idris come to mind, you get very sound abstractions, and these can even verify totality. 
That is kinda interesting, let's pause for a bit here...  Consider the levels of abstraction used in proof assistants. It appears that our brain needs something at the level of a dependently typed lambda calculus to work correctly[^ml]. 
That could make sense, for things to be logical you need, well you need the logic itself. 
Proof assistants are not "gotcha" free though, they have different types of gotchas[^proofassist].   

[^non-termination]: I sometimes see this argument "It is impossible to statically reason about termination in a Turing complete PL, thus, all hope is lost".  Firstly, this is inaccurate: it is possible to statically verify totality on a subset of programs. Secondly: if non-termination is like accidentally hurting your foot, then exception is like shooting yourself in the foot.  A missing DB record should, IMO, rarely be treated as non-termination. (I use the terms _total_, _terminating_ and _partial_, *non_terminating* interchangeably.)

[^haskell]: Haskell dedicates a significant effort to soundness. E.g. see [Type Classes vs. the World](https://www.youtube.com/watch?v=hIZxTQP1ifo). 
Not everything is perfect however. 
Haskell allows for easy to abuse error non-termination (e.g. `error`, `undefined` functions), however ability to `catch` is more limited than in most PLs. Non-termination in itself throws a wrench, one Haskell should not be blamed for, see [Hask is not a category](http://math.andrej.com/2016/08/06/hask-is-not-a-category/) and 
[What Category do Haskell Types and Functions Live In](http://blog.sigfpe.com/2009/10/what-category-do-haskell-types-and.html).
Overall Haskell language comes with much fewer surprises if compared to the mainstream.    
The Haskell ecosystem (including its standard library) are more lax than the language itself.  Michael Snoyman's [_add_blank_target Haskell Bad Parts](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/) is a great series on this topic. 
The most recent surprise for me is how _Aeson_ (the most popular Haskell library for dealing with JSON)
[_add_blank_target generic instances work](https://github.com/haskell/aeson/issues/961). 

[^ml]: _Standard ML_ is known for its soundness, I do not know _ML_ family that well, but I do know it has exceptions and `throw/catch` (in this case `raise/handle`) games. Possibly a more accurate point here is that we need strict formal semantics, it does not need to be dependently typed. 

[^proofassist]: Languages like Idris push the limits of what a compiler can do. I have experienced compiler hanging, compilation error messages overflowing my terminal buffer... (the second happened to me in Haskell too but the scenario warranted it more). These issues happen if you start doing certain type level things (I was just trying to implement a block chain on the type level &#128578;).

**Wrong abstraction for the job**

Let's talk about data structures a bit. The choice you make can impact extraneous complexity a great deal. An example which emphasizes this is [_add_blank_target CRDT](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type).  
Imagine that you are working on an app where 2 or more agents (human or not) can concurrently work on some list  and your program needs to merge their work.  Using a standard _list type_ will be a cognitive nightmare, right?
Think about one agent (R) removing items, another agent (A) adding items. How do you know if an item was removed by (R) or (A) just added it?  So what do you do? You introduce some distributed locking mechanism?  ...Things are becoming complex very fast.   
The choice of which data structure is used can have a big impact on extraneous complexity.  This extends to other abstractions as well.

**High levels of abstraction, an extraneous aspect**

I have seen very abstract code where the abstraction was like trees preventing developers from noticing a forest. 
One source of such examples is error handling. 
Mathematics rarely concerns itself with error messages, falsehood is falsehood.  I have blogged about it in my posts about [_add_blank_target Maybe Overuse](2021-01-17-maybe-overuse.html) and [_add_blank_target Alternative and errors](2021-02-13-alternative.html).    

_side_note_start
Side note:  Probably not surprisingly, these were rather negatively received, heavily downvoted posts. 
The topic itself is very much a _repetitive negative thinking_. 
Incidentally, the negative comments mostly belonged in the general “what you are describing is just bad code, whoever wrote it should have been more careful" category.  I want to understand how code abstractions could promote erroneous code, my interest is in what makes people not careful.  
_side_note_end

Let's focus on Haskell. One simple to explain and not very abstract example that still fits into this section is the `guard`[^guard] combinator. 
I see it used and I also scratch my head when, say, a JSON parser error says only `"mempty"`. 
Possibly, some programmers think about the abstraction called `Alternative` when they should be thinking
about something like `MonadFail`, an abstraction that allows to specify error messages.   
Abstractions seem to come with what psychologists call a _commitment bias_ (hey, I am doing `MonadPlus` damn it!).   
It is us, not the tooling, Haskell ecosystem offers a very expressive variety of abstractions. 
E.g. consider the error handling blind spot we talked about earlier.
You can think about `Either` as a `Bifunctor` or an `ArrowChoice` argument, what typically gets our attention is its throw and forget `Monad` semantics.    
Some of us really dig abstractions and are arguably very good at them. 
But we are kidding ourselves if we do not acknowledge that abstractions can also blind us.   
IMO the one thing we can do about it is to be aware.  More diligence + awareness is typically all it takes.

[^guard]: For readers not familiar with Haskell, `guard` allows to reject a computation 
based on a boolean expression. It is defined using a very general concept of `Alternative` and at this level of generality specifying an error message is not possible. In real life I see it used with parsers and other computations that could really use an error message.  

**Section Summary**   
Some developers react to gotchas with something akin to _omission neglect_, while other developers appear to create a mental store of gotchas and their potential impacts.  I am in the second group.   Maintaining this store is not necessarily easy.
I will also note a possible relationship to _repetitive negative thinking_.  
    

Gotchas presented to us (thank you very much) by language designers or library implementers should technically be classified as _intrinsic_ since a common bloke like me can't do much about them other than look for a job that has a better tooling. 
If you look at programming as a whole, these are extraneous loads. 

I have left the subject of abstraction vs imperative (abstractions being less straightforward and harder to map to actual execution) untouched. I plan to return to this and to the topic of gotchas in my next post.

_side_note_start
**There is a planet** where PL designers treat all programming abstractions and types with respect.
Only sound, correctly implemented abstractions are used.  
As a result, this planet has only unpopular languages, their TIOBE index starts at number 100. &#127776;
_side_note_end

## Germane and intrinsic load of FP

I expect that nothing in this section will be surprising to a functional programmer, but FP has such a unique cognitive impact 
that it is hard for me to not talk about.    

Functional Programming allows us to understand computations in ways that are not possible without FP. 
Understanding is a big cognitive simplifier[^understanding]. We are more at home with things we understand than with things
we just know.  Realizing that computations is something I can actually study to understand has been a game changer for me as a programmer. 

[^understanding]: The terms understanding and 
knowledge are often conflated.  The difference, however, is significant, here it is described in the context of learning math: [_add_blank_target “Understanding” Versus “Knowledge” – What’s The Difference?](https://japan-math.com/blogs/news/understanding-versus-knowledge). This has been very much my experience as a mathematics learner and educator. 
Things become both easy and simple once you understand them. 

Consider the following (middle-school?) formulas and how they relate to programming:

> $a^{(b + c)} = a ^b * a ^c$    
> $a^{(b * c)} = (a ^ b) ^ c$

These, pattern match and currying formulas, suggest that computations relate to other things we already know in ways that are almost surprising[^ct].   
From the cognitive load theory point of view, an ability to map to existing knowledge needs to be viewed as a big plus (and a missed opportunity in how we learn programming). 

[^ct]: Category Theory will never cease to surprise

FP is hard and there are 2 reasons why.  One: it is simply hard (has a decent surface area but is also deep), two: it is different.  

I was learning FP while working as a Java / Groovy developer. 
It took me 8 years, I estimated about 7000 hours.  This effort included Category Theory, Types (my main interest), PLT, and programming in a bunch of FP languages. 
This has been, obviously, a big personal investment. And, I still had to internalize a lot of this when I started my actual Haskell job.  Please do not interpret these stats as an argument that FP cannot be learned incrementally, or that learning FP does not provide immediate benefits. I am including these personal stats as evidence of an overall effort but also as evidence of the multitude of learning opportunities. We should resist thinking about knowledge as a binary checkbox. 

[^fpeffort]: 3 learning experiences stood out:
[_add_blank_target TAPL](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages) (~7 months), [_add_blank_target CTFP](https://www.goodreads.com/book/show/33618151-category-theory-for-programmers) (~6 months), and [_add_blank_target T(ype)DD in Idris](https://www.manning.com/books/type-driven-development-with-idris) (~4 months).

FP requires a shift in how developers think.  This shift is especially hard if the developer can only practice imperative 
skills at work. The tools we use impact our cognitive function. 

> &emsp; "It is not only the violin that shapes the violinist, we are all shaped by the tools we train ourselves to use, and in this respect programming languages have a devious influence: they shape our thinking habits."

The quote is from [_add_blank_target Dijkstra letter to The University of Texas](https://chrisdone.com/posts/dijkstra-haskell-java/) protesting their Haskell -> Java curriculum change.  If you are into technical sports, you may have heard the term "muscle memory".  It is often harder to unlearn or adjust a body movement than learn a new one from scratch.  It is even harder to "own" the old movement and the new movement at the same time.  Psychologists also believe that unlearning is hard[^unlearning].   
The required mental shift for FP is the source of all kinds of additional problems.  It can form a communication barrier, it can divide the community and teams.   
At the same time, this cognitive shift is an opportunity to understand programs in a different way.

[^unlearning]: see 2.1 section in [_add_blank_target Unlearning before creating new knowledge: A cognitive process.](https://core.ac.uk/download/pdf/77240027.pdf)

I will dig my hole a little deeper. 
This one line of code made a huge impact on me (it is called the _Free Monad_ and is in Haskell[^free]):

```Haskell
data Free f a = MkFree (f (Free f a)) | Pure a 
```
 
I decided to dedicate a full summer to learning this line and it ended up taking longer than that.  There is actually quite a bit to learn and _understand_ here!  
For example, how does it relate to this line (looks very similar, just replace `Free` with `Fix` and drop one constructor):

```Haskell
newtype Fix f a = MkFix (f (Fix f a))
```

Or, how is this a monad?  Does it satisfy monad laws?  What does _free_ mean? Can other things than monads be _free_?  Can `Free`-s with different `f`-s be combined?  If so, are there easier and harder ways of combining them? What is _freer_?  How do I program with it? 
How does it (and should it) relate to `try-catch` games? And finally, what libraries implement and use `Free`?
The point I am trying to make is that FP computations are a different breed. 
They actually have properties and the learner can build an understanding of these properties.    
Effect systems (the main application of `Free`) are a very powerful programming tool, they can add a lot of structure and cognitive simplicity[^effect].  I use 2 of them at work, one of them we maintain. 
Effect systems allow us to organize code into DSLs and interpreters.  This approach creates a very high level of code reuse, testability, and defines very explicit, self-documenting types.   
Now, is it realistic to learn these concepts in a day or a week when starting a new project?  Imagine a programmer who uses Java at work exploring this knowledge.   

[^free]: For readers not familiar with this concept and curious about what this does, `Free f a` allows to construct monadic (whatever that means) syntax trees with instructions provided by `f`.  In the context of this article, the only important point is that this one line of code has a lot of properties that can be learned (and that many computations come with a similar learning potential).

[^effect]: Any extraneous cognitive loads associated with effects?  Yes, there are a few, especially on the implementation side. 
Also like most other tools, effects can be abused. I sometimes see a single DSL instruction interpreted directly to IO (more Haskell terminology here, IO is what we call a sin-bin) and used in a brute-force IO code.  This just adds cognitive overhead of effects without taking advantage of what they have to offer. 


There has been some discussion about making Haskell itself more accessible (e.g. [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html))
and some library effort in this direction as well (e.g. [_add_blank_target IHP](https://github.com/digitallyinduced/ihp)).  
Some teams separate _hard_ micro services with high levels of abstraction from the rest.  
Some places separate a possibly very advanced implementation from a simple to use API (I believe Facebook's Haxl does it). 
Creating a progression from easy to hard is less trivial.   

FP is a niche,  I think FP has a stable niche in programming. 
Correctness and understanding of computations are problems almost nobody in the industry cares about but they are sometimes needed.
This reminds me of a Terry Pratchett Diskworld character: Esmerelda (Granny) Weatherwax 

>  &emsp; _"Esme Weatherwax hadn't done nice. She'd done what was needed."_

Wanted means popular, needed means stable.  However, basic principles of FP will probably find a wider use (as discussed in 
[Extraneous loads that grow](#extraneous-loads-that-grow)).   
I plan to return to discussion of cognitive aspects of FP in my next post. 

_side_note_start
**In a parallel dimension** Alonso Church did not take a temporary break from lambda calculus and showed it to his student, Alan Turning.  The first computer hardware was based on SKI calculus. In that dimension kids learn mathematics with proofs, imperative programming is considered a great addition after programmers learn the principles.  In that dimension 
software has very few bugs, however, this universe has fewer programs, even fewer programmers, and the error messages suck. &#127756;
_side_note_end


## Post Summary

My readers may get the impression that this post is a criticism of imperative programming. 
Applying cognitive load theory to programming does not translate to "imperative is complex", rather it translates to "too much of imperative in one place (logically coupled) is complex".  IMO, some amount of imperative is often helpful.   
I plan to return to this topic in my next blog.

I am sure you have noticed that I think a lot about code complexity.
And, yes, I do not feel comfortable working in messy code. Assessing and controlling the level of code complexity is crucial to me. 

It has dawned on me that my dislike of code complexity may not be shared by others. _False consensus effect_  is assuming that everyone else thinks like me.   
I remain convinced that some programmers react negatively to code complexity, but now I think that many programmers feel at home in code with a high cognitive load.  This motivated me to work on this and the next post.  IMO it is important that we try to understand each other a little better. 
 
Are we doing a good job in managing code complexity?  I think this a fair question to ask even if you think
that simplicity is not crucially important.  This post has argued that we are mostly failing on that front. 
In this post, we looked at how project complexity grows unnoticed, how bugs are a missed opportunity to learn about how we fail, and how FP changes the cognitive process but can be hard to learn. As a whole this post has been a bit of _repetitive negative thinking_, but I hope you found some positives and useful ideas in it as well.  The main point of this post was to advocate for including cognitive aspects of programming projects into consideration and to present some useful terminology for doing it. 


## There is much more to it

This post took a very narrow path through the very broad subject of cognitive aspects of programming.  

These were observations of a programmer, this post does not try to cover research on this topic or provide a good list of reading materials. I do not feel qualified to provide either. This topic is also related to code quality and this has been vastly discussed.

My focus was coding rather than process. I did not discuss things like cognitive loads in pool requests, cognitive considerations during sprint planning, git hygiene, etc.
  
Size of program files is an obvious, related topic I did not discuss.

Monorepo vs single projects has interesting relevance.  Dependency graphs of or sorts (version, library deps) are a similar interesting topic. 

Coding efficiency and the 10X programmer in the context of cognitive loads is an interesting (but contentious) topic.

Low Code: The idea of distributing cognitive load across different components is not new.  The terms "decoupling" or
"isolation of concerns" are in this space.  Low code is an idea of a very lopsided distribution in which most of the complexity falls onto the infrastructure.  I started writing about it but decided to remove my notes as this post feels already too long.  
 
Some PLs (Haskell is a good example of this) suffer from what some people call the _Lisp curse_.  Instead of using established libraries, proprietary or one-off tools are often created.  It is interesting why this happens and what to do about it.  Could love of abstractions be causing it (reuse abstractions, not a code)? 
Is writing it from scratch a lower cognitive effort than learning and applying an existing solution? 
The end result, obviously, increases the cognitive load. 

Cognitive load should be viewed as a resource problem, one that does not scale very well, and one that is not well understood. 
Cognitive load is greatly impacted by turn over rates, switching of code ownership, and by installed processes. 
Context switching is very expensive, the programmer's inability to find contiguous blocks of time to focus could be viewed as an indication of an under-resourced project.  

Linting, formatting, aesthetics are all very interesting cognitive load topics.  Most programmers seem to be very sensitive to how the code is presented, (e.g. would you ever use a light background in your code editor?). Similarly, syntax vs semantics, it seems syntax has a huge cognitive role even if we think about it as bikeshed. 

Habit formation and unlearning are a big and very interesting topic. 

Cognitive biases in the context of coding seem like very interesting topics too. In particular _bandwagon effect_ (TypeScript is popular and hence must be very good), _framing effect_ (new cool technology), _commitment bias_ (we done it like this before, it has been tried and tested), _functional fixedness_ (we do not need another PL), _omission neglect_ (things we do not know are not important), _groupthink_ (we want to work with people who think like us), _bikeshedding_ (possibly most of this post &#128578;).

Point-free code, I stayed away from discussing it.

Cognitive aspects of troubleshooting are something I only touched on.  

Imperative vs denotative is something I only touched on.

One topic I do plan to discuss (in the next post) is a distinction between empirical and formal processes in programming and how it impacts cognitive loads and acts as a divider.  

Cognitive loads are related to stress, I intend to return to this topic in the future as well.

This post did not run out of topics, rather I have run out of steam.  I hope I gave you things to think about.  Thank you for reading! 

