---
title:  Let's agree to be different. On empirical and deductive nature of coding. 
lastmodified: Oct 20, 2022
featured: true
summary:  On Empirical Nature of Coding. Let's agree to disagree
toc: true
tags: Haskell
---

> *"When the going gets tough, the tough get empirical." Jon Carroll* 

I will discuss (on a very high level) empirical, experimental, and deductive aspects of programming. 
This distinction is fun to analyze (and a somewhat unusual way to look at programming) but it seems mostly a useless curiosity in itself.  I have convinced myself that programmers tend to favor empirical or favor deductive. IMO, we are placing ourselves into 2 camps.  I call these camps _pragmatists_ and _theorists_. 
This division impacts how we program and communicate. Discussion of both mindsets is the main goal of this post. 

The topic for this post came from a realization I had when thinking about cognitive loads ([_add_blank_target Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html)). 
I may be obvious to some of you, but it was not obvious to me: programming is largely an empirical process. 
I will argue that the pragmatic empirical mindset is also dominant. 

What the others mean, how they reason, or what is important to them are the contexts in human communication. Communication without the basis of common interest is hard. Good communication requires an effort of understanding these contexts (we call it finding a common language).  IMO, _empirical, deductive, pragmatist,_ and _theorist_ are a good terminology to analyze some of the current discourse (especially about FP). 
In this post I will try to present my honest opinions about the empirical and deductive mindsets. You may disagree with my opinions, please let me know if you do. The point is to get these contexts right, or at least to have all of us think about them a little.  

Fairness and lack of bias are rare but beautiful if encountered in human interactions.  At my current work I mostly use Haskell and I am interested in functional programming, thus, this post is likely to have some unintended bias. Also, these are my opinions, not an attempt at a scientifically sound reasoning.  

I have not found much discussion about this, I am not following academic research in any related area. 
The topic of [_add_blank_target empirical software engineering](https://en.wikipedia.org/wiki/Empirical_software_engineering) is relevant to programming and the empirical method, but is not really what I will talk about. 
Retrospecting on my software programmer career,  I recall good and bad things. The good had a good communication, the bad had a bad communication of some sort.  Pragmatists vs theorist is just a part of a bigger puzzle, I am going to explore that part here. 

## Empirical vs Deductive 

I my experience, developers do not use this terminology. Hey, in my experience, I do not use this terminology so a recap is in order. 

Scientists use these 6 (or more) terms: inductive, empirical, a posteriori, deductive, rational, and a priori. 
This sounds like a beginning of a catchy song.  There is also informal and formal but it breaks the rhythm, so I skip these for now. _Practical_ and _theoretical_ are great terms to describe a mindset, while _empirical_ and _deductive_ describe the thought process. 
In this post I will mostly try to use _empirical_ and _deductive_ when referring to the thought process, I will use _pragmatist_ and _theorist_ when referring to people. 

Empirical (to avoid using inductive[^inductive]) reasoning draws general conclusions from observations.  E.g. certain software functionality will work because we tested it (notice how well software testing fits in to this definition).
Thinking about software testing as an empirical process allows us to consider things like bias[^biasdef] (e.g. did we miss a range of scenarios?), observation sample size (did we test enough?), correlation vs causality (e2e tests are unstable, is there something wrong with e2e testing itself?), if observations are balanced (adequate coverage of test scenarios across functional areas)... 
Here is an exaggerated example of a correlation vs causation problem: "Each time I test the app it works just fine, yet users keep reporting issues. Something's wrong with these people!". 
But is it even more fun to think about both experiments and observations. 

[^biasdef]: I use the term bias loosely as well.  Scientists are concerned about bias in design of empirical studies (which observations go in, how things are observed, etc), bias in how results are analyzed (e.g. proper stratification).  Bias is a [_add_blank_target statistics](https://en.wikipedia.org/wiki/Bias_(statistics)) term, [_add_blank_target psychology](https://en.wikipedia.org/wiki/List_of_cognitive_biases) term,  all of this seems very relevant to programming. 

Deductive reasoning goes from general knowledge to specific conclusions. E.g. certain software functionality will work because of type safety or because it is a straightforward application of something else we believe works. In this post, deductive represents a wide range of thought processes: from tedious observationless mental verification 
of values and types (oh, this value was supposed to be a positive number why is it negative?, is the new refactored code equivalent to the previous?...) all the way to formal reasoning.  

Empirical and deductive work in tandem and both are essential.  IMO, we (as individuals) prefer to use one more than the other. 
Understanding more about these preferences will be the main topic of this post.


[^inductive]: This inductive reasoning should be confused with mathematical (or structural in PLT) induction. Believe it or not it sometimes is. 


_side_note_start
Formal reasoning[^formalprog] is the only approach humans have figured out to solve complex problems correctly[^formalprog2] on the first go (without trial and error associated with empirical reasoning). 
Being a programmer, I like to think about formal reasoning as immutable and empirical reasoning as an in-place mutation (e.g. mathematics keeps adding to itself while empirical sciences like medicine keep changing).  
In this post I am not separating the formal out, it is bundled into deductive. 
I have struggled with this decision, at the end I decided to simplify things and keep formal reasoning bundled into the more broad deductive process.  
_side_note_end


[^formalprog]: The term _formalism_ has special meaning in mathematics, I use it colloquially (i.e. all mathematics is formal).
Examples of formal approaches popular among FP-ers could be equational reasoning, use of logical implication (e.g. with Haskell type class constraints), use of mathematical or structural induction (e.g. with inductive types like a functional list prove properties of, say, the implementation of `map`). Readers familiar with equational reasoning may agree with me about its similarity to a refactoring process where developer mentally verifies that the new code is equivalent to the old. The line between formal and informal is sometimes thin. 

[^formalprog2]: This is kinda fun: we have only empirical evidence of mathematics itself being _correct_, we know we will never prove if formally.  However, we have a lot of empirical evidence of (past) _incorrectness_ in empirical sciences. 
I am a pragmatist enough to say that mathematics is correct, the rest looks good only until we find something wrong with it (just like bugs in software). 

You may think that I am spending too much time explaining the obvious:  any engineering will have a strong empirical side, engineers like to tinker with tings.  I assume that some readers are like me and have not thought about it before.  The next section goes deeper into the empirical nature of coding.   

## Coding by experimenting and observing

Consider these tools and processes:  debuggers (_observe_ execution of statements), loggers (record _observed_ behavior), testing (_observe_ app behavior and _draw general conclusions_ about app correctness), TDD (pre-define _observation_ outcomes for the code), design patterns[^patterns] (generalize code _observed_ to work well, create coding protocols). 
I encourage you to think about tools and processes that are targeting the deductive, there are some!

[^patterns]: Design patterns are an interesting bunch because they include some deductive work.  E.g. factory decouples idiosyncratic aspects of object construction and decoupling is known to be beneficial. 
However, this is not that much different from, say, a wildlife biologists generalizing observed behavior of individuals to a whole species using a known symbiotic relationship in their argument.  
Hard to resist: They say symbiotic, we say decoupling... ;)   

Trial and error is how a lot of programming is done.  We write some code (_experiment_) and then _observe_ the result using tests, a debugger, looking at the logs, or simply observe how the app behaves ignoring other diligence. 
Personally, I am a little afraid of experimenting with certain types of code and I try to think through all scenarios, I could miss something but my mental process includes a lot of deductive effort. I have worked with many programmers who  operate in a similar way.  However, I still need to test the code to _observe_ it working, make changes if needed, and rinse and repeat. Despite some deductive elements, this approach is still very experimental in its nature.  

How about working in code that is a complete mess? 
We make experimental changes and then test / debug / trace the heck out of it[^heck], right? 
We may try to reason about what solution is likely to work, but that is not much different from experimental alchemists trying not to blow themselves up. I can recall several projects where all my understanding was derived from debugging or tracing. 

[^heck]: Notice, I did not say "debug the crap out of it" because that could imply that we are making code improvements. 

Let's talk process bureaucracy.  Working in empirical world means procedural protocols.  Scrum is a procedural protocol: 
consider continuous improvement process with retrospectives or team velocity calculation,  these are all very empirical. 
Test plans, test cases, coding and formatting standards, git hygiene, even design patterns are also procedural protocols.
In contrast, the deductive needs a cushy couch. Deductive and bureaucracy, IMO, do not mix well. Waterfall was a failed idea of applying a deductive approach to project management.  There is a lot of real word complexity in project management and empirical is needed (see also [_add_blank_target Defined Process Vs Empirical Process](https://premieragile.com/defined-vs-empirical-process/)). 


Let's talk bias in empirical reasoning. Figuring out contributing factors and causality is often the hardest and the most important part in empirical reasoning.  If you wrist hurts when you type, is this a pinched nerve in the wrist? or, are you looking down on your laptop and the nerve is pinched around your neck? or there is a pressure point somewhere on your arm? or maybe it is not a nerve issue at all?  Practitioners of empirical have been known to assume wrong cause[^bias]. Empirical is tricky. 

[^bias]: Rate of misdiagnosis for carpal tunnel is over 80% [_add_blank_target carpal tunnel misdiagnosis](https://www.carpalrx.com/post/carpal-tunnel-misdiagnosis). A very sad example for the past: it is now believed that a big cause of death during Spanish Flu was too much aspirin prescribed to treat the symptoms. 

Programmers are exposed to bias too. Performance issue or a bug could be caused by many factors, e.g. an application could misbehave only in certain scenarios or only in certain environment configurations, the underlying issue could be in the application code, library code, configuration issue, environment problem... 
It is not unlikely for a developer to go down a wrong path during troubleshooting process. I view programming as empirical process with accelerated feedback. You can go down a wrong path but you typically learn that fast.

I had a fun discussion with my wife, she is a statistician and data scientist working on pharmacological studies.  We were discussing if a design of a clinical trial could be adjusted to software benchmarks. 
There are some intriguing similarities. Obviously, humans are much more complex than programs and targeted approaches will work better in either domain.  But it was a fun discussion and one that convinced me even more about the empirical nature of programming. 

So, if programming is mostly experimental and empirical, let's do something unusual and discuss it as such:
 
## Programming as empirical process

This section can be treated as a side note.  Considering programming as empirical process provides an opportunity to think a little more about the programming process itself. An opportunity I simply can's miss. 
Let's put our empirical hats on and let's discuss what code is bad and what code is good "empirically". 

### Experimental process and high extraneous loads

This frames the discussion of empirical in the context of my previous ([_add_blank_target cognitive loads in programming](2022-08-30-code-cognitiveload.html)) post. 
If you have not looked at that post, simply substitute _high extraneous load_ with messy code. 
I think we are in a position to put a 1 and 1 together:  

> &emsp; _The experimental nature of programming is a consequence of its high extraneous load and also one of its main causes_ 

The bigger extraneous load the more we experiment.  The more we experiment the bigger the extraneous load gets.  
To work on a complex code we are effectively forced to experiment.  Adding more experimental code only increases the complexity.  

This is a feedback loop.  To break this loop (and control the complexity) we need to rewrite or refactor parts of the code.  These involve some deductive process.  Some amount of deductive is essential, deductive process is what can break the feedback loop.  

There is one notable exception where piling up experiments does not result in extraneous complexity.
It is called type safety and I will briefly discuss it:


### Type safe experiments

This is my favorite approach to programming. 
In the presence of a nice type system, coding can become solving jig-saw puzzles. 
A similar process can work even in TypeScript[^jigsaw]. 
This approach could include an interactive, type checker assisted deductive process (asking compiler a series of type questions, something akin to type hole driven development[^idris]) or just making a guess and trying to see if the pieces fit. 
The second approach is a form of experimentation. 
Working with _hlint_ (Haskell linter, other static tools that do similar things exist) is lots of experimental fun too. 
I often need to get _a solution_ in and the linter will replace it with a much nicer fused code.  

[^jigsaw]: See [_add_blank_target A walk in the park](2022-03-13-ts-types-part6.html#a-walk-in-the-park), [_add_blank_target phantom types](2022-01-09-ts-types-part4.html#phantom-types), [_add_blank_target existential types](2022-01-09-ts-types-part4.html#preventing-information-escape) in my TS series.

Combinator term is 100 years old and I could be wrong about its intended intuitive meaning[^combinator]. It feels like it was all about jigsaw puzzles.  What I do now is that functional programming is about designing well fitting combinators. 

This jig-saw puzzle approach results in very clean programs, is addictive, and a lot of fun, but (sadly) is not commonly used at large. 
It is an approach FP-ers often use. Is FP empirical?  


[^combinator]: Schönfinkel (credited for the concept of combinatory logic) was Russian and worked with Hilbert in Germany.  His original work was in German. Curry work with Hilbert as well.  The term has to do with building blocks (primitive "functions"), aka S K and I in SKI calculus.  In LC combinator is a lambda expression without free variables. Looks like a piece of jigsaw puzzle to me. 

[^idris]: A nice presentation that illustrates interacting with the type checker to write code is: [Idris programming Vect youtube](https://youtu.be/DRq2NgeFcO0?t=356).  Adding type level information about the size of list constricts the solution space quite a bit creating a jigsaw. 


### Empirical FP
 
FP is considered theoretical for a least 2 reasons: _(1)_ FP considers formalism to be important, _(2)_ how it is being presented to the public[^practical]. But, is FP an empirical process as well?  

[^practical]: There are efforts to change that, e.g. see this reddit: [_add_blank_target Practical Haskell Bits](https://www.reddit.com/r/haskell/comments/xao5fy/announcing_practical_haskell_bits_initiative/).

Consider 2 cornerstones of scientific experiments: reproducibility and properly randomized sufficiently large observation samples, and restrict our attention to automated testing. 
FP-ers will immediately notice two related concepts: _referential transparency_ and _property testing_ (e.g. QuickCheck).  Clearly FP has a lot going on for it if you look at it from the empirical angle. 

Why did we settle on using empirical method in science?  We do empirical not because we want to but because we have to. 
Empirical is the only way to study the unruly real world. Why did we settle on using empirical method in programming?  IMO, there are 2 reasons: nondeterminism and code complexity. Whatever your opinion about FP vs code complexity is, you have to agree that FP is about controlling nondeterminism. Functional programming tries to separate out the predicable deterministic part (pure functions) and limit (even stratify) the unruly effectful parts. Functional programming tries to reduce the need for empirical. I think that is why it seems so empirically-friendly.    

Haskell is sometimes called _the best imperative PL_[^best_imperative].  I propose that _FP could become the best empirical programming method_ (yeah, could I be a little biased). However, it is "could become" rather than "is".  Empirical process benefits from good observability, e.g. decent debuggers, stack traces, rich amount of error information...  This is an area where FP could improve in general (and Haskell could improve in particular)[^logger]. 

[^best_imperative]: "Haskell is the world’s finest imperative programming language" famous quote, probably originated in this paper [_add_blank_target Simon Peyton Jones on Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)

[^logger]: I implemented a proprietary Haskell logger library at my work. It is interesting to think about what we want to observe when we FP. Standard logger libraries for, say, Java are "object-centric" and will allow configuration options based on which class spilled out the log. The library I implemented is "data-centric" and allows you to configure what data you want to see based on what data it is. 
FP is about clear inputs and outputs after all. 

Programming is often associated with mathematics and logic (mostly by non-programmers[^reynolds]). FP is a very much an outlier and an interesting one, FP is both quite mathematical and quite empirical. 
However, it's the theoretical part that many FP-ers love.  

[^reynolds]: I just love the beginning story in [_add_blank_target Types, Abstraction and Parametric Polymorphism (John C. Reynolds)](https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf) paper. 

## Pragmatists and theorists

Let's start thinking about the human aspect. 
I am using terms _theorist_ and _pragmatist_ somewhat colloquially, but the meaning is close to how the terms are used elsewhere.  

If you listen to a functional programmer talk, you are likely to hear these terms: "reasoning about code", "principled computation", "computation laws", "correctness by design", "type safety".  These people are likely to study things like lambda calculi, operational semantics, category theory, type theory...  All these things come with formal proofs and could result in a very specific mental training.  _To this group programming is more of a deductive process._  

These things do not resonate with vast majority of programmers who have a more pragmatic mindset.  You are more likely to hear these terms:  "testing", "TDD", "hacking" (meant as a compliment), even "design patterns" (though rarely these days).  _To this group programming is more an empirical process._[^pragmatists]   

[^pragmatists]: The association of pragmatism and empirical process is not unique to programmers, e.g. if you google "pragmatists vs formalists" today you will probably get a link to this quote "Formalism follows deductive approach whereas pragmatism applies empirical approach" from this legal philosophy paper: [_add_blank_target Formalism vs. Pragmatism in Legal Philosophy](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3423036)

IMO, mindset and interest are very related terms. In this post I could almost use them interchangeably. 

A theorist primary interest is in the deductive reasoning. This implies an interest in the deductive itself. Deductive reasoning is often called top-down reasoning (general knowledge comes first). Formal reasoning (the pinnacle of deductive) has a very strong attraction for some individuals while being very much disliked by others. 
Some programmers dive deep into FP and learn formal reasoning.  Sometimes a mathematician (this was the case with me) makes a career conversion and becomes a developer. Interest in the deductive and formal is rather rare, to many programmers formal methods are a foreign concept. 

In contrast, pragmatist primary interest is in accomplishing the tasks at hand. Pragmatists will experiment and go empirical to get there. “When the going gets tough, the tough get empirical.”  Real world complexity is rarely fully amendable to formal reasoning. Unfortunately pragmatists take a stronger stance on this, 
a pragmatist may think that formal reasoning is never useful. 

Pragmatist will say "it works because we tested it".  Theorists will say "it is correct by design" (or closer to Dijkstra: "we tested it and we know it does not work"[^dijkstra]). Pragmatist will say "this abstraction is too complicated to use because is too theoretical".  Theorists will say "this abstraction is too complicated because it lacks theoretical backing".  The 2 groups may find it difficult to communicate "this is nice" and "this is terrible" have a very different context when spoken by pragmatist and theorist.  


[^dijkstra]: IMO, some experience with formal is needed to better understand limitations of empirical. "Program testing can be used to show the presence of bugs, but never to show their absence!" is a projection of that understanding onto programming.


Pragmatist want the tools to be mainstream and popular to pass their test of practical usefulness.
Pragmatist will select tools that provide good ability to observe how the code runs.  To a pragmatist reasoning about code often means studying its execution flow. 
Some pragmatists go a step further and will only select a PL that allows them the best control over execution (e.g. 
_JS_ is used by browsers so this is what they will want to write). This could be caused by a (somewhat justified[^abstractions]) distrust for abstractions. 

[^abstractions]: See [_add_blank_target Extraneous nature of abstraction](2022-08-30-code-cognitiveload.html#extraneous-nature-of-abstraction).

In contrast, theorists want tools that support, rather than inhibit, deductive reasoning.  They typically want to reason on a higher level than the execution flow.  Many will view programs and execution as decoupled concerns[^fussion]. 
This is where the _imperative vs denotative_ discussion comes into play as well. 

[^fussion]: E.g. rewrite rules in Haskell. 

I consider programming to be a combination of both engineering procedural process and a math-like science. 
Pragmatists are interested in the process, they want code standards, formatting standards, clean git history...  Theorists will want functors, monads, higher rank types, higher kinded types, dependent types...  Programming, obviously, benefits from both engineering and mathematics.  

One bizarre difference I have noticed between heavy deductive thinkers and empirical mindsets is their favorite approach 
to learning.  When learning, some (not all!) theorists may want to finish a whole book before writing a single line of code. The top-down thinking sometimes extends to top-down learning.  Me and my wife are in the first group. 
I had to force myself to write some code when going through [_add_blank_target TAPL](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages), I decided to create a public github repo to give myself incentive to "code as I learn" with [_add_blank_target CTFP](https://www.goodreads.com/book/show/33618151-category-theory-for-programmers) and [_add_blank_target T(ype)DD in Idris](https://www.manning.com/books/type-driven-development-with-idris).  Obviously, this is not ideal and such theorists should learn to like bottom-up learning.  Some theorists treat learning as a murder mystery and want to learn ASAP who done it, pragmatists know it will be the butler.  

On the flipped side, pragmatists prefer a hands on learning from code examples (ideally, associated with a project work) and typically expect immediate return on their learning investments. E.g. a pragmatist is unlikely to spend several months studying, say, a one line of code[^one-line].  Pragmatists are less likely to search for deep understanding (this is kinda definitional, having theoretical interests makes you a theorist).  
Also, it seems logical to assume that learning from experience is more habituating. Pragmatists may have harder time making a mental shifts to how they work with code.  I have linked this Dijkstra quote ([_add_blank_target Dijkstra letter to The University of Texas](https://chrisdone.com/posts/dijkstra-haskell-java/)) in my previous post, but is seems so relevant that I will repeat it:  

> &emsp; _"It is not only the violin that shapes the violinist, we are all shaped by the tools we train ourselves to use, and in this respect programming languages have a devious influence: they shape our thinking habits."_

Unlearning is hard on all of us, I believe it is harder on pragmatists. 

What makes us effective and confident when working with code? 
For a pragmatist the source of confidence is likely to be test coverage, 
for theorist it will be type safety, abstractions, lawfulness.  For both confidence implies some ability to understand the code (absorb the cognitive load). 
To put some of these thoughts in the context of the previous post:

>  &emsp;  _Theorist typically prefer germane, pragmatists are more at home with extraneous cognitive load_   
>  &emsp;  _Theorist typically prefer simple and hard, pragmatists prefer easy and will accept complex_ 


[^one-line]:  I dig a deep hole by using [_add_blank_target Free Monad](2022-08-30-code-cognitiveload.html#germane-and-intrinsic-load-of-fp) as example of such a line in my previous post. 

The part of FP that has been the most disappointing for me is the typically low quality of error outputs.  This may have to do with all falsehoods being equivalent in mathematics[^falsehood].  However, we should not criticize the theory, rather the theorists for selecting abstractions that suppress or confuse error information.  I wrote about it in the previous post and also before[^before].

[^before]: See my posts about maybe alternative overuse [_add_blank_target patterns-of-erroneous-code](/tags/patterns-of-erroneous-code.html)

In contrast, to a pragmatist error output is an (important) observation.  It is a pragmatic thing to do to know _what_ went wrong.  

[^falsehood]: I am not a logician, I vaguely remember some versions of logic that allow multiple moralities, in particular one where there was _lax_ in addition to _false_.  Few (even) mathematicians will know these. 

Some theorists may not see a big difference between between a prototype and a product that is fully implemented and maintained.  This could be caused by academic background where _published_ often equates to _done_ and can be quite annoying. 

I have noticed some pragmatists (not all!) arrive at conclusions too soon.  E.g. “ABC PL is no good, each time I try it I get (this) negative experience”, "Our issues are caused by the technology we use", etc.  This is often confounding correlation with causation and is bad empirical reasoning. These statements are better justified if there is more than the observations to support them.  

_side_note_start
Theoretical and practical mindsets are antipodes of programming.  Even for people who "own" both, being a pragmatist or being a theorist is like wearing a hat.  You can't wear both at the same time, you would look ridiculous. 
I started making a conscious effort to understand which hat I have on. 

Many things about programming seems to be on their head (making hats a somewhat tricky accessory).  Some ideas typically associated with FP are very pragmatic.  E.g. descriptive types, clear inputs and outputs, getting the same result on each try... Lack of these is IMO unpragmatic. At the same time OOP is quite theoretical and very complex (e.g. variance).  

Another complex aspect is how _theoretical_ or _pragmatic_ are you.  If we classify a typical _Rust_ programmer as a _theorist_, where do we put someone using [_add_blank_target _ATS_](http://www.ats-lang.org/)?  If a typical _Haskell_ developer is a _theorist_, how do we classify someone working with _Agda_ or _Coq_...?

We are simplifying all of this and consider programmers to be either _pragmatists_ or _theorists_ 
and take an oversimplified (binary) and a somewhat stereotypical view of what these terms mean.
_side_note_end


This post argues that both traits are important.  We will dig deeper into both mindset by analyzing some examples. 


## Conversations

Perhaps not surprisingly, theorists are not all equally disappointed about logical software defects.
Explaining this diversity is a price I have to pay for bundling all theorists together. 
Think physics, it is very theoretical, yet theoretical physicists are happy to do hand waving arguments. In contrast, there is no such thing as "a partially correct" in mathematics.  IMO, this stricter view plays a role in how some of us approach programming. 


Alice: "We have a concurrency issue in our code"   
Bob: "Are you talking about a production issue, a failing test, or is it purely theoretical?"   
If Alice gets a change to explain the race condition, she may hear this response:   
Carol: "We done it like this before and everything was fine". 
  
Alice (nick name Negative Nancy, a theorist) considers all logical defects to be a disappointment.  Bob and Carol are pragmatists and approach logical defects in a more relaxed way. 
It seems like a good idea for Bob and Carol to understand a little bit about how Alice approaches programming, and vice versa. 
Let's analyze this dialog a little bit.  

To Alice a logical issues are kinda a big deal. She will consider it very hard to reason about code sprinkled with logical flaws. 
There is actually a good reason for this. 
Some logical flaws we examined in my previous post[^logicalflaws] are quite isolated but it is hard (if not impossible) to understand the full impact of many of them.  I am a like Alice, working in a complex imperative program or a poorly written functional code results in me forming a large mental repository of issues and their unclear impacts. Maintaining it is a tough mental effort.   

[^logicalflaws]: See [_add_blank_target Extraneous nature of abstraction](2022-08-30-code-cognitiveload.html#extraneous-nature-of-abstraction) and footnotes. 

Here is a story from my personal experience.  A few year back I done a code review session with two (very capable) developers.  I showed them one of my "bug stashes" in the project we all were contributing to. It had to do with a logically brittle use of inheritance.  I demonstrated the process I go through 
to verify the brittle bits.  This session was very productive, we all learned something from it, and this code was refactored later. 
Their response is something I still contemplate:  "We do not go through such steps, we just assume it will work". 
For me it was a learning experience I still think about, I realized that developers non only create code differently, but also very differently consume it. 

Returning to Bob, he is a pragmatists.  Notice that Bob has stratified all contexts he assumed relevant to Alice's finding: production issue, failing test, and theoretical. To Bob, a logical issue in code is just a part of life.   "It has bugs, it's called software."  This empirical mindset, in some ways, is healthier[^rnt]. 
It is not unusual for empirical reasoning to dismiss theoretical concerns, however in this case this is likely to be wrong. 
It is hard to spot or even assess impact of some bugs (e.g. race conditions) using testing or other observation based methods.

[^rnt]: I have discussed RNT (repetitive negative thinking) in my [_add_blank_target previous post](2022-08-30-code-cognitiveload.html) in my previous post. 


Carol response suggest an empirical mindset as well. Carol has generalized previous observations of a working product and that generalization overrides Alice's warning bells. Going from specific to general is what empirical process is about. 
Proper empirical reasoning will question, even invalidate, previous "hypothesis" if new evidence provides reasons for doing so, but Alice is not providing any empirical evidence. 
Alice argument is purely deductive, it could be helpful if she came up with a test that exposes the concurrency problem she has identified. 

Bob: "We are starting the new frontend project, I propose we keep using XYZ PL, but maybe we could add a new library to our setup?"   
Alice: "XYZ is fundamentally broken, we should move to something sound, like Reason."   
Carol: "Alice, we do not know that PL, this will put the project at risk!"   
Alice: "We know so many problems about XYZ, XYZ puts the project in jeopardy too".   

You may be wondering why I call JavaScript XYZ?  Well, XYZ was intended as a placeholder. 
Alice would like to use tools that help, not inhibit her deductive process and are logically sound.
Alice has witnessed her colleagues (and probably herself) trip over XYZ unsound design numerous times. She considers 
use of XYZ akin to building a house on a broken foundation.  
Bob and Carol insist on tools that have good IDE support, good debugging, and are familiar even if logically unsound. 
Pragmatists want to reason about execution flow, browsers use _JS_, thus, they may want a language very close to _JS_. 
Carol has a very valid point too, one that Alice may have hard time accepting.  I admit, I sympathize with Alice, even if Carol is probably right here. 


Alice: "You are memoizing a computation that is not referentially transparent"   
Carol:  "I have manually tested it and, besides, this code has a 100% test coverage"   
Bob:  "We are assuming that it is referentially transparent and want consistent results when we use it"   
Alice: "Remember we patched a bug by updating shared state in the middle of this computation, did you retest this scenario?"  

This code review session shows a benefit of having someone around who keeps a repository of potential issues in their head.
I have noticed that developers are typically surprised when computation behavior keeps changing, yet are mostly not willing to engage with the concept of referential transparency.  I also think many do not think about what 100% test coverage implies and what it does not[^limitations]. 

[^limitations]: I will repeat myself here, it does seem that there is a more general lack of understanding about the limitations of empirical reasoning.  Few people think about physics as a set of simplified mathematical models that only approximate reality. 
Few people look deeply for bias in biological studies.  100% test coverage is in the "we tested it and it is correct" category. 

Let's get a little philosophical:

Alice: "Ideal code to me is one I would still be proud of after 10 years"  
Bob: "If you think about your 10 year old code as perfect, you learned nothing in these 10 years"  
Alice: "I am looking for something as timeless as mathematics"  
Carol: "Mathematics keeps improving and changing I am sure, everything does"  
Alice: "No, it only grows, it has not changed its mind in 100+ years"

This is almost an exact copy of a conversation I had with some of my coworkers. Formal reasoning is a foreign concept to many programmers.  The immutability analogy I have used before works well here: mathematics is immutable while empirical sciences mutate in-place.  

_side_note_start
We consider PLs that reach a certain threshold of usage as immortal. 
A PL could be immortal but the ideas that went into its design may have died a long time ago. 
For that reason alone, it is a good idea to try to broaden the amount of formalism.
_side_note_end

These conversation examples were not intended to be exhaustive. They are somewhat biased, it is hard for me
to present the other view point this way. 
I invite you to think more about the differences between pragmatists and theorists in both creating
and consuming the code.  I invite you to think more about how each side thinks.   
I need to emphasize, this is not a binary separation where everyone is are either pragmatist or theorist. Many of us have both traits, just one is more dominant then the other and they tend to not manifest at the same time. 


_side_note_start
For a very long time I could not figure out why certain decisions about PLs, popular libraries, or programming projects are being made.
I could not understand why certain bugs remain not fixed, why there are no deprecation attempts, why certain 
decisions have been made in the first place.  The idea of software as something that is made from loose ends and somehow made whole by a testing effort feels wrong to me.  The empirical mindset I have tried to explain here is my best attempt
at understanding these things.  E.g. I cannot explain in any other way why Java maintainers decided not to deprecate standard library classes where `equals` is not symmetric (the list of such examples is long[^why]).  If I was in charge of designing programming courses, an example exercise would look like this:   

> &emsp;  *There is a common belief that TypeScript compilation flags like `strictNullChecks` prevent escaped `null` and `undefined`.   
&emsp;  Exploit how TS defines variance to create a function that has `number` as the return type but it returns `undefined` for some of its input parameter values.* 

The pragmatist take (as I see it) is: the ingredients you use do not need to be sound, there are just a part of noise. I do not agree with this argument, but at least I think I know what the argument is. 
_side_note_end

[^why0]: I created a long list of logically unsound decisions made by PLs and some mainstream libraries in this [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fn12)
in my previous post, I wrote a [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fnref19) about Haskell as well.

[^why]: I will partially repeat a list I came up with for my last post.  Example of non-symmetric equals is `java.sql.Timestamp` used with `java.sql.Date` or `java.util.Date`, these remain used as standard JDBC mappings for DB columns, the usage will show no deprecation warning.  Working in JS often feels like explosives engineering.   
I wrote a blog series about [_add_blank_target TypeScript Types](/tags/TypeScript-Notes.html) and ended up presenting a lot of complexities and gotchas that probably surprise any TS developer.  
How do Hibernate users prevent this [_add_blank_target concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html)?  
I remember Grails as a land mine of issues, I wrote an 11 part [_add_blank_target blog series](http://rpeszek.blogspot.com/2014/10/i-dont-like-hibernategrails-part-11_31.html) about it back in 2014.  
Returning to Java, its implementation of `Optional` is often [_add_blank_target criticized](https://www.sitepoint.com/how-optional-breaks-the-monad-laws-and-why-it-matters/). Java array variance is fundamentally broken.
Java Streams are broken as well:  if you execute a stream twice the second attempt will fail. This is probably the first and only attempt at dynamically typed linear types &#128578; and is bound to create interesting intermittent issues.   
I wrote a [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fnref19) in the previous post about Haskell ecosystem issues as well. 


## Negativity

There is quite a bit of negativity around us, programming community is not exempt from it. 
I will wrap up with some loose thoughts about negativity in the context of deductive and empirical mindsets. 

Venting frustration, IMO, does not improve how either the "venter" or the "ventee" feel. It only fuels the negativity. 
IMO, the best way to fight negative emotions is to employ the deductive.  Figuring out the underlying context that causes negativity can save
a conversation and simply engaging in that search can protect you.  Using logic to confront emotions is a form of what psychologist call [_add_blank_target cognitive restructuring](https://www.apa.org/topics/anger/control).

Criticism of bad code is bound to be unpleasant to its authors. Even worse, a lot of code can be criticized on purely logical grounds. This can be interpreted
as a critique of the author's competence and, I am sure, is a big source of negativity and tension.  Let's look at this differently.
Look at some of the empirical sciences like neuroscience and see how much of the knowledge got adjusted if not invalidated[^medical].
IMO, programming is on a very localized and very accelerated path of the same process.
Is "bad code" phenomena partially related to the empirical nature of programming?  I think it is[^turning]. 

[^medical]: The main reason typically presented for this is: improved ability to make and measure observations. In programming, we witness things going south in front of our eyes, we typically do not need to wait for a new testing technology. 

[^turning]: In my last post I included a trivia about Turing original paper having several bugs and his adviser's (Alonso Church's) work being bug free. This is also related
to the imperative vs denotative discussion. Here is the link to that footnote: [_add_blank_target Turing](2022-08-30-code-cognitiveload.html#fn11). It should make us feel better about ourselves if Turing himself wrote bugs.

Theorists are likely to devote a significant effort into learning. 
How does it feel to not be allowed to use what you worked hard to figure out?
Examples like [_add_blank_target How to stop functional programming](https://brianmckenna.org/blog/howtostopfp) come to mind. 
This is something that can be changed on a small scale but there are a (very) few places who care to do so. 
The job market for functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust had toped the weekend use stats based on stackoverflow surveys[^weekend].  There must be quite a few frustrated programmers out there.  I have been in that position and I know it is mentally hard. 
I have argued that the deductive process plays important role and, IMO, it is in the interest of the industry to treat the "theorists" minority better.   

[^weekend]: Repeating some of what I wrote [_add_blank_target here](2022-03-13-ts-types-part6.html#about-simplicity):  Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. Here is one link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  In [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/) Rust moved ahead of Haskell. 
The job ranking (based on the UK's [_add_blank_target IT Jobs Watch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do)) puts Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017. 
This ranking is possibly exaggerated too, lots of jobs list Haskell as good to have but will have you code in PHP.  This bias exist
in any language but is stronger for something like Haskell than say COBOL. 
    


## Final toughs and summary
 
This and the previous ([_add_blank_target Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html)) post were high level rants about topics that have changed how I think about programming. 
Both looked at the programming from outside, facilitated a critique of the programming process, and forced thinking about the human factor.  Both allow me to more critically look at myself.

In this post we considered programming to be a mostly empirical process and we asked the question: are we good at this empirical thing?    
We have looked at programmers as individuals interested in either the empirical or the deductive. Can you place yourself into one of these camps? Can you place your coworkers? This is not always easy, sometimes the placement will depend on the situation. 
I hope I have convinced you that understanding these mindsets is helpful.  IMO, it provides extra context when interacting with others and a mirror.  This post presented my honest opinions, these are not scientific claims, just opinions. 

The odd discourse between formal and empirical is not unique to programming. 
I still remember a few anecdotes about "a mathematician, a physicist, a chemist, ...". 
Empirical vs deductive, if one needs to budge then the empirical wins, deductive has to move. 
Some argue that computer science and programming could benefit from a stricter application of formalism. This position is rather rare  (restricted to functional programmers). 
The difference is where the source of truth is.  In a pure empirical world that source has to be what we observe.  In programming there can be just enough determinism to favor a stricter application of a deductive process.  I hope I made a convincing case for us to try harder for a peaceful coexistence of both in programming.

Thank you for reading!   

## Unexplored

Psychological cognitive biases and how they impact both groups. In particular _bandwagon effect_ (popular == good), _commitment bias_ (we done it like this before, it has been tried and tested, all this effort to learn something...), _omission neglect_ (things we do not know are not important), _bikeshedding_ (syntax vs semantics, etc). 
In particular, syntax vs semantics would be an interesting discussion.  Theorists tend to think about syntax as a bikeshed, pragmatists like to talk about liners and formaters, clearly both aspects are important.  

Imperative vs denotative in the context of both mindsets.  IMO, this is not a clear cut (few things are), I consider myself very theoretical, yet I like parts of my code to be imperative.  

Impact of education on the development of either mindset.  

As we have discussed, developers approach bugs differently. This is how my interests in figuring out different programmer mindsets has started. There is a different way to look at this. Consider these 3 axes: "It has bugs, it's called software" is the origin, testing is one axis, 
"correctness by design" abstractions and type safety is second, a mental repository of 
possible issues and their impacts is third.  Pragmatists are on the first axis, theorists on the second.  We need the name for the
third group, lets call them _perfectionists_. I came to FP on a correctness wagon, theory and improved coding efficiency are for me an added bonus. The question is how the _perfectionists_ fit into this picture.  This post bundled them with theorists, this was likely an oversimplification. 

Implicit contexts in communication between programmers. There appears to be much more to explore here. Consider programming internet discussion forums (IMO, a Manhattan of communication skills, if you can make it there you will do really good in your project team).  One can observe all kinds of context related bias issues (e.g. some redditers do not consider reading to be a prerequisite for responding) or lack of context clarity (e.g. heavily downvoted posts with no comments). 
Moving away form discussion groups, teams tend to create their own localized contexts (unique vocabulary, proprietary technical solutions) which is often not ideal.  IMO, context clarity is to communication what referential transparency is to programming.  IMO, context is to communication what causation is to empirical science. 

I am sure there is are many more interesting angles to explore here. 
