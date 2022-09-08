---
title:  Let's agree to be different. On empirical and deductive nature of coding. 
lastmodified: Sep 20, 2022
featured: true
summary:  On Empirical Nature of Coding. Let's agree to disagree
toc: true
tags: Haskell
---

> *"When the going gets tough, the tough get empirical." quote from columnist Jon Carroll* 


I will discuss (on a very high level) empirical, experimental and deductive aspects of programming.  Distinction between empirical and deductive in itself seems just a useless curiosity and it probably it is.  I have convinced myself that programmers tend to favor empirical or favor deductive. IMO, we are placing ourselves into 2 camps. 
This division impacts how we program and communicate. This division can create stress for ourselves and for others. Communication is the end goal of this post. 

The topic for this post came from a realization I had when thinking about [cognitive loads in programming](). 
I may be obvious to some of you, but it was not obvious to me: programming is largely an empirical process. 
I will argue that the pragmatic empirical mindset is predominant among programmers, while theoretical deductive mindset is more rare. 


What the others mean, how they reason, or what is important to them are the contexts in human communication. Good communication without basis of common interest requires extra effort, one that includes attempt to understand these contexts. 
In this post I will try to present my honest opinions about the empirical and deductive mindsets. You may disagree with my description, please let me know if you do! The point is to get these contexts right and to have all of us think about them a little.  Search for context is often the most important element in empirical reasoning, if we want to be any good
at this empirical thing we better improve on the task of understanding each other! 

Fairness and lack of bias are required in proper empirical reasoning as well, both are rare but beautiful if encountered in human interactions.  My programming interests are quite theoretical, and thus, this post is likely to have some unintended bias. Also, these are my opinions, not an attempt at a scientifically sound reasoning.  

I have not found anyone talking about this, I am not following academic research in any related area. 
The topic of [empirical software engineering](https://en.wikipedia.org/wiki/Empirical_software_engineering) is relevant to programming and the empirical method, but is not really what I will talk about.  



## Empirical vs Deductive 

I my experience, developers do not use this terminology. Hey, in my experience, I do not use this terminology so a recap is in order. 

You are likely to hear at least these 6 terms: inductive, empirical, a posteriori, deductive, rational, and a priori. 
This sounds like a beginning of a catchy song.  There is also informal and formal but it breaks the rhythm, so I skip these for now. Actually, in this post I will mostly try to use _empirical_ and _deductive_. 

Empirical (to avoid using inductive[^inductive]) reasoning draws general conclusions from observations.  E.g. certain software functionality will work because we tested it.
Thinking about software testing as an empirical process allows us to consider things like context bias (did we miss a range of scenarios?), observation sample size (did we test enough?), correlation vs causality (e2e tests are unstable, why?), if observations are balanced (adequate coverage of test scenarios across functional areas)...
But is it even more fun to think about a full blown empirical process, to think about both experiments and observations. 

Deductive reasoning goes from general knowledge to specific conclusions. E.g. certain software functionality will work because of type safety or because it is a straightforward application of something else we believe works. In this post, deductive represents a wide range of thought processes: from tedious mental verification 
of values and types (oh, this value was supposed to be a positive number why is it negative?, why this function was never called?...) all the way to formal reasoning.  

Empirical and deductive work in tandem and both are essential.  IMO, we (as individuals) prefer to use one more than the other. 
Understanding more about these preferences will be the main topic of this post. 

[^inductive]: This inductive reasoning should be confused with mathematical (or structural in PLT) induction. Believe it or not it sometimes is. 

_side_note_start
Formal reasoning[^formalprog] is the only approach humans have figured out to solve complex problems correctly on the first go (without trial and error associated with empirical reasoning).  In this post, I am not separating the formal out, it is bundled into deductive. 
Isolating formal reasoning process could be relevant to this post.
I have struggled on this decision, at the end I decided to simplify things and keep formal reasoning bundled into the more broad deductive process.
_side_note_end

[^formalprog]: Examples of formal approaches popular among FP-ers could be equational reasoning, use of logical implication (e.g. with Haskell type class constraints), use of mathematical or structural induction (e.g. with inductive types like a functional list prove properties of, say, the implementation of `map`). 


## Coding by experimenting and observing

Trial and error is how a lot of programming is done.  We write some code (_experiment_) and then _observe_ the result using tests, a debugger, looking at the logs, or simply observe how the app behaves ignoring other diligence.  This is, basically, an empirical process. 
Personally, I am a little afraid of experimenting with certain types of code and I try to think through all scenarios, I could miss something but my mental process includes a lot of deductive effort. I have worked with many programmers who seem to operate in a similar way.  However, I still need to run the code to _observe_ it working, make changes, and rinse and repeat. Despite some deductive elements, this approach is still very experimental in its nature.  

How about working in code that is a complete mess? 
We make experimental changes and then run the tests, and if there is not enough coverage we debug or trace the heck out of it[^heck], right? 
We may try to reason about what solution is likely to work, but that is not much different from experimental alchemists trying not to blow themselves up. I can recall several projects where all my understanding was derived from debugging or tracing. 

[^heck]: Notice, I did not say "debug the crap out of it" because that could imply that we are making an improvement by removing it. 



Consider these tools and processes:  debuggers (_observe_ execution of statements), loggers (record _observed_ behavior), testing (_observe_ app behavior), TDD (pre-define _observation_ outcomes for the code), design patterns[^patterns] (generalize code _observed_ to work well).   
I encourage you to think about tools and processes that are targeting deductive process, there are some!

[^patterns]: Design patterns are an interesting bunch because they include some deductive work.  E.g. factory decouples idiosyncratic aspects of object construction and decoupling is known to be beneficial. 
However, this is not that much different from, say, a wildlife biologists generalizing observed behavior of individuals to a whole species using a known symbiotic relationship in their argument.  
Hard to resist: They say symbiotic, we say decoupling... ;)   

Let's talk process bureaucracy.  Working in empirical world means procedural protocols.  Scrum is a procedural protocol: 
consider continuous improvement process with retrospectives or team velocity calculation.  These are all very empirical. 
In contrast, deductive needs a cushy couch. 

Let's talk context,  is often the hardest and most important part in empirical reasoning.  E.g. in pharmacology: drug response depends on many factors like drug interactions, dosage, population stratification, other medical conditions...  In biology:  animal behavior depends on many factors like activity factors, stress factors, location factors...  Programmers are exposed to context bias too.  When troubleshooting a bug the context is the exact scenarios causing it.  Performance issue could be caused by many factors, an application could be slow only in certain scenarios or only in certain environment configurations.   Being a good programmer could mean an ability to consider all contexts.
The related _correlation vs causality_ problem is something programmers face as well
(e.g. the underlying cause of an issue is more subtle then what the observations suggest).   

In this analogy programmers are a crazy bunch of experimental scientists. In other words, to be good in programming 
you need to be good in empirical reasoning. Am I convincing you that programming is often an empirical process? 





### Experimental process and high extraneous loads

In my last post I discussed [cognitive loads in programming]().  Messy code has _high extraneous load_. 
If you are not familiar with this term, substitute _high extraneous load_ with messy code, however the term has a broader meaning.  

I think we are in a position to put a 1 and 1 together:  

> &emsp; _The experimental nature of programming is a consequence of its high extraneous load and also one of its main causes_ 

The bigger extraneous load the more we experiment.  The more we experiment the bigger the extraneous load gets.  
To work on a complex code we are effectively forced to experiment.  Adding more experimental code only increases the complexity.  

This is a feedback loop.  To break this loop (and control the complexity) we need to rewrite or refactor parts of code.  These involve some deductive process.  Some amount of deductive is essential, deductive process is what can break the feedback loop.  

There is one notable exception where piling up experiments does not result in extraneous complexity, we will discuss it next. 


### Type safe experiments

This is my favorite approach to programming. 
In the presence of a nice type system, coding can become solving jig-saw puzzles. 
A similar process can work even in [TypeScript](). 
This approach could include an interactive deductive process (asking compiler a series of type questions, something akin to type hole driven development[^idris]) or just making a guess and trying to see if the pieces fit. 
The second approach is a form of experimentation. 
Working with _hlint_ (Haskell linter, other static tools that do similar things exist) is lots of fun because I often need to get _a solution_ in and the linter will replace it with a much nicer fused code.  

This jig-saw puzzle approach results very nice and clean programs, is addictive and a lot of fun, but is not commonly used at large.    

[^bruteforce]: Not a brute force IO. 

[^idris]: TODO


### Programming as empirical science

It is hard to resist pushing analogy like this to its limits, even if things seem more and more farfetched. 
So before we move to discussing human interactions, lets think more about programmers as a bunch of experimental scientists.
If we are experimental scientists, we can as well try to do it well.  

Let's focus on 2 cornerstones of scientific experiments: reproducibility and big sample sizes. 
and restrict out observation method to automated testing.   
FP-ers will immediately notice two related concepts: _referential transparency_ and _property testing_ (e.g. QuickCheck). Haskell is sometimes called _the best imperative PL_[^best_imperative].  I propose that FP is the best empirical programming method.  Yeah, could I be a little biased, just a little :)?

[^best_imperative]: "Haskell is the world’s finest imperative programming language" famous quote, probably originated in this paper [Simon Peyton Jones on Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)

How about other nuances of empirical process like the context bias?  A lot of this, I am sure, is well understood by experts in some areas of programming (e.g. benchmark testing, UX testing).  I had a fun discussion with my wife, she is a statistician and data scientist working in the Pharma sector.  We were discussing if a design of a clinical trial could be adjusted to compare two versions of a software application and analyze several aspects in one go (e.g. non-termination and performance). 
There are some intriguing similarities.   Obviously, humans are much more complex than programs and more targeted approaches, I am sure, work better.  But it was a fun discussion and one that convinced me even more about the empirical nature of programming. 


CS and programming are often associated with mathematics and logic but I argue that programming is more empirical than deductive. 
I was surprised when I realized this.  Talking about humans being more complex than software, let's stop discussing programs and start discussing programmers (the observers). 


## Pragmatists and theorists

I am using terms theorist and pragmatist somewhat colloquially, but the meaning is close to how the terms are used elsewhere.  

Some programmers dive deep into FP and learn formal reasoning.  Sometimes a mathematician (this was the case with me) makes a career conversion and becomes a developer.  Formal reasoning can be a very strong attraction for some individuals or can be just a lot of stress and frustration.  Most software developers are very pragmatic and are not attracted to things like formal logic. 

If you listen to a functional programmer talk, you are likely to hear these terms: "reasoning about code", "principled computation", "computation laws", "correctness by design".  These people are likely to study things like lambda calculi, operational semantics, category theory, type theory...  All these things come with formal proofs and will result in a very specific mental training.  _To this group programming is more of a deductive process._  

These things do not resonate with vast majority of programmers who have more pragmatic mindset.  You are more likely to hear these terms:  "testing", "TDD", "hacking" (meant as a compliment), even "design patterns" (though rarely these days).  They will study new frameworks, tooling, and PLs. They want the tools to be mainstream and popular to pass their test of practical usefulness.  I will argue that _To this group programming is more an empirical process._[^pragmatists]   

[^pragmatists]: The association of pragmatism and empirical process is not unique to programmers, e.g. if you google "pragmatists vs formalists" today you will probably get a link to this quote "Formalism follows deductive approach whereas pragmatism applies empirical approach" from this legal philosophy paper: [Formalism vs. Pragmatism in Legal Philosophy](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3423036)

Pragmatist will say "it works because we tested it",  theorists (my name for the first group) will say "it is correct by design" (or closer to Dikstra: "we tested it and we know it does not work").  Pragmatist will select tools that provide good 
ability to observe how the code runs.  To a pragmatist reasoning about code often means studying its execution flow. 
Some pragmatists go a step further and will only select a PL that allows them the best control over execution (e.g. 
_JS_ is used by browsers so this is what they will want to write). This could be caused by ([justified](TODO)) distrust for abstractions.  
Theorists want tools that support, rather than inhibit, a more theoretical deductive reasoning.  They typically want to reason on a higher level than the execution flow.  Some even view programs and execution as decoupled concerns[^fussion].

[^fussion]: E.g. rewrite rules in Haskell. 

One distinction I have noticed between heavy deductive thinkers vs empirical / inductive mindsets is their favorite approach 
to learning.  When learning, "a theorist" may prefer to finish a whole book before writing a single line of code, while 
inductive thinker will prefer a more hands on learning from code examples. The top-down thinking extends to top-down learning.  Me and my wife are in the first group. 
I had to force myself to write some code when going through [TAPL](), I decided to create a public github repo to give myself incentive to "code as I learn" with [CTFP]() and [T(ype)DD in Idris]().  Theorists can adjust and learn to like some bottom-up learning (and benefit from it) but it probably will be an adjustment.  Theorists treat learning as a murder mystery and want to learn ASAP who done it,  pragmatists know it will be the butler.  

On the flipped side, a pragmatist may have a hard time seeing a benefit of spending several months studying, say, a one line of code[^one-line].  Pragmatists are less likely to search for deep understanding or have theoretical interests.  IMO, this has more to do with with interests than capability.   

[^one-line]:  I git a deep hole by using [Free Monad](TODO) as example of such a line in my previous post. 
An interesting question is why learning a book in say 5 months is considered OK but learning one line of code is considered weird? 

In extreme cases, this yields a separation: "this abstraction is too complicated to use because is too theoretical" vs "this abstraction is too complicated because it lacks theoretical backing".  The 2 groups may find it difficult to communicate 
"this is nice" and "this is terrible" have a very different context when spoken by pragmatist and theorist.  

This post argues that both traits are important.  I need to emphasize that we all have both characteristics but one tends to be dominant.   


## Conversations

Alice: "We have a logical flaw in our code" could solicit this reply:   
Bob: "Are you talking about a production issue, a failing test, or is it purely theoretical?"   
If Alice gets a change to explain the issue, she may hear this response:   
Carol: "We done it like this before and everything was fine". 
  
Alice (nick name Negative Nancy) has a theorist mindset.  Bob and Carol are pragmatists. 
The conversation could be in an impasse, it could be hard to reset it. 
Being shut down may not be as hard for Alice as having to work with code she now knows has one more flaw. 
It seems like a good idea for Bob and Carol to understand a little bit about how Alice approaches programming, and vice versa. 
Let's analyze this dialog a little bit.  

To Alice a falsehood is kinda a big deal.
Mathematicians and logicians are trained to think about falsehoods very differently.  Arriving at a falsehood is "game over" in formal logic.
Theorist could consider logical correctness as something of a paramount importance, something that overrides other consideration[^formal]. E.g. Alice may have a hard time understanding that "it is not a bug is a feature" is even a thing.   
Many logical flaws we examined in my previous post [TODO](TODO) are quite isolated (e.g. `equals` becomes symmetric if you avoid offending data) but it is hard to understand the full impact of some of them.  A theorist is more likely to worry about these things. I am little bit like Alice, working on a complex imperative program or a poorly written functional code results in me forming a large mental repository of issues with unclear impacts. Maintaining it is a tough mental effort.    
When the going gets tough, the tough get empirical.
[^formal]:  This is actually definitional, formalism is typically defined as a strict adherence to some set of principles (here logic).  

Here is a story from my personal experience.  A few year back I done a code review session with two (very capable) developers.  I showed them one of my "bug stashes" in the project we all were contributing to. It had to do with logically brittle use of inheritance.  I demonstrated a process I go through 
to verify the brittle bits.  This session was very productive, we all learned something from it, and this code was refactored later.  
Their response is something I still contemplate:  "We do not go through such steps, we just assume it will work".  
For me it was a learning experience I still think about, I realized that developers non only create code differently, but also very differently consume it. 

Returning to Bob, he is a pragmatists.  Bob exhibits inductive reasoning, notice that Bob has stratified all contexts he assumed relevant to Alice's finding: production issue, failing test, and theoretical.  Adding "or is it purely" in front of "theoretical" suggest that this option feels the least concerning to him.  To Bob, a logical issue in code are just a part of life.  He will not lose much sleep over it.  We test, find and fix.  This empirical mindset, in some ways, is healthier[^rnt]. 
However, proper empirical reasoning does not dismiss theoretical concerns.  

[^rnt]: I have discussed RNT (..) in my previous post. 

Carol response suggest an empirical mindset as well. Carol has generalized previous observations of a working product and that generalization overrides Alice's warning bells. Going from specific to general is what empirical process does. However, proper empirical reasoning will question, even invalidate, previous "hypothesis" if new evidence provides reasons for doing so. 

Bob: "We are starting new frontend project, I propose we keep using XYZ PL, but maybe we could add a new library to our setup?"   
Alice: "XYZ is fundamentally broken, we should move to something sound like Reason."   
Carol: "Alice, we do not know that PL, this will put the project at risk!"   
Alice: "We know so many problems about XYZ, XYZ puts the project in jeopardy too."   

You may be wondering why I call JavaScript XYZ?  Well it does not need to be JS, and XYZ it is here just an an example. 
Alice would like to use tools that help, not inhibit her deductive process and are logically sound.
Alice has witnessed her colleagues (and probably herself) trip over XYZ unsound design numerous times. She considers 
use of XYZ akin to building a house on a broken foundation[^foundation].  
Bob and Carol insist on tools that have good IDE support, good debugging, and are familiar even if logically unsound. 
Pragmatists want to reason about execution flow, browsers use _JS_, thus, they may want a language very close to _JS_. 
Carol has a very valid point too, one that Alice may have hard time accepting.  I admit, I sympathize with Alice, even if Carol is probably right here. 

[^foundation]: TODO

I invite you to think more about the differences between deductive and empirical process of both creating
and consuming the code.  People typically think about logical deduction process as something akin to formal verification, a method used to verify that code is correct. Consider deductive processes that goes into troubleshooting as well.   

In the context of my previous post, theorists and pragmatists experience cognitive load very differently. Extraneous loads, in particular, tent to be harsher on theorists.  

I need to emphasize, this is not a binary separation where everyone is are either pragmatist or theorist. We all have both traits, just one is more dominant then the other.  Hopefully, a little insight into how other people think, what is important to them, what is stressful to them will fill in some context gaps and improve the communication. 


 
## Pragmatists are probably happier

An interesting line of thought is reconstruct the process that leads to figuring out a bug like this.  
   
Identifying that there is a problem is an _empirical_ process. Typically the unexpected behavior is _observed_ by a user or tester.  Two things can happen to address it: various exploratory
code changes are made to work around the issue (this is still _empirical_, it is an _experiment_),  or an effort is made to identify the root cause (an underlying logical flaw).  
The second approach means an effort to identify a proposition (or a property) that is violated. 
Does this smell formal to you?  
...It also helps immensely if the PL can syntactically express
that proposition,  even better if the troubled code uses parts of that syntax. 
Does that sounds more and more like functional programming?

Deductive process is a cognitive ability, so is the empirical process.  It appears to me that an ability to identify gotchas is related to possessing the first one.  





## More on Negativity

Bad code criticism is contentious and is bound to hurt its authors. Bad code feels like a ... of incompetence.  
This criticism feels like aggression and can solicit aggression in response.  Why is this happening?  I want to point out one tiny smidgen of a reason. 
Look at some of the empirical sciences like medicine, neurology and see how frequently the knowledge gets adjusted if not invalidated[^medical]. 
IMO, programming is on a very localized and very accelerated path of the same process. 
In other words,  this problem is not unique to programming.  
Is "bad code" phenomena partially related to the empirical nature of programming?  I think it could be.   
To some extent, "bad code" is the nature of things in programming. 

[^medical]: The main reason typically presented for this is: improved ability to make and measure observations. 
In programming, we just witness things going south in front of our eyes. 
   
Discussion forums seem to be the places programmers go vent their frustration.  
This may not work, this approach will fuel more frustration rather then lower it.  
Let's talk about out of context replies in programming forums.  
Consider a link-post in a programming forum like r/programming.  
It appears that many redditers do not consider _reading_ the linked post a pre-requisite for leaving a comment or down/up voting it. 
I have observed cases were a redditer clearly _did not read_ the full content of the message before responding it with a scolding remark.  
Such behavior is rude and should be criticized back.  Assuming a context and using this assumption in critique is bad inductive reasoning.  It is a form of _context bias_ (something we have discussed in this post).  Programmers who do that in a discussion forum may also do that in their programs.  To programmers who do that: you can learn how to improve your coding skills by trying to be civil.  

Theorists are likely to devote a significant effort into learning the things that theorists like. 
We can imagine how it feels (well I can since I lived it) to not be able to use what you worked hard to figure out.
Examples like [How to stop functional programming](https://brianmckenna.org/blog/howtostopfp) come to mind.  
This is something that can be changed on a small scale but there a (very) few places who care to do so.  
The job market for functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust top the weekend use stats based on stackoverflow surveys[^weekend].  There must be quite a few frustrated programmers out there.  I have been in that position and I know it is mentally hard.  

[^weekend]: Repeating some of what I wrote [here](2022-03-13-ts-types-part6.html#about-simplicity):  Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. Here is one link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  In [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/) Rust moved ahead of Haskell. 
The job ranking (based on the UK's [_add_blank_target IT Jobs Watch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do)) puts Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017. 
This ranking is possibly exaggerated too, lots of jobs list Haskell as good to have but will have you code in PHP.  This bias exist
in any language but is stronger for something like Haskell than say COBOL. 
    
I wrote about RNT () in my last post.  


## Side Notes

In this post we asked is programming can learn from empirical process. Putting this question on its head seems interesting. 
Empirical sciences (e.g. medical sciences, neuroscience, nutrition science...) knowledge gets adjusted if not invalidated from time to time. This is reminiscent of finding a bug in a program. Programming does it in a very accelerated fashion with immediate feedback from testing and app use.


Empirical scientists to do dismiss theory. Programmers often do. Arguments that formal process is inadequate to solve
all programming problems is used to dismiss it completely. Most programmers will never do a single equational reasoning, 
will never experience formal inductions process (e.g. Type Class constraints in a Haskell-like language).   



Good communication is facilitated by common interest more than by common knowledge. 

Pragmatist sometimes prefer to code in the language that is used...
Could be caused by (justified) distrust for layers of abstractions. 