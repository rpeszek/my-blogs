---
title:  Let's agree to be different. On empirical and deductive nature of coding. 
lastmodified: Sep 20, 2022
featured: true
summary:  On Empirical Nature of Coding. Let's agree to disagree
toc: true
tags: Haskell
---

> *"When the going gets tough, the tough get empirical." Jon Carroll* 

This and the last ([Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html)) post share a high level information that has changed my emotional and technical approach to programming.  

I will discuss (on a very high level) empirical, experimental, and deductive aspects of programming. 
This distinction is fun to analyze (and a somewhat unusual way to look at programming) but it seems mostly a useless curiosity in itself.  I have convinced myself that programmers tend to favor empirical or favor deductive. IMO, we are placing ourselves into 2 camps (I call these camps _pragmatists_ and _theorists_). 
This division impacts how we program and communicate. Improved understanding of both mindsets is the end goal of this post. 

The topic for this post came from a realization I had when thinking about cognitive loads. 
I may be obvious to some of you, but it was not obvious to me: programming is largely an empirical process. 
I will argue that the pragmatic empirical mindset is dominant. 

What the others mean, how they reason, or what is important to them are the contexts in human communication. Communication without the basis of common interest is hard. Good communication requires an effort of understanding these contexts.  IMO, _empirical, deductive, pragmatist,_ and _theorist_ are a good terminology to analyze some of the current discourse (especially about FP). 
In this post I will try to present my honest opinions about the empirical and deductive mindsets. You may disagree with my opinions, please let me know if you do. The point is to get these contexts right and to have all of us think about them a little.  Search for context is often the most important element in empirical reasoning, if we want to be any good
at this empirical thing we better improve on the task of understanding each other! 

Fairness and lack of bias are needed in proper empirical reasoning as well, both are rare but beautiful if encountered in human interactions.  My programming interests are quite theoretical, and thus, this post is likely to have some unintended bias. Also, these are my opinions, not an attempt at a scientifically sound reasoning.  

I have not found much discussion about this, I am not following academic research in any related area. 
The topic of [empirical software engineering](https://en.wikipedia.org/wiki/Empirical_software_engineering) is relevant to programming and the empirical method, but is not really what I will talk about.  

Retrospecting on my software programmer career,  I recall good and bad things. The good had a good communication, the bad had a bad communication of some sort.  Pragmatists vs theorist is just a part of a bigger puzzle, I am going to explore that part here. 

## Empirical vs Deductive 

I my experience, developers do not use this terminology. Hey, in my experience, I do not use this terminology so a recap is in order. 

Scientists and philosophers use these 6 terms (or more): inductive, empirical, a posteriori, deductive, rational, and a priori. 
This sounds like a beginning of a catchy song.  There is also informal and formal but it breaks the rhythm, so I skip these for now. _Practical_ and _theoretical_ are great terms to describe a mindset, while _empirical_ and _deductive_ describe the thought process. 
In this post I will mostly try to use _empirical_ and _deductive_ when referring to the though process, I will use _pragmatist_ and _theorist_ when referring to people. 

Empirical (to avoid using inductive[^inductive]) reasoning draws general conclusions from observations.  E.g. certain software functionality will work because we tested it.
Thinking about software testing as an empirical process allows us to consider things like context bias (did we miss a range of scenarios?), observation sample size (did we test enough?), correlation vs causality (e2e tests are unstable, why?), if observations are balanced (adequate coverage of test scenarios across functional areas)... 
Here is a fun extreme example of a correlation vs causality problem: "Each time I test the app it works just fine, yet users
keep reporting issues. What's wrong with these people?". 
But is it even more fun to think about a full blown empirical process, to think about both experiments and observations. 

Deductive reasoning goes from general knowledge to specific conclusions. E.g. certain software functionality will work because of type safety or because it is a straightforward application of something else we believe works. In this post, deductive represents a wide range of thought processes: from tedious observationless mental verification 
of values and types (oh, this value was supposed to be a positive number why is it negative?, why this function was never called?, is the new refactored code equivalent to the previous?...) all the way to formal reasoning.  

Empirical and deductive work in tandem and both are essential.  IMO, we (as individuals) prefer to use one more than the other. 
Understanding more about these preferences will be the main topic of this post. 

[^inductive]: This inductive reasoning should be confused with mathematical (or structural in PLT) induction. Believe it or not it sometimes is. 

_side_note_start
Formal reasoning[^formalprog] is the only approach humans have figured out to solve complex problems correctly on the first go (without trial and error associated with empirical reasoning).  In this post, I am not separating the formal out, it is bundled into deductive. 
Isolating formal reasoning process could be relevant to this post. 
I have struggled on this decision, at the end I decided to simplify things and keep formal reasoning bundled into the more broad deductive process.
_side_note_end

[^formalprog]: Examples of formal approaches popular among FP-ers could be equational reasoning, use of logical implication (e.g. with Haskell type class constraints), use of mathematical or structural induction (e.g. with inductive types like a functional list prove properties of, say, the implementation of `map`). Readers familiar with equational reasoning may agree with me about its similarity to a refactoring process where developer mentally verifies that the new code is equivalent to the old. The line between formal and informal is sometimes thin. 


## Coding by experimenting and observing

Consider these tools and processes:  debuggers (_observe_ execution of statements), loggers (record _observed_ behavior), testing (_observe_ app behavior), TDD (pre-define _observation_ outcomes for the code), design patterns[^patterns] (generalize code _observed_ to work well).   
I encourage you to think about tools and processes that are targeting deductive process, there are some!

[^patterns]: Design patterns are an interesting bunch because they include some deductive work.  E.g. factory decouples idiosyncratic aspects of object construction and decoupling is known to be beneficial. 
However, this is not that much different from, say, a wildlife biologists generalizing observed behavior of individuals to a whole species using a known symbiotic relationship in their argument.  
Hard to resist: They say symbiotic, we say decoupling... ;)   

Trial and error is how a lot of programming is done.  We write some code (_experiment_) and then _observe_ the result using tests, a debugger, looking at the logs, or simply observe how the app behaves ignoring other diligence.  This is, basically, an empirical process. 
Personally, I am a little afraid of experimenting with certain types of code and I try to think through all scenarios, I could miss something but my mental process includes a lot of deductive effort. I have worked with many programmers who seem to operate in a similar way.  However, I still need to run the code to _observe_ it working, make changes if needed, and rinse and repeat. Despite some deductive elements[^emperical-deductive], this approach is still very experimental in its nature.  

[^emperical-deductive]: There is an old "joke" term used in regards to clinical trial design: "triple blinded study" defined as TODO.  Empirical process doe not imply no deductive. 

How about working in code that is a complete mess? 
We make experimental changes and then test / debug / trace the heck out of it[^heck], right? 
We may try to reason about what solution is likely to work, but that is not much different from experimental alchemists trying not to blow themselves up. I can recall several projects where all my understanding was derived from debugging or tracing. 

[^heck]: Notice, I did not say "debug the crap out of it" because that could imply that we are making an improvement by removing it. 

Let's talk process bureaucracy.  Working in empirical world means procedural protocols.  Scrum is a procedural protocol: 
consider continuous improvement process with retrospectives or team velocity calculation.  These are all very empirical. 
In contrast, the deductive needs a cushy couch. Deductive and bureaucracy, IMO, do not mix well. Waterfall was a failed idea of applying a deductive approach to project management.  There is a lot of real word complexity in project management and empirical is needed (see also [Defined Process Vs Empirical Process](https://premieragile.com/defined-vs-empirical-process/)). 

Let's talk context,  is often the hardest and the most important part in empirical reasoning.  E.g. in pharmacology: drug response depends on many factors like drug interactions, dosage, population stratification, other medical conditions...  In biology:  animal behavior depends on many factors like activity factors, stress factors, location factors...  Programmers are exposed to context bias too.  When troubleshooting a bug the context is the exact scenarios causing it.  Performance issue could be caused by many factors, an application could be slow only in certain scenarios or only in certain environment configurations.   Being a good programmer could mean avoiding context bias.
The related _correlation vs causality_ problem is something programmers face as well
(e.g. the underlying cause of an issue is more subtle then what the observations suggest).   

I had a fun discussion with my wife, she is a statistician and data scientist working on pharmacological studies.  We were discussing if a design of a clinical trial could be adjusted to software testing. 
There are some intriguing similarities. Obviously, humans are much more complex than programs and different approaches will work better. A lot of this will be well understood by experts in some areas of programming (e.g. benchmark testing, UX testing), areas I may not know enough about. But it was a fun discussion and one that convinced me even more about the empirical nature of programming. 

In this analogy programmers are a crazy bunch of experimental scientists. In other words, to be good in programming 
you need to be good in empirical reasoning. Am I convincing you that programming is often an empirical process? 


### Experimental process and high extraneous loads

As discussed in [cognitive loads in programming](),  messy code has _high extraneous load_. 
If you are not familiar with this term simply substitute _high extraneous load_ with messy code. 
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

This jig-saw puzzle approach results in very clean programs, is addictive and a lot of fun, but is not commonly used at large.    

[^bruteforce]: Not a brute force IO. 

[^idris]: TODO


### Empirical FP

Why did we settle on using empirical method in programming?  IMO, there are 2 reasons: nondeterminism and code complexity.
FP is definitely about controlling the first, it also can improve [the second](TODO).

Let's focus on 2 cornerstones of scientific experiments: reproducibility and properly randomized and sufficiently large samples and restrict out observations to automated testing. 
FP-ers will immediately notice two related concepts: _referential transparency_ and _property testing_ (e.g. QuickCheck). Haskell is sometimes called _the best imperative PL_[^best_imperative].  I propose that _FP is the best empirical programming method_.  Yeah, could I be a little biased, just a little.

[^best_imperative]: "Haskell is the world’s finest imperative programming language" famous quote, probably originated in this paper [Simon Peyton Jones on Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)

FP is considered very theoretical for a least 2 reasons: one is that FP considers formalism to be important (E.g. operational semantics, lambda calculus as intermediate language, etc), second is in how it is being presented[^practical] (theorists found their niche in FP and are running with it).   

[^practical]: TODO link 


CS and programming are often associated with mathematics and logic but I argue that programming is more empirical than deductive by design. FP is a very much an outlier[^reynolds].   

[^reynolds]: I just love the beginning story in [Types, Abstraction and Parametric Polymorphism (John C. Reynolds)](https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf) paper. 


## Pragmatists and theorists

I am using terms theorist and pragmatist somewhat colloquially, but the meaning is close to how the terms are used elsewhere.  

If you listen to a functional programmer talk, you are likely to hear these terms: "reasoning about code", "principled computation", "computation laws", "correctness by design".  These people are likely to study things like lambda calculi, operational semantics, category theory, type theory...  All these things come with formal proofs and will result in a very specific mental training.  _To this group programming is more of a deductive process._  

These things do not resonate with vast majority of programmers who have a more pragmatic mindset.  You are more likely to hear these terms:  "testing", "TDD", "hacking" (meant as a compliment), even "design patterns" (though rarely these days).  _To this group programming is more an empirical process._[^pragmatists]   

[^pragmatists]: The association of pragmatism and empirical process is not unique to programmers, e.g. if you google "pragmatists vs formalists" today you will probably get a link to this quote "Formalism follows deductive approach whereas pragmatism applies empirical approach" from this legal philosophy paper: [Formalism vs. Pragmatism in Legal Philosophy](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3423036)

IMO, mindset and interest are very related terms. In this post I could almost use them interchangeably. IMO, it is all about interests rather then capabilities. 

A theorist primary interest is in the deductive reasoning. This implies an interest in the deductive itself. Deductive reasoning is often called top-down reasoning (general knowledge comes first). Formal reasoning (the pinnacle of deductive) has a very strong attraction for some individuals while being a lot of stress and frustration for others. 
Some programmers dive deep into FP and learn formal reasoning.  Sometimes a mathematician (this was the case with me) makes a career conversion and becomes a developer. Interest in the deductive is rather rare. 

In contrast, pragmatist primary interest is in accomplishing tasks at hand. Pragmatists will experiment and go empirical to get there. “When the going gets tough, the tough get empirical.”  Real world complexity is rarely fully amendable to formal reasoning, unfortunately "rarely" is often interpreted by pragmatists as "never" and the "fully" quantifier is ignored. 


Pragmatist will say "it works because we tested it".  Theorists will say "it is correct by design" (or closer to Dikstra: "we tested it and we know it does not work"). Pragmatist will say "this abstraction is too complicated to use because is too theoretical".  Theorists will say "this abstraction is too complicated because it lacks theoretical backing".  The 2 groups may find it difficult to communicate "this is nice" and "this is terrible" have a very different context when spoken by pragmatist and theorist.  

Pragmatist want the tools to be mainstream and popular to pass their test of practical usefulness.
Pragmatist will select tools that provide good ability to observe how the code runs.  To a pragmatist reasoning about code often means studying its execution flow. 
Some pragmatists go a step further and will only select a PL that allows them the best control over execution (e.g. 
_JS_ is used by browsers so this is what they will want to write). This could be caused by ([justified](TODO)) distrust for abstractions. 

In contrast, theorists want tools that support, rather than inhibit, deductive reasoning.  They typically want to reason on a higher level than the execution flow.  Many will view programs and execution as decoupled concerns[^fussion]. 
This is where the _imperative vs denotative_ discussion comes into play as well. 

[^fussion]: E.g. rewrite rules in Haskell. 

One bizarre difference I have noticed between heavy deductive thinkers and empirical mindsets is their favorite approach 
to learning.  When learning, some theorists may want to finish a whole book before writing a single line of code. The top-down thinking sometimes extends to top-down learning.  Me and my wife are in the first group. 
I had to force myself to write some code when going through [TAPL](), I decided to create a public github repo to give myself incentive to "code as I learn" with [CTFP]() and [T(ype)DD in Idris]().  Obviously, theorists can learn to like some bottom-up learning (and benefit from it).  Some theorists treat learning as a murder mystery and want to learn ASAP who done it, pragmatists know it will be the butler.  

On the flipped side, pragmatists prefer a hands on learning from code examples (ideally, associated with project work) and typically expects immediate return on their learning investments. E.g. a pragmatist is unlikely to spend several months studying, say, a one line of code[^one-line].  Pragmatists are less likely to search for deep understanding (this is kinda definitional, having theoretical interests makes you a theorist).  
Also, it seems logical to assume that learning from experience is more habituating. Pragmatists may have harder time making a mental shifts to how they work with code.  I have linked this Dijkstra quote ([_add_blank_target Dijkstra letter to The University of Texas](https://chrisdone.com/posts/dijkstra-haskell-java/)) in my previous post, but is seems so relevant that I will repeat it:  

> &emsp; "It is not only the violin that shapes the violinist, we are all shaped by the tools we train ourselves to use, and in this respect 
programming languages have a devious influence: they shape our thinking habits."

Unlearning is hard on all of us, I believe it is harder on pragmatists. 

To put some of these thoughts in the context of the previous post:

>  &emsp;  _Theorist typically prefer germane, pragmatists are more at home with extraneous cognitive load_   
>  &emsp;  _Theorist typically prefer simple and hard, pragmatists prefer easy and will accept complex_ 


[^one-line]:  I dig a deep hole by using [Free Monad](TODO) as example of such a line in my previous post. 

The part of FP that has been the most disappointing for me is the typically low quality of error outputs.  This may have to do with all falsehoods being equivalent one to another in mathematics[^falsehood].  However, we should not criticize the theory, rather the theorists for selecting abstractions that suppress or confuse error information.  I wrote about it in the previous post and also [before](TODO). 

In contrast, to a pragmatist error output is an (important) observation.  It is a pragmatic thing to do to know what went wrong.  

[^falsehood]: I am not a logician, I vaguely remember some versions of logic that allow multiple moralities, in particular one where there was _lax_ in addition to _false_.  Few (even) mathematicians will know these. 

This post argues that both traits are important.  I need to emphasize that we all have both characteristics but one tends to be dominant.  We will dig deeper into both mindset by analyzing some examples. 


## Conversations

Perhaps not surprisingly, theorists are not all equally disappointed about logical defects.
Explaining this diversity is a price I have to pay for bundling all theorists together. 
Think physics, it is very theoretical, yet theoretical physicists are happy to do crazy unsound things before they pat themselves in the back. In contrast, there is no such thing as "a partially correct" in mathematics.  IMO, this stricter view plays a role in how some of us approach programming. 


Alice: "We have a concurrency issue in our code"   
Bob: "Are you talking about a production issue, a failing test, or is it purely theoretical?"   
If Alice gets a change to explain the race condition, she may hear this response:   
Carol: "We done it like this before and everything was fine". 
  
Alice (nick name Negative Nancy, a theorist) considers all logical defects to be a disappointment.  Bob and Carol are pragmatists. 
The conversation could be in an impasse, it could be hard to reset it. 
It seems like a good idea for Bob and Carol to understand a little bit about how Alice approaches programming, and vice versa. 
Let's analyze this dialog a little bit.  

To Alice a logical issues are kinda a big deal. She will consider it very hard to reason about code sprinkled with logical flaws. 
There is actually a good reason for this. 
Some logical flaws we examined in my previous post [TODO](TODO) are quite isolated but it is hard (if not impossible) to understand the full impact of many of them.  I am little bit like Alice, working on a complex imperative program or a poorly written functional code results in me forming a large mental repository of issues and their unclear impacts. Maintaining it is a tough mental effort.    

Here is a story from my personal experience.  A few year back I done a code review session with two (very capable) developers.  I showed them one of my "bug stashes" in the project we all were contributing to. It had to do with logically brittle use of inheritance.  I demonstrated a process I go through 
to verify the brittle bits.  This session was very productive, we all learned something from it, and this code was refactored later. 
Their response is something I still contemplate:  "We do not go through such steps, we just assume it will work". 
For me it was a learning experience I still think about, I realized that developers non only create code differently, but also very differently consume it. 

Returning to Bob, he is a pragmatists.  Notice that Bob has stratified all contexts he assumed relevant to Alice's finding: production issue, failing test, and theoretical. To Bob, a logical issue in code are just a part of life.  He will not lose much sleep over it.  "It has bugs, it is called software."  This empirical mindset, in some ways, is healthier[^rnt]. 
However, proper empirical reasoning should not dismiss theoretical concerns.  It is hard to assess impact of some bugs (e.g. race conditions) using testing.

[^rnt]: I have discussed RNT (..) in my previous post. 

Carol response suggest an empirical mindset as well. Carol has generalized previous observations of a working product and that generalization overrides Alice's warning bells. Going from specific to general is what empirical process is about. However, proper empirical reasoning will question, even invalidate, previous "hypothesis" if new evidence provides reasons for doing so. 

Bob: "We are starting new frontend project, I propose we keep using XYZ PL, but maybe we could add a new library to our setup?"   
Alice: "XYZ is fundamentally broken, we should move to something sound, like Reason."   
Carol: "Alice, we do not know that PL, this will put the project at risk!"   
Alice: "We know so many problems about XYZ, XYZ puts the project in jeopardy too".   

You may be wondering why I call JavaScript XYZ?  Well, XYZ was intended as a placeholder. 
Alice would like to use tools that help, not inhibit her deductive process and are logically sound.
Alice has witnessed her colleagues (and probably herself) trip over XYZ unsound design numerous times. She considers 
use of XYZ akin to building a house on a broken foundation[^foundation].  
Bob and Carol insist on tools that have good IDE support, good debugging, and are familiar even if logically unsound. 
Pragmatists want to reason about execution flow, browsers use _JS_, thus, they may want a language very close to _JS_. 
Carol has a very valid point too, one that Alice may have hard time accepting.  I admit, I sympathize with Alice, even if Carol is probably right here. 

[^foundation]: TODO list to the footnote

I have noticed some pragmatists (not all!) arrive at conclusions too soon.  E.g. “ABC PL is no good, each time I try it I get (this) negative experience”, "Our issues are caused by the technology we use", etc.  This is often confounding correlation with causality and is bad empirical reasoning. These statements can be valid if there is more than the observations to support them.

I invite you to think more about the differences between pragmatists and theorists in both creating
and consuming the code.  I invite you to think more about how each side thinks.  

I need to emphasize, this is not a binary separation where everyone is are either pragmatist or theorist. We all have both traits, just one is more dominant then the other. 


## Negativity

There is quite a bit of negativity around us, programming community is not exempt from it. 
I will wrap up with some loose thoughts about negativity in the context of deductive and empirical mindsets. 

I fully expect this post to go into negative territory in some discussion boards, I hope it will be well received by some readers too, but the negative is given. Blogging is putting a nontrivial effort into something that is almost guaranteed to receive a significant amount of negative response (which makes me question the sanity of it). 
I can't answer why this is happening.  I can observe that the negativity often comes with very little or no context. 
Engaging in internet discussion forums is like a Manhattan of communication skills, if you can make it there you will do really good in your project team. 

   
Venting frustration, IMO, it does not improve how either the "venter" or the "ventee" feel. It only fuels the negativity. 
IMO, the best way to fight negative emotions is, you guessed it, to employ the deductive.  Figuring out the underlying context that causes negativity can save
a conversation, simply engaging in that deductive search can protect you.  Using logic to confront emotions is a form of what psychologist call [cognitive restructuring](https://www.apa.org/topics/anger/control).

Criticism of bad code is bound to be unpleasant to its authors. Even worse, a lot of code can be criticized on purely logical grounds. This can be interpreted
as a critique of the author's competence and, I am sure, is a big source of negativity.  Let's look at this differently.
Look at some of the empirical sciences like medicine or neuroscience and see how frequently the knowledge gets adjusted if not invalidated[^medical].
IMO, programming is on a very localized and very accelerated path of the same process.
Is "bad code" phenomena partially related to the empirical nature of programming?  I think it is[^turning]. 

[^medical]: The main reason typically presented for this is: improved ability to make and measure observations. 
In programming, we just witness things going south in front of our eyes. 

[^turning]: In my last post I included a trivia about Turing original paper having several bugs and his adviser's work being bug free. This is, again, related
to the imperative vs denotative discussion. Here is the link to that footnote: [TODO](TODO). 

Theorists are likely to devote a significant effort into learning the things that theorists like. 
We can imagine how it feels (well I can since I lived it) to not be able to use what you worked hard to figure out.
Examples like [How to stop functional programming](https://brianmckenna.org/blog/howtostopfp) come to mind. 
This is something that can be changed on a small scale but there a (very) few places who care to do so. 
The job market for functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust top the weekend use stats based on stackoverflow surveys[^weekend].  There must be quite a few frustrated programmers out there.  I have been in that position and I know it is mentally hard. 
Deductive process plays important role and, IMO,  software industry should treat the "theorists" minority better.   

[^weekend]: Repeating some of what I wrote [here](2022-03-13-ts-types-part6.html#about-simplicity):  Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. Here is one link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  In [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/) Rust moved ahead of Haskell. 
The job ranking (based on the UK's [_add_blank_target IT Jobs Watch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do)) puts Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017. 
This ranking is possibly exaggerated too, lots of jobs list Haskell as good to have but will have you code in PHP.  This bias exist
in any language but is stronger for something like Haskell than say COBOL. 
    


## Post Summary 

This and the previous post ([Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html) have shared a high level rants that have changed my approach to programming. 
Both have looked at the programming from the outside. Both allowed to critique the programming process. Both forced thinking about the human factor. 

In this post we considered programming to be a mostly empirical process and we asked the question: are we good at this empirical thing?    
More importantly, we have looked at programmers as individuals interested in either the empirical or the deductive. Can you place yourself into one of these camps? Can you place your coworkers? This is not always easy, sometimes the placement will depend on the situation. 
I hope I have convinced you that understanding these mindsets is helpful, it provides extra context when interacting with others and a mirror.  This post presented my honest opinions, these are not scientific claims, just opinions.  Thank you for reading!   

## Unexplored

In this post we asked is programming can learn from empirical process. Putting this question on its head seems interesting. 
Empirical sciences (e.g. medical sciences, neuroscience, nutrition science...) knowledge gets adjusted if not invalidated from time to time. This is reminiscent of finding a bug in a program. Programming does it in a very accelerated fashion with an immediate feedback from testing and app use. Can software development explain some of what is happening in other sciences?

It is interesting to consider psychological cognitive biases and how they impact both groups. In particular _bandwagon effect_ (popular == good), _commitment bias_ (we done it like this before, it has been tried and tested), _omission neglect_ (things we do not know are not important), _bikeshedding_ (syntax vs semantics, etc).  
In particular, syntax vs semantics would be an interesting discussion.  Theorists tend to think about syntax as a bikeshed, pragmatists fill the gap and talk mostly about liners and formaters, clearly both aspects are important.  

Imperative vs denotative in the context of both mindsets.  IMO, this is not a clear cut (few things are), I consider myself very theoretical, yet I like parts of my code to be imperative.  

Impact of education on the development of either mindset. 

It appears that developers approach bugs differently. This is how my interests in figuring out different programmer mindsets has started. I think the approach to bugs has 3 axes: "It has bugs, it is called software" is the origin, testing is one axis, 
"correctness by design" abstractions and type safety is second, a mental repository of 
possible issues and their impacts is third.  Pragmatists are on the first axis, theorists on the second.  We need the name for the
third group, lets call them _perfectionists_. I came to FP on a correctness wagon, theory and improved coding efficiency are for me an added bonus. The question is how the _perfectionists_ fit into this picture.    

I am sure there is are many more interesting angles to explore here. 

