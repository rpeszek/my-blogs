---
title:  Let's agree to be different. On empirical and deductive nature of coding. 
featured: true
summary:  Empirical and deductive mindsets compared.
changelog: <ul> 
     <li> (2022.11.12-20) Feedback from readers including <a href="#fn16">footnote [16]</a>
    and <a href="#fn37">footnote [37]</a>. Clarified parts as "IMO". 
     </li>
     <li> (2022.12.31) Added link to xena project and new footnote <a href="#fn5">footnote [5]</a> (this breaks previous external links to footnotes above 4). 
     </li>
     <li> (2024.06.08) Added section <a href="#side-note-formal-deduction-mathematics-and-immutability-of-knowledge">formal deduction, mathematics, and immutability of knowledge</a>. 
     </li>     
     </ul>
toc: true
tags: patterns-of-erroneous-code, communication
---

> *"When the going gets tough, the tough get empirical." Jon Carroll*   

> *"If in physics there's something you don't understand, you can always hide behind the uncharted depths of nature. You can always blame God. You didn't make it so complex yourself. But if your program doesn't work, there is no one to hide behind. You cannot hide behind an obstinate nature. If it doesn't work, you've messed up." Edsger W. Dijkstra*


I will discuss (on a very high level) empirical, experimental, and deductive aspects of programming. 
This distinction is fun to analyze (and a somewhat unusual way to look at programming) but it seems mostly a useless curiosity in itself.  I have convinced myself that programmers tend to favor empirical or favor deductive. IMO, we are placing ourselves into 2 camps.  I call these camps _pragmatists_ and _theorists_. 
This division impacts how we program and communicate. Discussion of both mindsets is the main goal of this post. 

The topic for this post came from a realization I had when thinking about cognitive loads ([_add_blank_target Cognitive Loads in Programming](2022-08-30-code-cognitiveload.html)). 
It may be obvious to some of you, but it was not obvious to me: programming is largely an empirical process. 
I will argue that the pragmatic empirical mindset is also dominant. 

What the others mean, how they reason, or what is important to them are the contexts in human communication. Communication without the basis of common interest is hard. Good communication requires an effort of understanding these contexts (we call it finding a common language).  IMO, _empirical, deductive, pragmatist,_ and _theorist_ are a good terminology choice to analyze some of the current discourse (especially about FP). 
In this post I will present my observations about the empirical and deductive mindsets. You may disagree with me, please let me know if you do. The point is to get these contexts right, or at least to have all of us think about them a little.  

Fairness and lack of bias are rare but beautiful if encountered in human interactions.  In my current work I mostly use Haskell and I have been interested in functional programming for a long time, I am a mathematician who became a software developer (27 years ago, but these things stay with you). Thus, this post is likely to have some unintended bias. Also, these are my opinions, not an attempt at a scientifically sound reasoning.  

I have not found much discussion about the empirical nature of programming, I am not following academic research in any related area. 
The topic of [_add_blank_target empirical software engineering](https://en.wikipedia.org/wiki/Empirical_software_engineering) is relevant to programming and the empirical method, but is not really what I will talk about. 
Retrospecting on my software programmer career,  I recall good and bad things. The good had good communication, the bad had bad communication of some sort.  Pragmatists vs theorists is just a part of a bigger puzzle, I am going to explore that part here. 

So what is the point I am trying to make? 
My only real point is that both mindsets are important, my goal is to 
discuss empirical and deductive (programming), theorists and pragmatists (programmers) in as much debt as I can muster. 

When reading it, keep in mind that my goal is to present different points of view developers are observed to have, I am not trying to make you agree with these viewpoints (that includes my side notes on formalism, mathematics, programming, and everything else in this post). The fact that we may disagree on these, is a corollary of the diversity I am trying to explore.  _This post is not about establishing consensus which viewpoint is better, it is about gaining some understanding of these viewpoints_.

## Empirical vs Deductive 

In my experience, developers do not use this terminology. Hey, in my experience, I do not use this terminology so a recap is in order. 

Scientists use these 6 (or more) terms: inductive, empirical, a posteriori, deductive, rational, and a priori. 
This sounds like the beginning of a catchy song.  There is also informal and formal but it breaks the rhythm, so I skip these for now. _Practical_ and _theoretical_ are great terms to describe a mindset, while _empirical_ and _deductive_ describe the thought process. 
In this post I will mostly try to use _empirical_ and _deductive_ when referring to the thought process, I will use _pragmatist_ and _theorist_ when referring to people. 

Empirical (to avoid using inductive[^inductive]) reasoning draws general conclusions from observations.  E.g. certain software functionality will work because we tested it (notice how well software testing fits into this definition).
Thinking about software testing as an empirical process allows us to consider things like bias[^biasdef] (e.g. the assumptions we make in unit tests), observation sample size (did we test enough?), correlation vs causality (e2e tests are unstable, is there something wrong with e2e testing itself?), if observations are balanced (adequate coverage of test scenarios across functional areas), establish observation baseline ([_add_blank_target golden testing](https://en.wikipedia.org/wiki/Characterization_test) for complex deterministic code)... 
Here is an exaggerated example of a correlation vs causation problem: "Each time I test the app it works just fine, yet users keep reporting issues. Something's wrong with these people!". 
But it is even more fun to think about both experiments and observations. 

[^biasdef]: I use the term bias loosely as well.  Scientists are concerned about bias in the design of empirical studies (which observations go in, how things are observed, etc), bias in how results are analyzed (e.g. proper stratification).  Bias is a [_add_blank_target statistics](https://en.wikipedia.org/wiki/Bias_(statistics)) term, [_add_blank_target psychology](https://en.wikipedia.org/wiki/List_of_cognitive_biases) term,  all of this seems very relevant to programming. 

Deductive reasoning goes from general knowledge to specific conclusions. E.g. certain software functionality will work because of type safety or because it is a straightforward application of something else we believe works. In this post, deductive represents a wide range of thought processes: from tedious "observation-less" mental verification 
of values and types (oh, this value was supposed to be a positive number, why is it negative?, is the new refactored code equivalent to the previous?...) all the way to formal reasoning (e.g. equational proof that refactored code is equivalent).  

In this essay, I use the term "formal" somewhat loosely. In particular, formalization of mathematics can mean something even much stricter than mathematical proofs, e.g. proofs that type check using a proof assistant.  In this post mathematics is viewed as an example of formal thought already, my goal is to contrast it with, say, biology.

Empirical and deductive work in tandem and both are essential.  IMO, we (as individuals) prefer to use one more than the other. 
Understanding more about these preferences will be the main topic of this post.


[^inductive]: This inductive reasoning should be confused with mathematical (or structural in PLT) induction. Believe it or not it sometimes is. 


_side_note_start
_I got quite a bit of pushback on this sidenote,  I will expand it in next section._
Formal reasoning[^formalprog] is the only approach humans have figured out to solve complex problems correctly[^formalprog2] on the first go (without trial and error associated with empirical reasoning). 
Mathematicians are not infallible, however mathematics is more formalized than any other science and, thus, is more "correct" than other sciences. 
Being a programmer, I like to think about formal reasoning as immutable and empirical reasoning as an in-place mutation (e.g. mathematics effectively keeps adding to itself while empirical sciences like medicine keep changing)[^immu].  
In-place mutation in programming is the refactoring process. Can you think about math-like immutable designs in programming?  E.g. a design or code that aged very well over, say, over the last 20 years?  
In this post I am not separating the formal out, it is bundled into deductive. 
I have struggled with this decision, at the end I decided to simplify things and keep formal reasoning bundled into the more broad deductive process.   

*Some people disagree strongly with the claim that formalism or mathematics are much less error prone than the empirical method.  I admit that I was surprised by this. My views about mathematical correctness are something I have developed from my experiences as a mathematician and are a part of who I am. However, both positions need to be noted.*
_side_note_end


[^formalprog]: The term _formalism_ has special meaning in mathematics, I use it colloquially (i.e. all mathematics is formal).
Examples of formal approaches popular among FP-ers could be equational reasoning, use of logical implication (e.g. with Haskell type class constraints), use of mathematical or structural induction (an exercise using it is included later in this post). Readers familiar with equational reasoning may agree with me about its similarity to a refactoring process where the developer mentally verifies that the new code is equivalent to the old. The line between formal and informal is sometimes thin. 

[^formalprog2]: This is kinda fun to think about: we have only empirical evidence of mathematics as a whole being _correct_, we know we will never prove it formally. Mistakes in mathematics are very rare (more on this in next section).  However, we have a lot of empirical evidence of (past) _incorrectness_ in various empirical sciences. 
I am a pragmatist enough to say that mathematics is correct (at least comparatively speaking), the rest looks good until we learn more about it (just like bugs in software). I believe a significant part of people trained in mathematics and formal reasoning share this viewpoint.  File it under IMO if you must.  


[^immu]: Obviously, a well established empirical knowledge will not change for ages as well. Immutability is a result of getting things right, empirical method converges towards it, formal method starts there. My metaphor "empirical is mutating in-place" is not perfect.  


You may think that I am spending too much time explaining the obvious:  any engineering will have a strong empirical aspect, engineers like to tinker with things.  I assume that some readers are like me and have not thought about it before.  The next section goes deeper into the empirical nature of coding.   


### Side note: formal deduction, mathematics, and immutability of knowledge

A logical thing to do here would be to discuss deductive systems in programming (e.g. operational semantics, equivalences between mathematical and programming concepts, etc). 
I decided against it, doing this would create a bias towards _theorists_ and would probably alienate some _pragmatists_.  I hope I am avoiding such bias by using mathematics as the example of formal thought. 

My comments about mathematics being mostly immutable and additive and very different from empirical sciences that keep mutating to get things right got a lot of pushback from the readers. 
This section expands on this topic and make some points, previously buried in footnotes, more visible. 
Here is an example of feedback from a reader:

> _"Lol what? Mathematicians are fallible and often find faults in decades old proofs."_
  
I agree with the quoted text in general, but I do not agree with the _often_ quantifier. 
We need to keep things in context.  We are comparing mathematics to other sciences.  I am not suggesting that published mathematical work is never wrong (e.g. the premises behind Homotopy Type Theory, Univalent Foundations [^hott], attempts to formalize maths with proof assistants [^xeno] are all thumbs up).
My argument is comparative, errors in mathematical proofs exist but are rare and exceptional. Comparatively speaking mathematics is a beacon of truth and an outlier.

Example: Why do we have infinitely many prime numbers? We are discussing formal deduction so let's do it!  

_Theorem_:  _Set of prime numbers is infinite._ 

_Proof_:  Let `S` denote the set of all primes. Assume it is finite.  Let `P` be the number obtained by multiplying all numbers in `S` PLUS `1`.   
By construction, `forall p ∈ S`,  `P mod p = 1` (`P` divided by any prime `p` has reminder of `1`).  
Thus, the prime factors of `P` are not in `S`.  Thus, we have a contradiction.  `S` has to be infinite.  _QED_

This is the proof I remember being taught in school. How old is the proof itself? 
I believe that nobody knows. We can find it in Euclid's Elements. So, it was known around 300 BC!
I hope this example gives you some evidence of the immutability of "formal thought". 
Yes, some areas of mathematics have changed around 1900, but a surprising amount of it has not changed for centuries. 

In contrast, Empirical sciences are in the midst of [_add_blank_target replication crisis](https://www.nature.com/articles/533452a) with reproducibility rates for published results below 50%[^repro].  

It is good to note that the above proof still does some hand waving. 
E.g. "Thus, the prime factors of `P` are not in `S`" implicitly assumes knowledge about prime factors and I am not referencing the "fundamental theorem of arithmetic": _every natural number greater than 1 is either a prime itself or can be factorized as a product of primes that is unique up to their order_. I left it ambiguous on purpose as such ambiguities are often present in math proofs. If `P` itself is prime the proof implicitly defines its prime factors as a one element set `{P}` which is somewhat not standard.  This all can be fully formalized but it would be tedious to do that and avoiding such tediosity is common in math. However, this looseness sometimes leads to overlooked errors. Again, such errors are rare. There is also the question of constructive proofs, this proof is not constructive.  There is a concept of "proof relevant" logic, traditional mathematics is not proof relevant (all proofs are treated as equivalent and are not themselves an object of study). I am not delving into these areas at all.  

Some mathematicians are trying to move towards formalizing such proofs. The new way is to write "informal" proofs for human consumption (like the one above) and accompany them with proof assistant type checked versions.  This change, again, is additive as opposed to mutating.  IMO, the above proof is likely to remain for another 2000 years. 

I believe proof assistants and type checking the proofs will be used much more in the future. This formula is a quote (I do not remember the source):

_lim<sub>t -> ∞</sub> Math(t) = CS_

I hope for this asymptotics: 

_lim<sub>t -> ∞</sub> CS(t) = Math_

and have a lot of thoughts about how this relates to LLMs and the future of human programmers.  These would take too much space and do not belong in this post. 

If you look at how most computer scientists approach programming and how most mathematicians approach writing proofs these areas remain very much divorced at the current moment.  Even people who know a lot of math and do a lot of programming (e.g. data scientists) are using programming to do math but not use math to program.

> _"Once upon a time, there was a university with a peculiar tenure policy.  All faculty were tenured, and could only be dismissed for moral turpitude. What was peculiar was the definition of moral turpitude: making a false statement in class. Needless to say, the university did not teach computer science. However, it had a renowned department of mathematics."_

_John C.Reynolds_ [_add_blank_target Types, Abstraction and Parametric Polymorphism (John C. Reynolds)](https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf) 1983

[^xeno]: This blog post: [_add_blank_target xena project, formalizing mathematics](https://xenaproject.wordpress.com/2021/01/21/formalising-mathematics-an-introduction/) is really worth reading as a whole. 
It points out that mathematicians do make errors too and argues about the need to add computer assisted formal verification to mathematics.   

[^hott]: See this lecture by Voevodsky himself about the motivation for [_add_blank_target Univalent Foundations: New Foundations of Mathematics](https://www.youtube.com/watch?v=E9RiR9AcXeE)

[^repro]: One line of defense presented in the quoted nature article is that experiment protocols are not being published. I find this line of reasoning very concerning: think about the concept of steady state. 

[^proof]: Since I mentioned formalization of mathematics, it is good to note that there is quite a bit of work that would need to be done to verify the above proof with a proof assistant.  E.g. "Thus, the prime factors of `P` are not in `S`" implicitly assumes knowledge about prime factors I am not even referencing the "fundamental theorem of arithmetic": every natural number greater than 1 is either a prime itself or can be factorized as a product of primes that is unique up to their order. Here I left it ambiguous on propose as it is often done in math proofs. If `P` itself is prime I think about its prime factors as a one element set `{P}` which is somewhat not standard.  This all can be fully formalized but it would be tedious to do that. Avoiding such tediousness sometimes leads to overlooked errors. 

## Coding by experimenting and observing

Consider these tools and processes:  debuggers (_observe_ execution of statements), loggers (record _observed_ behavior), testing (_observe_ app behavior and _draw general conclusions_ about app correctness), TDD (pre-define _observation_ outcomes for the code), design patterns[^patterns] (generalize code _observed_ to work well, create coding protocols). 
I encourage you to think about tools and processes that are targeting the deductive, there are some!

[^patterns]: Design patterns are an interesting bunch because they include some deductive work.  E.g. factory decouples idiosyncratic aspects of object construction and decoupling is known to be beneficial. 
However, this is not that much different from, say, a wildlife biologist generalizing observed behavior of individuals to a whole species using a known symbiotic relationship in their argument.  
Hard to resist: They say symbiotic, we say decoupling... ;)   

Trial and error is how a lot of programming is done.  We write some code (_experiment_) and then _observe_ the result using tests, a debugger, looking at the logs, or simply observe how the app behaves ignoring other diligence. 
Personally, I am a little afraid of experimenting with certain types of code and I try to think through all scenarios, I could miss something but my mental process includes a lot of deductive effort. I have worked with many programmers who  operate in a similar way.  However, I still need to test the code to _observe_ it working, make changes if needed, and rinse and repeat. Despite some deductive elements, this approach is still very experimental in its nature. 

How about working in code that is a complete mess? 
We make experimental changes and then test / debug / trace the heck out of it[^heck], right? 
We may try to reason about what solution is likely to work, but that is not much different from experimental alchemists trying not to blow themselves up. I can recall several projects where all my understanding was derived from debugging or tracing. 

[^heck]: Notice, I did not say "debug the crap out of it" because that could imply that we are making code improvements. 

Let's talk about process bureaucracy.  Working in an empirical world means procedural protocols.  Scrum is a procedural protocol: 
consider the continuous improvement process with retrospectives or team velocity calculation,  these are all very empirical. 
Test plans, test cases, coding and formatting standards, git hygiene, even design patterns are also procedural protocols.
In contrast, the deductive needs a cushy couch. Deductive and bureaucracy, IMO, do not mix well. Waterfall was a failed idea of applying an informal deductive approach to project management.  There is a lot of real world complexity in project management and empirical is needed (see also [_add_blank_target Defined Process Vs Empirical Process](https://premieragile.com/defined-vs-empirical-process/)). 
Waterfall reminds me of mocking in unit testing.  Both do not work well for similar reasons. 


Let's talk about bias in empirical reasoning. Figuring out contributing factors and causality is often the hardest and the most important part in empirical reasoning.  If your wrist hurts when you type, is this a pinched nerve in the wrist? or, are you looking down on your laptop and the nerve is pinched around your neck? or there is a pressure point somewhere in your arm? or maybe it is not a nerve issue at all?  Practitioners of empirical have been known to assume wrong cause[^bias]. Empirical is tricky. 

[^bias]: Rate of misdiagnosis for carpal tunnel is over 80% according to this article: [_add_blank_target carpal tunnel misdiagnosis](https://www.carpalrx.com/post/carpal-tunnel-misdiagnosis). A very sad example for the past: it is now believed that a big cause of death during Spanish Flu was too much aspirin prescribed to treat the symptoms. 

Programmers are exposed to bias too. Performance issues or a bug could be caused by many factors, e.g. an application could misbehave only in certain scenarios or only in certain environment configurations, the underlying issue could be in the application code, library code, a configuration issue, an environment problem... 
It is not unlikely for a developer to go down a wrong path during the troubleshooting process. I view programming as an empirical process with accelerated feedback. You can go down a wrong path but you typically learn that fast.

I had a fun discussion with my wife, she is a data scientist working on pharmacological studies.  We were discussing if a design of a clinical trial could be adjusted to do software benchmarks. 
There are some intriguing similarities. Obviously, humans are much more complex than programs and targeted approaches will work better in either domain.  But it was a fun discussion and one that convinced me even more about the empirical nature of software development. 


Why did we settle on using the empirical method in science?  We do empirical not because we want to but because we have to. Empirical is the only way to study the unruly real world. Why did we settle on using the empirical method in programming? 
 
## Why is programming empirical? 
 
I will focus on these 2 reasons: nondeterminism and code complexity.
Let's start with the second. 
An interesting question is how much the empirical nature of programming has to do with arbitrary (artificial) complexity.  IMO, a lot.  

### Experimental process and high extraneous loads

I am framing this in the context of my previous ([_add_blank_target cognitive loads in programming](2022-08-30-code-cognitiveload.html)) post. 
If you have not looked at that post, simply substitute _high extraneous load_ with messy code or complex code. 
I think we are in a position to put a 1 and 1 together:  

> &emsp; _The experimental nature of programming is a consequence of its high extraneous load and also one of its main causes_ 

The bigger extraneous load the more we experiment.  The more we experiment the bigger the extraneous load gets.  
To work on a complex code we are effectively forced to experiment.  Adding more experimental code only increases the complexity.  

This is a feedback loop.  To break this loop (and control the complexity) we need to rewrite or refactor parts of the code.  These involve some deductive process.  Some amount of deductive is essential, a deductive is what can break the feedback loop.  Deductive and empirical thought processes are closely related to cognitive loads discussed in my previous post and to controlling arbitrary complexity. 

There is one notable exception where piling up experiments does not result in extraneous complexity.
It is called type safety.


_side_note_start
**Side Note: Type safe experiments** are my favorite approach to programming. 
In the presence of a nice type system, coding can become solving jig-saw puzzles (writing code by using building blocks that can fit only in a correct way). 
A similar process can work even in TypeScript[^jigsaw]. It is typically enough to provide just enough safety to reduce the implementation solution space to prevent accidental incorrect implementations (this more relaxed approach ignores [Hyrum's Law](https://www.hyrumslaw.com/) but IMO still works well). 

This approach could include an interactive, type checker assisted deductive process (asking the compiler a series of type questions, something akin to type hole driven development[^idris]) or just making a guess and trying to see if the pieces fit. 
The second approach is a form of experimentation. 
Working with _hlint_ (Haskell linter, other static tools that do similar things exist) is lots of experimental fun too. 
I often need to get _a solution_ in and the linter will replace it with a much nicer fused code.  

_Combinator_ is a 100 years old term and I could be wrong about its intended intuitive meaning[^combinator]. To me it means a _building block_.  Functional programming is about designing and using well fitting combinators.

Jig-saw puzzle approach results in very clean programs, is addictive, and a lot of fun, but is not commonly used at large.  Importance of type safety is a position I hold but is also one that many programmers will disagree with.   
_side_note_end

[^jigsaw]: See [_add_blank_target A walk in the park](2022-03-13-ts-types-part6.html#a-walk-in-the-park), [_add_blank_target phantom types](2022-01-09-ts-types-part4.html#phantom-types), [_add_blank_target existential types](2022-01-09-ts-types-part4.html#preventing-information-escape) in my TS series.

[^combinator]: Schönfinkel (credited for the concept of combinatory logic) was Russian and worked with Hilbert in Germany.  His original work was in German. Curry worked with Hilbert as well.  The term has to do with building blocks (primitive "functions"), aka S K I in SKI calculus.  In LC, combinator is a lambda expression without free variables. Looks like a piece of a jigsaw puzzle to me. 

[^idris]: A nice presentation that illustrates interacting with the type checker to write code is: [Type-driven Development of Idris, Vect](https://youtu.be/DRq2NgeFcO0?t=356) on youtube.  Adding type level information about the size of list constricts the solution space and  creates a jigsaw puzzle. 


### Empirical FP
 
FP is considered theoretical for at least 2 reasons: _(1)_ FP considers formalism to be important, _(2)_ how it is being presented to the public[^practical]. But, is FP an empirical process as well?  

[^practical]: There are efforts to change that, e.g. see this reddit: [_add_blank_target Practical Haskell Bits](https://www.reddit.com/r/haskell/comments/xao5fy/announcing_practical_haskell_bits_initiative/).

Consider 2 cornerstones of scientific experiments: reproducibility and properly randomized sufficiently large observation samples, and restrict our attention to automated testing. 
FP-ers will immediately notice two related concepts: _referential transparency_ and _property testing_ (e.g. QuickCheck).  Clearly FP has a lot going on for it if you look at it from the empirical angle. 

Whatever your opinion about FP vs code complexity is, you have to agree that FP is about controlling nondeterminism. Functional programming tries to separate out the predictable deterministic part (pure functions) and limit (even stratify) the unruly effectful parts. Functional programming handling of nondeterminism is analogous to empirical study designs trying to control variance. I think that this is why FP seems so empirically-friendly.    

Haskell is sometimes called _the best imperative PL_[^best_imperative].  I propose that _FP could become the best empirical programming method_ (yeah, could I be a little biased). However, it is "could become" rather than "is".  Empirical process benefits from good observability, e.g. decent debuggers, stack traces, rich amount of error information...  This is an area where FP could improve in general (Haskell in particular)[^logger]. 

[^best_imperative]: "Haskell is the world’s finest imperative programming language" famous quote, probably originated in this paper [_add_blank_target Simon Peyton Jones, Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)

[^logger]: I implemented a proprietary Haskell logger library at my work. It is interesting to think about what we want to observe when we FP. Standard logger libraries for, say, Java are "object-centric" and will allow configuration options based on which class spilled out info into the log. The library I implemented is "data-centric" and allows you to configure what data you want to see. FP is about clear inputs and outputs after all. 

_side_note_start
**Side Note:**[^change1] (Haskell 101).  What could better explain a theorist mindset than to actually experience some of the formal thinking?   Here is an "elementary" (from ground up, using only basic language features) 
implementation of a popular Haskell combinator. 
Notice the implementation is just a bunch of equations
that use constructors or pattern match and nothing else:

```Haskell
partitionEithers :: [Either a b] -> ([a], [b]) 
partitionEithers [] = ([], [])
partitionEithers (Left a: es) = (a: ra, rb)
    where (ra, rb) = partitionEithers es
partitionEithers (Right b: es) = (ra, b: rb)
    where (ra, rb) = partitionEithers es
```

**Fun exercise 1**:  Identify the obvious conservation law for list lengths. Use QuickCheck or other property testing library to verify it. 
It suffices to test just one type, say `a ~ b  ~ Bool` (unit `()` will do too), can you provide a reason why? 
Can you try to formalize it?   
**Fun exercise 2**: Use paper and pencil to prove that this implementation satisfies that law. 
This exercise shows, formal reasoning does not need to be complex or advanced.   
**Fun exercise 3**: Change the above code to violate the conservation law.  How likely is for such implementation to be accidental?  Add [_add_blank_target Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/) annotations to prevent unlawful solutions.    
**Fun exercise 4**: In Haskell, any implementation of 
`partitionEithers` type is a [_add_blank_target natural transformation](https://bartoszmilewski.com/2015/04/07/natural-transformations/) in `a` and `b`.  Can this be used in exercise 1?   
**Fun exercise 5**: (1) and (4) rely on Haskell language property called parametricity.  This prevents Haskell programs from 
learning what the actual types behind `a` and `b` are or use values of these types in a concrete way (e.g. there are no globally available `==`, `toString` etc).  A number of mainstream languages have 
now the ability to express a type similar to `partitionEithers`.  Exploit lack of parametricity in your chosen language to create an implementation of `partitionEithers`
that violates the conservation law only for some types and works as expected for others (this makes randomized testing harder, why?). Show that your implementation is not a natural transformation. 

Hints and partial solutions[^hints].  It would be interesting to know how often thinking about properties 
and verification of properties is included in actual project work. 
_side_note_end

[^change1]: I rewrote the side note and changed the exercises type based on comments from [_add_blank_target u/kindaro](https://www.reddit.com/r/haskell/comments/yonk01/comment/ivk8pgr/?utm_source=share&utm_medium=web2x&context=3)
 on reddit. Thanks!


[^proofassist]:  Even with proof assistants, increased refactoring cost is a consideration.  But I do hope we will 
see more of Idris like PLs and proofs in real world projects in the future. 

[^hints]:  Exercise 2: You will need "elementary" implementation for `length` (`length [] = 0; length (x:xs) = 1 + length xs`). Recursion step becomes induction step, the conservation law is an equation, you prove it by writing bunch of equations you get from the program itself.    
Exercise 3: Liquid Haskell annotation just spells out the law: `{-@ partitionEithers :: xs:[Either a b] -> {ys: ([a], [b])  | (len xs) = (len (fst ys)) + (len (snd ys))} @-}`.  As of this writing, you can try it online [_add_blank_target here](http://goto.ucsd.edu:8090/index.html#?demo=lenMapReduce.hs).   
Here is a page with more information: [_add_blank_target partial solutions](https://github.com/rpeszek/experiments/tree/master/partition-eithers-excercise).


FP is a hybrid containing both empirical and formal. ... But I got sidetracked a bit towards areas of my interest.  You can classify this under IMO if you disagree with what I wrote.  
Let's finally get to my main topic: the human aspect. 

## Pragmatists and theorists

I am using terms _theorist_ and _pragmatist_ somewhat colloquially, but the meaning is close to how the terms are used elsewhere.  In this section I will try my best to describe both mindsets.  IMO, mindset and interest are very related terms. In this post I could almost use them interchangeably. 

If you listen to a functional programmer talk, you are likely to hear these terms: "reasoning about code", "principled computation", "computation laws", "correctness by design", "type safety".  These people are likely to study things like lambda calculi, operational semantics, category theory, type theory...  All these things come with formal proofs and could result in a very specific mental training.  _To this group programming is more of a deductive process._  

These things do not resonate with the vast majority of programmers who have a more pragmatic mindset.  You are more likely to hear these terms:  "testing", "TDD", "hacking" (meant as a compliment, "I made it work"), even "design patterns" (though rarely these days).  _To this group programming is more an empirical process._[^pragmatists]   

[^pragmatists]: The association of pragmatism and empirical process is not unique to programmers, e.g. if you google "pragmatists vs formalists" today you will probably get a link to this quote "Formalism follows deductive approach whereas pragmatism applies empirical approach" from this legal philosophy paper: [_add_blank_target Formalism vs. Pragmatism in Legal Philosophy](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3423036)


A theorist's primary interest is in deductive reasoning. This implies an interest in the deductive itself. Deductive reasoning is often called top-down reasoning (general knowledge comes first). Formal reasoning (the pinnacle of deductive) has a very strong attraction for some individuals while being very much disliked by others. 
Some programmers dive deep into FP and learn formal reasoning.  Sometimes a mathematician (this was the case with me) makes a career conversion and becomes a developer. Interest in the deductive and formal is rather rare, to many programmers formal methods are a foreign concept. 

In contrast, a pragmatist's primary interest is in accomplishing the tasks at hand. Pragmatists will experiment and go empirical to get there. “When the going gets tough, the tough get empirical.”  Real world complexity is rarely fully amenable to formal reasoning. Unfortunately pragmatists take a stronger stance on this, a pragmatist may think that formal reasoning is never useful.  

I need to emphasize that what I am presenting is an oversimplification.  People are complex and cannot be
easily labeled.  In particular, data scientists, statisticians, programmers interested in probabilistic computing models have a lot of formal training but probably will develop a more empirical attitude towards programming.
The attitude towards programming is shaped by many factors (e.g. work experiences, types of programming projects).
Theoretical vs pragmatic mindset is IMO a good first approximation. 

Pragmatists will say "it works because we tested it".  Theorists will say "it is correct by design" (or closer to Dijkstra: "we tested it and we know it does not work"[^dijkstra]). Pragmatists will say "this abstraction is too complicated to use because it is too theoretical".  Theorists will say "this abstraction is too complicated because it lacks theoretical backing".  The 2 groups may find it difficult to communicate "this is nice" and "this is terrible" have a very different context when spoken by a pragmatist and a theorist.  

[^dijkstra]: IMO, some experience with the formal is needed to better understand limitations of empirical. "Program testing can be used to show the presence of bugs, but never to show their absence!" is a projection of that understanding onto programming.

Pragmatists want the tools to be mainstream and popular to pass their test of practical usefulness.
Pragmatists will select tools that provide good ability to observe how the code runs.  To a pragmatist reasoning about code often means studying its execution flow. 
Some pragmatists go a step further and will only select a PL that allows them the best control over execution (e.g. 
_JS_ is used by browsers so this is what they will want to write). This could be caused by a (somewhat justified[^abstractions]) distrust for abstractions. 

[^abstractions]: See [_add_blank_target Extraneous nature of abstraction](2022-08-30-code-cognitiveload.html#extraneous-nature-of-abstraction).

In contrast, theorists want tools that support, rather than inhibit, deductive reasoning.  They typically want to reason on a higher level than the execution flow.  Many will view programs and execution as decoupled concerns[^fussion]. 
This is where the _imperative vs denotative_ discussion comes into play as well. 

[^fussion]: E.g. rewrite rules in Haskell. 

I consider programming to be a combination of both the engineering procedural processes and math-like science. 
Pragmatists are interested in the process, they want code standards, formatting standards, clean git history...  Theorists will want functors, monads, higher rank types, higher kinded types, dependent types...  Programming, obviously, benefits from both engineering and mathematics.  

What makes us effective and confident when working with code? 
For a pragmatist the source of confidence is likely to be test coverage, 
for a theorist it will be type safety, abstractions, lawfulness.  For both confidence implies some ability to understand the code (absorb the cognitive load). 
To put some of these thoughts in the context of the previous post:

>  &emsp;  _Theorist typically prefer germane, pragmatists are more at home with extraneous cognitive load_   
>  &emsp;  _Theorist typically prefer simple and hard, pragmatists prefer easy and will accept complex_ 


One bizarre difference I have noticed between heavy deductive thinkers and empirical mindsets is their favorite approach 
to learning.  When learning, some theorists will want to finish a section or chapter before writing a single line of code. The top-down thinking sometimes extends to top-down learning (a theorist wants to internalize the theory before applying it).  My wife and I are in this group. 
I had to force myself to write some code early when going through [_add_blank_target Types and Programming Languages](https://www.goodreads.com/book/show/112252.Types_and_Programming_Languages), I decided to create a public github repo to give myself incentive to code as I learn when reading [_add_blank_target Category Theory for Programmers](https://www.goodreads.com/book/show/33618151-category-theory-for-programmers) and [_add_blank_target T(ype)DD in Idris](https://www.manning.com/books/type-driven-development-with-idris).  Some theorists treat learning as a murder mystery and want to learn ASAP who done it, pragmatists know it will be the butler.  

On the flip side, pragmatists prefer hands-on learning from code examples (ideally, associated with project work) and typically expect immediate return on their learning investments. E.g. a pragmatist is unlikely to spend several months studying, say, a one line of code[^one-line].  Pragmatists are less likely to search for deep understanding (this is kinda definitional, having theoretical interests makes you a theorist).  
Also, it seems logical to assume that learning from experience is more habituating. Pragmatists may have a harder time making mental shifts to how they work with code.  _Unlearning_ is hard on all of us, I believe it is harder on pragmatists. 

[^one-line]:  I dig a deep hole by using [_add_blank_target Free Monad](2022-08-30-code-cognitiveload.html#germane-and-intrinsic-load-of-fp) as example of such a line in my previous post. 

The part of FP that has been the most disappointing for me is a typically low quality of error outputs.  This may have to do with all falsehoods being equivalent in mathematics[^falsehood].  However, we should not criticize the theory, rather the theorists for selecting abstractions that suppress or confuse error information (IMO).  I wrote about it in the previous post and also before[^before].    

[^before]: See my posts about maybe and alternative overuse [_add_blank_target patterns-of-erroneous-code](/tags/patterns-of-erroneous-code.html)

In contrast, to an experienced pragmatist error output is (typically) an important observation.  It is a pragmatic thing to do to know _what_ went wrong.  However, the tendency towards the use of `null`, `Option`, `Maybe` suppressing available information is something I do not understand. IMO, goes beyond the topics we are discussing here[^errorhandling]. 

[^errorhandling]: E.g. browsing a random website with developer tools console opened and observing all the red is, IMO, a signal of something different going on, e.g. things like _omission neglect_ (psychological concept loosely described by the phrase: out of sight out of mind) should be considered. 

While theorists may have a problem engaging with things outside of their theoretical model, pragmatists often have a problem engaging with things that are even mildly theoretical like computational laws
or even referential transparency.  Programmers often don't care about computational properties, but will be surprised by software behavior when they are missing.  Many gotchas can be described as 
a "natural" computation property that is violated.  

[^falsehood]: I am not a logician, I vaguely remember some versions of logic that allow multiple moralities, in particular one where there was _lax_ in addition to _false_.  Few (even) mathematicians will know these. 

Some theorists may not see a big difference between a prototype and a product that is fully implemented and maintained.  This could be related to _published equals done_ in academia and can be quite annoying. 


**Probably, the most interesting difference between both mindsets is:**

> &emsp; _A bug means an observed malfunction to empirical pragmatists_    
> &emsp; _A bug means a logical flaw to formal theorists_

In empirical science, rare is tricky. Rare can escape the empirical process. 
The term _outlier_, used in statistics, is relevant. The term "zebra" is used by medical doctors, "zebras" are hard to figure out, rare cases.  Empirical reasoning sometimes equates rare with impossible. 
When dealing with real world we say:

> &emsp; _"the exception that proves the rule"_   
> &emsp; In formal reasoning _exceptions disprove the rule._

In software engineering an exceptional, rare event often stops being rare (e.g. when the data or usage changes expose it). 
I fixed many bugs caused by a programmer's decision that a certain 
scenario is very unlikely and, thus, it is OK to cut corners. 
In my experience, ignoring rare scenarios saves hours and ends up costing weeks or even months later. 

However, I am not convinced that cutting corners is unique to either mindset.  A pragmatist may think "this is so unlikely, I will not waste time on it". 
The rare case could be not represented by the theoretical model that a theorist is considering (e.g. error information in a bunch of FP code). IMO, pragmatists and theorists cut corners in different ways, however, _cutting corners has no place in formal reasoning_ and I expect some programmers (most likely a subset of theorists) carefully think through rare cases. 

Theoretical and practical mindsets are antipodes of programming.  Even for people who "own" both, being a pragmatist or being a theorist is like wearing a hat.  You can't wear both at the same time, you would look ridiculous. 
I started making a conscious effort to understand which hat I have on. 

_side_note_start
**Side Notes.** Many things about programming seem to be on their head (making hats a somewhat tricky accessory).  Some ideas typically associated with FP are very pragmatic.  E.g. descriptive types, clear inputs and outputs, getting the same result on each try, ADTs (how could the ability to get a lot of generic code for free in Haskell be considered anything but pragmatic?)...  At the same time OOP is quite theoretical (taxonomic knowledge is fascinating academically, e.g. in biology or linguistics, but how practically important is it in programming?) and very complex (e.g. subtyping variance).  I dislike OOP primarily because it makes type checking less effective and I rely on type safety.

Another complex aspect is how _theoretical_ or _pragmatic_ you are.  If we classify a typical _Rust_ programmer as a _theorist_, where do we put someone using [_add_blank_target _ATS_](http://www.ats-lang.org/)?  If a typical _Haskell_ developer is a _theorist_, how do we classify someone working with _Agda_ or _Coq_...?

I am simplifying all of this and consider programmers to be either _pragmatists_ or _theorists_ 
and take an oversimplified (binary) and a somewhat stereotypical view of what these terms mean.
_side_note_end

This post argues that both traits are important.  We will dig deeper into both ways of thinking by analyzing some examples. 


## Conversations

So far, this post has tried to upset people on both sides equally. This section will be biased towards theorists, it is hard for me to present the pragmatist's viewpoint this way.  Engineers are some of the cleverest people on this planet. If someone
rewrote this section from an engineer/pragmatist point of view, I would love to read it. 

Perhaps not surprisingly, theorists are not all equally disappointed about logical software defects.
Explaining this diversity is a price I have to pay for bundling all theorists together. 
Think physics, it is very theoretical, yet theoretical physicists are happy to do hand waving arguments. In contrast, there is no hand waving in mathematics.  IMO, this stricter view plays a role in how some of us approach programming. 


Alice: "We have a concurrency issue in our code"   
Bob: "Are you talking about a production issue, a failing test, or is it purely theoretical?"   
If Alice gets a change to explain the race condition, she may hear this response:   
Carol: "We did it like this before and everything was fine". 
  
Alice (nick name Negative Nancy, a theorist) considers all logical defects to be a disappointment.  Bob and Carol are pragmatists and approach logical defects in a more relaxed way. 
It seems like a good idea for Bob and Carol to understand a little bit about how Alice approaches programming, and vice versa. 
Let's analyze this dialog a little bit.  

To Alice logical issues are kinda a big deal. She will consider it very hard to reason about code sprinkled with logical flaws. 
There is actually a good reason for this. 
Some logical flaws we examined in my previous post[^logicalflaws] are quite isolated but it is hard (if not impossible) to understand the full impact of many of them.  I am like Alice, working in a complex imperative program or a poorly written functional code results in me forming a large mental repository of issues and their unclear impacts. Maintaining it is a tough mental effort. 

Here is a story from my personal experience.  A few years back I did a code review session with two (very capable) developers.  I showed them one of my "bug stashes" in the project we all were contributing to. It had to do with a logically brittle use of inheritance.  I demonstrated the process I go through 
to verify the brittle bits.  This session was very productive, we all learned something from it, and this code was refactored later. 
Their response is something I still contemplate:  "We do not go through such steps, we just assume it will work". 
For me it was a learning experience I still think about, it made me realize how different our mindsets are. 

[^logicalflaws]: See [_add_blank_target Extraneous nature of abstraction](2022-08-30-code-cognitiveload.html#extraneous-nature-of-abstraction) and its footnotes. 

Returning to Bob, he is a pragmatist.  Notice that Bob has stratified all contexts he assumed relevant to Alice's finding: production issue, failing test, and theoretical. To Bob, a logical issue in code is just a part of life.   "It has bugs, it's called software."  This empirical mindset, in some ways, is healthier[^rnt]. 
It is not unusual for empirical reasoning to dismiss theoretical concerns, however in this case this is likely to be wrong. 
It is hard to spot or even assess the impact of some bugs (e.g. race conditions) using testing or other observation based methods[^tla]. The concurrency flaw Alice has identified can start manifesting itself all the sudden, this is what concurrency issues have been _observed_ &#128578; to do. 

[^rnt]: I have discussed RNT (repetitive negative thinking) in my [_add_blank_target previous post](2022-08-30-code-cognitiveload.html) in my previous post. 

[^tla]: Concurrency in particular is a good example, deductive tools like TLA+ exist. 

Carol's response suggests an empirical mindset as well. Carol has generalized previous observations of a working product and that generalization overrides Alice's warning bells. Going from specific to general is what empirical process is about. 
Proper empirical reasoning will question, even invalidate, previous "hypothesis" if new evidence provides reasons for doing so, but Alice is not providing any empirical evidence. 
Alice's argument is purely deductive, it could be helpful if she came up with a test that exposes the concurrency problem she has identified (this could be, obviously, very hard or even impossible to do).  

Bob: "We are starting the new frontend project, I propose we keep using XYZ PL, but maybe we could add a new library to our setup?"   
Alice: "XYZ is fundamentally broken, we should move to something sound, like Reason."   
Carol: "Alice, we do not know that PL, this will put the project at risk!"   
Alice: "We know so many problems about XYZ, XYZ puts the project in jeopardy too".   

You may be wondering why I call JavaScript XYZ?  Well &#128578;, XYZ was intended as a placeholder. 
Alice would like to use tools that help, not inhibit her deductive process and are logically sound.
Alice has witnessed her colleagues (and probably herself) trip over XYZ unsound design numerous times. She considers the
use of XYZ akin to building a house on a broken foundation.  
Bob and Carol insist on tools that have good IDE support, good debugging, and are familiar even if logically unsound. 
Pragmatists want to reason about execution flow, browsers use _JS_, thus, they may want a language very close to _JS_. 
Carol has a very valid point too, one that Alice may have hard time accepting.  I admit, I sympathize with Alice, even if Carol is probably right here. 


Alice: "You are memoizing a computation that is not referentially transparent"   
Carol:  "I have manually tested it and, besides, this code has a 100% test coverage"   
Bob:  "We are assuming that it is referentially transparent and want consistent results when we use it"   
Alice: "Remember we patched a bug by updating shared state in the middle of this computation, did you retest this scenario?"  

This code review session shows a benefit of having someone around who keeps a repository of potential issues in their head.
I have noticed that developers are typically surprised when computation behavior keeps changing, yet are mostly not willing to engage with the concept of referential transparency.  I also think some do not think about what 100% test coverage implies and what it does not[^limitations]. 

[^limitations]: It does seem that there is a more general disagreement about the limitations of empirical reasoning.  Few people think about physics as a collection of simplified mathematical models that only approximate reality. Even some famous physicists (e.g. Niels Bohr, if I remember correctly my reading about it) have apparently thought otherwise. 
Few people look deeply for bias in biological studies. _Hypotheses non fingo_ is a rare position.  100% test coverage is in the "we tested it and it is correct" category. 

Bob: "I changed the interface, you can now pass new parameters to control how the data is processed"   
Alice: "I changed the module, you can now use new functions (combinators) to manipulate the data"

Functions are great for reasoning about code, parameters are great for tweaking and experimenting.  
Alice’s deductive approach can really be beneficial when writing code, probably more than when troubleshooting empirically implemented code.  Let's get a little philosophical:

Alice: "Ideal code to me is one I would still be proud of after 10 years"  
Bob: "If you think about your 10 year old code as perfect, you learned nothing in these 10 years"  
Alice: "I am looking for something as timeless as mathematics"  
Carol: "Mathematics keeps improving and changing I am sure, everything does"  
Alice: "No, it only grows, it has not changed its mind in over 100 years"

This is almost an exact copy of a conversation I had with some of my coworkers. The immutability analogy I have used before works well here: mathematics is immutable while empirical sciences mutate in-place (some theorist hold this view). 
Bob's argument is partially valid as there is a lot of engineering going into coding and that is likely to keep changing[^eng]. Also, formal verification has a maintenance cost[^mathcost] making me question how realistic Alice's dream is. Can you think about code examples that aged very well?

[^eng]: E.g. consider performance improvements that can be made to my `partitionEithers`. 
Note, Haskell code that is implemented using constructors and pattern matching only
does not take advantage of rewrite rules that are already in place for combinators like `foldr`.  Compare my code to the source of [_add_blank_target `paritionEithers`](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Either.html#partitionEithers) in `Data.Either`.  

[^mathcost]: If you use a PL with a proof assistant feature and write proofs for your programs, a refactoring will have additional cost of rethinking the proofs. This could be not an issue with Liquid Haskell, which does the proofs for you, but still may require extra work if the logic solver needs extra help.  Consider refactoring my `partitionEithers` (annotated as before) to use `foldr` (which is incidentally what Base implementation uses), Liquid Haskell will tell you that your code is unsafe.  It is interesting to note that QuickCheck-like property tests maintain very well. 

_side_note_start
We consider PLs that reach a certain threshold of usage as immortal. 
A PL could be immortal but the ideas that went into its design may have died a long time ago.
Empirical needs an ability to mutate to improve. PLs are an example where, due to backward compatibility, changes are very hard to do.  Empirical + immutable + immortal is a bad combination.  This is why some (theorists of course) dislike mainstream PLs. 
Another example where immutability of formal thought is very, very useful is P2P (e.g. distributed blockchains). 
If distributing a code change is expensive or impossible, then the formal is needed. 
Inadequate amount of formal in these areas is simply unpragmatic and costly. 
IMO, "code that ages well" is an important topic.  
_side_note_end

These conversation examples were not intended to be exhaustive. 
I invite you to think more about the differences between pragmatists and theorists in both creating
and consuming the code.  I invite you to think more about how each side thinks.   
I need to emphasize, this is not a binary separation where everyone is either pragmatist or theorist. Many of us have both traits, just one is more dominant than the other and they tend to not manifest at the same time. 


_side_note_start
For the longest time, I could not figure out why certain decisions about PLs, popular libraries, or programming projects are being made.
I could not understand why certain bugs remain not fixed, why there are no deprecation attempts, why certain decisions have been made in the first place.  The empirical mindset I have tried to explain here is my best attempt at understanding these things.  E.g. I cannot explain in any other way why Java maintainers decided not to deprecate standard library classes where `equals` is not symmetric. The list of such issues is long[^why]. Are all of these "exceptions that prove the rule of solid design", rare and thus not important cases in the mindset of the maintainers?  I consider this approach to be not pragmatic and expensive.  If I was in charge of designing programming courses[^training], an example exercise would look like this:   

> &emsp;  *There is a common belief that TypeScript compilation flags like `strictNullChecks` prevent escaped `null` and `undefined`.   
&emsp;  Exploit how TS defines variance to create a function that has `number` as the return type but it returns `undefined` for some of its input parameter values.* 

The pragmatist's take (as I see it) is: (1) the impact of these booby traps is small and with some luck you will not notice (or _observe_) them in your project, 
(2) the ingredients you use do not need to be sound, they are just a part of a bigger implementation noise. I do not agree with these arguments, but at least I think I know what the arguments are, 
(3) counter examples do not come up much when using inductive reasoning (pragmatists are more likely to think about examples rather than general concepts and counter examples). 
_side_note_end

[^why0]: I created a long list of logically unsound decisions made by PLs and some mainstream libraries in this [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fn12)
in my previous post, I wrote a [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fnref19) about Haskell as well.

[^why]: I will partially repeat a list I came up with in my last post.  Example of non-symmetric equals is `java.sql.Timestamp` used with `java.sql.Date` or `java.util.Date`, these remain used as standard JDBC mappings for DB columns, the usage will show no deprecation warning. 
I wrote a blog series about [_add_blank_target TypeScript Types](/tags/TypeScript-Notes.html) and ended up presenting a lot of complexities and gotchas that probably surprise any TS developer.  
How do Hibernate users prevent this [_add_blank_target concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html)?  
I remember Grails as a land mine of issues, I wrote an 11 part [_add_blank_target blog series](http://rpeszek.blogspot.com/2014/10/i-dont-like-hibernategrails-part-11_31.html) about it back in 2014.  
Returning to Java, its implementation of `Optional` is often [_add_blank_target criticized](https://www.sitepoint.com/how-optional-breaks-the-monad-laws-and-why-it-matters/). Java array variance is fundamentally broken.
Java Streams are broken as well:  if you execute a stream twice the second attempt will fail. This (interesting take on referential transparency &#128578;) is bound to create interesting intermittent issues.   
I wrote a [_add_blank_target footnote](2022-08-30-code-cognitiveload.html#fnref19) in the previous post about Haskell ecosystem issues as well. 

[^training]: On the subject of training, how do you teach, say, equational reasoning using any of the mainstream PLs? Is equational reasoning something that a software engineer will never need?  I do use it. 


## Negativity

There is quite a bit of negativity around us, the programming community is not exempt from it. 
I will wrap up with some loose thoughts about negativity in the context of deductive and empirical mindsets. 

Venting frustration, IMO, does not improve how either the "venter" or the "ventee" feel. It only fuels the negativity. 
IMO, the best way to fight negative emotions is to employ the deductive.  Figuring out the underlying context that causes negativity can save
a conversation and simply engaging in that search can protect you.  Using logic to confront emotions is a form of what psychologists call [_add_blank_target cognitive restructuring](https://www.apa.org/topics/anger/control).

Criticism of bad code is bound to be unpleasant to its authors. Even worse, a lot of code can be criticized on purely logical grounds. This can be interpreted
as a critique of the author's competence and, I am sure, is a big source of negativity and tension.  Let's look at this differently.
Look at some of the empirical sciences like neuroscience and see how much of the knowledge got adjusted if not invalidated[^medical].
IMO, programming is on a very localized and very accelerated path of the same process.
Is "bad code" phenomena partially related to the empirical nature of programming?  I think it is[^turning]. 

[^medical]: The main reason typically presented for this is: improved ability to make and measure observations. In programming, we witness things going south in front of our eyes, we typically do not need to wait for a new testing technology. 

[^turning]: In my last post I included a trivia about Turing's original paper having several bugs and his adviser's (Alonso Church's) work being bug free. This is also related
to the imperative vs denotative discussion. Here is the link to that footnote: [_add_blank_target Turing](2022-08-30-code-cognitiveload.html#fn11). It should make us feel better about ourselves if Turing himself wrote bugs.

Theorists are likely to devote a significant effort into learning. 
How does it feel to not be allowed to use what you worked hard to figure out?
Examples like [_add_blank_target How to stop functional programming](https://brianmckenna.org/blog/howtostopfp) come to mind. 
This is something that can be changed on a small scale but there are only a few places who care to do so. 
There must be quite a few frustrated programmers out there[^weekend].  I have been in that position and I know it is mentally hard. 
I have argued that the deductive process plays an important role and, IMO, it is in the interest of the industry to treat the "theorists" minority better.   

[^weekend]: The job market for functional programming jobs is, frankly, dismal.  At the same time, languages like Haskell and Rust had topped the weekend use stats based on stackoverflow surveys. 
Repeating some of what I wrote [_add_blank_target here](2022-03-13-ts-types-part6.html#about-simplicity):  Haskell was firmly in the first position for the stackoverflow weekend use statistics for several years. Here is one link: [_add_blank_target 2017](https://stackoverflow.blog/2017/02/07/what-programming-languages-weekends/).  In [_add_blank_target 2019](https://stackoverflow.blog/2019/10/28/research-update-coding-on-the-weekends/) Rust moved ahead of Haskell. 
The job ranking (based on the UK's [_add_blank_target IT Jobs Watch](https://www.itjobswatch.co.uk/jobs/uk/haskell.do)) put Haskell at 932 as of 2022/02/06.  Haskell moved ahead of COBOL in that ranking in 2017. 
This ranking is possibly exaggerated too, lots of jobs list Haskell as good to have but will have you code in PHP.  This bias exist
for any language but is stronger for something like Haskell than say COBOL. 
    
I have promised at the beginning that I will not try to make too many "points".  This section contained the exceptions that prove the rule &#128578;. 


## Final thoughts
 
Did I sneak in any other side-"points" worth noticing?   IMO, this one (if you agree that formalism is less prone to errors):

> _Strict backward compatibility implies a need for formalism_       
or, equivalently   
> _Empirical, immortal, immutable, sanity:  pick 3_


The odd discourse between formal and empirical is not unique to programming. 
I still remember a few jokes about "a mathematician, a physicist, a chemist, ...". 
Empirical vs deductive, if one needs to budge then the empirical wins, mathematics has to move.
Logical correctness is often more a guideline rather than a shackle. 
Some (mostly functional programmers) argue that computer science and programming could benefit from a stricter application of formalism. 
The difference is where the source of truth is.  In a pure empirical world that source has to be what we observe. 
In programming there can be just enough determinism to benefit from treating logical soundness concerns more seriously. 
I believe, we need both empirical and deductive and I hope I made a convincing case for us to try harder for their peaceful coexistence. We can start by trying to understand the other viewpoint
even if we do not agree with it. 

Thank you for reading!   

 

## Unexplored

We can learn a thing or two about programming if we think about it as an empirical process. We can learn a
thing or two about the empirical process itself if we
examine programming as a case study. We did a little bit of both in this post. 
It seems that this synergy can be explored more. 

Impact of education on the development of either mindset.  

Related psychology, evolutionary biology: Humans survived and evolved by "observing" things and acting on these observations. Empirical process is in our nature. This also explains why we dismiss rare scenarios. 
I do not feel qualified to discuss these in more depth.  

As we have discussed, developers approach bugs differently. This is how my interest in figuring out different programmer mindsets has started. There is a different way to look at this. Consider these 3 axes: "It has bugs, it's called software" is the origin, testing is one axis, 
"correctness by design" abstractions and type safety is second, a mental repository of 
possible issues and their impacts is third.  Pragmatists are on the first axis, theorists on the second.  We need the name for the
third group, let's call them _perfectionists_. I came to FP on a correctness wagon, theory and improved coding efficiency are for me an added bonus. The question is how the _perfectionists_ fit into this picture.  This post bundled them with theorists, this was likely an oversimplification. 

Implicit contexts in communication between programmers. There appears to be much more to explore here. Consider programming internet discussion forums (IMO, a Manhattan of communication skills, if you can make it there you will do really well in your project team).  One can observe all kinds of context related bias issues (on the extreme end of this, some redditers do not consider reading to be a prerequisite to responding) or lack of context clarity (e.g. heavily downvoted posts with no comments). 
Moving away from discussion groups, teams tend to create their own localized contexts (unique vocabulary, proprietary technical solutions) which is often not ideal.  IMO, context clarity is to communication what referential transparency is to programming.  IMO, context is to communication what understanding of causation is to empirical science. 

Program synthesis, you know the thing that is going to render programmers obsolete:  It appears that empirical vs formal 
translates to 2 different approaches to do program synthesis.  I have listened to a few presentations about the formal approach.  I expect probabilistic models, deep learning approaches to be more mainstream and prevalent.  It is interesting
how this will play out, but I do not feel qualified to discuss this in much depth. _"The effort of using machines to mimic the human mind has always struck me as rather silly. I would rather use them to mimic something better."_ (Dijkstra, of course).  Quite possibly we will see an equivalent of empirical vs deductive play out in this area.

[_add_blank_target Abductive reasoning](https://en.wikipedia.org/wiki/Abductive_reasoning) and applicability of Ockham razor to software development seems like another fascinating philosophical topic that is orthogonal to what we have discussed[^abductive]. 

LLM's like ChatGPT cab be viewed as the ultimate "inductive learners".  Does this create opportunity for human minds capable of a more deductive thought process?  

I am sure there are many more interesting angles to explore here. 

[^abductive]: Abductive reasoning as a possibly relevant topic was pointed out by [_add_blank_target hellwolf](https://discourse.haskell.org/t/lets-agree-to-be-different-on-empirical-and-deductive-nature-of-coding/5276/7?u=rpeszek)