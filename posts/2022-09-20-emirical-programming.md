---
title: On Empirical Nature of Coding and Why We Can't Agree 
lastmodified: Sep 20, 2022
featured: true
summary:  Easy to implement is not the same as simple to understand. Programming wants to be empirical it needs to be more deductive. 
toc: true
tags: Haskell
---


## Pragmatists vs Alice

A term like formalist has a meaning when applied to a mathematician, philosopher, or law scholar.  I am using terms formalist and pragmatist very colloquially.  

Some programmers dive deep into FP and learn formal reasoning.  Sometimes a mathematician (this is the case with me) makes a career conversion and becomes a developer.  Formal reasoning can be a very strong attraction for some individuals or can be just a lot of stress and frustration.  Most software developers are very pragmatic and are not attracted to things like formal logic. 

If you listen to a functional programmer talk, you are likely to hear these terms: "reasoning about code", "principled computation", "computation laws", "correctness by design".  These people are likely to study things like lambda calculi, operational semantics, category theory, type theory...  All these things come with formal proofs and will result in a very specific mental training.  _To this group programming is more of a deductive process._  

These things do not resonate with vast majority of programmers who have more pragmatic mindset.  You are more likely to hear these terms:  "testing", "design patterns", "TDD", "hacking" (meant as a compliment).  They will study new frameworks, tooling and PLs (as long as they are mainstream and popular and pass their test of practical usefulness).  _To this group programming is more an empirical process._   

Pragmatist will say "it works because we tested it",  formalists (my name for the first group) will say "it is correct by design" (or closer to Dikstra "we tested it and it does not work").  
 
This diversity, however, is often not productive. There is a communication barrier between the 2 camps.  

Alice: "We have a logical flaw in our code, we have to change things" could solicit these replies:   
Bob: "are you talking about a production issue, a failing test, or is it purely theoretical?"   
Carol: "we done it like this before and everything was fine". 
  
Alice (nick name Negative Nancy) has a formalist mindset.  Bob and Carol are pragmatists. 
The conversation now is in an impasse, it will be very hard to reset it.   
Alice clearly has noticed something that Bob and Carol do not know about but the chance of sharing her finding is probably gone.  
Being shut down may not be as hard for Alice as having to work with code she now knows has logical flaws. 
It seems like a good idea for Bob and Carol to understand a little bit about how Alice approaches programming, and vice versa. 

To Alice a falsehood is kinda a big deal.
Mathematician and logician are trained to think about falsehoods very differently.  Arriving at a falsehood is "game over" in logic.
Formalist will consider logical correctness as something of a paramount importance, something that overrides other consideration[^formal]. E.g. Alice may have a hard time understanding that "it not a bug is a feature" is even a thing.   
TODO Many logical flaws we examined in the previous section are quite isolated (e.g. `equals` becomes symmetric if you avoid offending data) but it is hard to understand the full impact of some of them (e.g. the Hibernate issue).  A formalist is more likely to worry 
about these things.   
I am little bit like Alice, working on a complex imperative project or poorly written functional code results in me forming of a large mental repository of issues with unclear impacts. 
Maintaining it is tough.    

[^formal]:  This is actually definitional, formalism is typically defined as a strict adherence to some set of principles (here logic).  

Here is a story from my personal experience.  A few year back I done a code review session with two (very capable) developers.  I showed them one of my "bug stashes" in the project we all were contributing to. It had to do with logically unsafe use of inheritance.  I demonstrated a process I go through 
to verify the brittle bits.  This session was very productive, we all learned something from it, and this code was refactored later.  
Their response is something I still contemplate:  "We do not go through such steps, we just assume it will work".  
For me it was a learning experience, I realized that developers non only create code differently, but also differently consume it. 

I invite you to think more about the differences between deductive and empirical process of both creating
and consuming the code.  People typically think about logical deduction process as something akin to formal verification, a method used to verify that code is correct. Consider deductive processes that goes into troubleshooting as well.   
DODO I want to summarize by pointing out that formalists and pragmatists experience cognitive load very differently.  It is obvious that 
germane load impacts both groups differently, it is less obvious that both groups experience extraneous loads in a significantly different way too.  These loads tent to be much harsher on formalists.  
Finally, this is not a binary everyone is are either pragmatist or formalist. We all have both traits, just one is more dominant then the other.  

But there is more to it!  The very reason why programming is mostly empirical could have to do with with high extraneous loads.  The next section will try to explain. 


## Programming as empirical process

CS and programming are often associated with mathematics and logic but the reality today is that programming more empirical than deductive.  
Consider these tools and processes:  debuggers (_observe_ execution of statements), loggers (record _observed_ behavior), testing (_observe_ app behavior), TDD (predefine _observation_ outcomes for (_experimental_) code), design patterns[^patterns] (generalize code _observed_ to work well).   
I encourage you to think about tools and processes that are targeting deductive process, there are some!

[^patterns]: Design patterns are an interesting bunch because they include some deductive work.  E.g. factory decouples idiosyncratic aspects of object construction and decoupling is known to be beneficial. 
However, this is not that much different from, say, a wildlife biologist generalizing observed behavior of individuals to a whole species  using a known symbiotic relationship in the argument.  
Hard to resist: They say symbiotic, we say decoupling... ;)   

How do you make changes to a complex program? You try things and then see if it works, right? 
You are _experimenting_ with code.  
This is by no means restricted to imperative programming, I _experiment_ in Haskell a lot.     
I think we are in a position to put another 1 and 1 together:  

> &emsp; _The empirical nature of programming is a consequence of its high cognitive load and also one of its main causes_ 

Code created as a sequence of experiments is bound to be complex, the only way to work on a complex code is to experiment.  
This is a feedback loop. The only way to control this loop is to inject some deductive process into it.

This is why continuous refactoring is a good idea. 
This is why using REPL and a PL with powerful types and abstractions works so well.  
This is why contribution is needed form both pragmatists and formalists.


## Programming, like cheese, has holes

You spotted an intermittent malfunction in your code. Thank god, you see only one commit in recent history and you have a strong hunch something is wrong with that commit. Only some 50 code changes. 
The one that caused the issue is: `var1 == var2` changed to `var2 == var1`.  Would you be able to spot it? 
I call this type of issue a "gotcha".   
How about, your _finder_ function seems to be not finding stuff, only that sounds too far fetched, the function looks correct, so you just ignore this as a possible explanation.  The underlying issue is that sometimes `x =! x` and you have used equality check to find things. 

I can point to examples in standard libraries of popular mainstream PLs or popular frameworks[^gotchas1].
The issues come with no deprecation warning and, if documented, are considered a 'feature'.  

[^gotchas1]: A few examples: Non-symmetric equals comes with use of combinations of `java.sql.Date`, `java.util.Date`, `java.sql.Timestamp` these remain used as standard JDBC mappings for DB columns, the usage will show no deprecation warning.  `[] !== []` and
`[] != []` in JS (incidetly `[] == ""`), working in JS often feels like explosives engineering. 
I wrote a blog series about [TypeScript Types](/tags/TypeScript-Notes.html) and ended up presenting a lot of complexities and gotchas that probably surprise any TS developer.  
How do Hibernate users prevent this [concurrency issue](http://rpeszek.blogspot.com/2014/08/i-dont-like-hibernategrails-part-2.html)?  


An interesting line of thought is reconstruct the process that leads to figuring out a bug like this.  
   
Identifying that there is a problem is an _empirical_ process. Typically the unexpected behavior is _observed_ by a user or tester.  Two things can happen to address it: various exploratory
code changes are made to work around the issue (this is still _empirical_, it is an _experiment_),  or an effort is made to identify the root cause (an underlying logical flaw).  
The second approach means an effort to identify a proposition (or a property) that is violated. 
Does this smell formal to you?  
...It also helps immensely if the PL can syntactically express
that proposition,  even better if the troubled code uses parts of that syntax. 
Does that sounds more and more like functional programming?

Deductive process is a cognitive ability, so is the empirical process.  It appears to me that an ability to identify gotchas is related to possessing the first one.  


Inductive reasoning...
Just thinking about computations as something that can be understood changed my life as a programmer.

Thinking about computations as something that can be 
understood changed my life as a programmer.



Referential transparency is perfect scientific experiment,  
Clear, predictable inputs and outputs are what is needed in empirical process. 

Haskell is considered a great imperative language, FP is a great empirical method.

Coping with cognitive overload is different. 