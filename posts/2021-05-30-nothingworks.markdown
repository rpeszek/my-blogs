---
title: Almost nothing works. cost of an average large scale software project
author: Robert Peszek

summary: todo
---



I like to think about the long term cost of a software project.  We typically call it the maintenance cost.  
What are: clean code, readable code, elegant code, good design, good documentation, best coding practices, agile process, test driven development?  These are all opinions.  What is the maintenance cost?  It is the factual verification of these options.   

It is really hard measure such cost and what contributes to it.  The data is typically not collected and, if it is, it would be only available inside
the organization.  I never had to put a JIRA ticket number on the timesheet.  (ref to bugs paper)

But, we kinda know what is eating our lunch.   

I am going to discuss several aspects of coding which result in a high maintenance.  I am going to present, what feels like a dystopian reality, in which 
high maintenance cost is unavoidable,  the only thing that works it taking away our ability of doing certain things. 

{ ---

Life is complicated.  What we observe can have many explanations and it is easy to focus on a wrong one.
I am not a big proponent of Occam's Rasor.  I still believe that if you observe many things and all have the same explanation the case of that explanation is stronger.  I am going to discuss several aspects of coding where past and current status quo has high maintenance cost.  That does not necessarily mean that cost was ignored.  There are other parts of the calculus that could explain these examples.  I will try to discuss these after each example and again at the end.  

I view maintenance cost as very important.  To me the maintenance cost quantifies the code quality.  
What are: clean code, readable code, well designed code, well documented code, elegant code, test driven development, best coding practices, best development process?  These are all opinions.
What is the maintenance cost?  It is the factual verification of these options.   

It would be a really interesting data science project to try to correlate certain code characteristics (like use of `null`) with the dollar cost.
Unfortunately, this would require access to data that is not in public domain and probably not even collected and would be non trivial even if the data was available.  I never had to put JIRA ticket number on a timesheet.  
The article will examine coding issues that are well known to cause maintenance problems
and try to understand what works and what does not in avoiding them.  Some examples will be drawn from my personal experience, some are so wildly spread that no personal story is needed to present them.

--- }


This article is a dump of my 25 years of experience in writing software.  It it touch on different technologies and languages.  
It will focus on languages I have used professionally in large scale projects:  -- Java, JavaScript, Groovy, Haskell. 
This post is not language specific, it is not about Haskell or FP, but it will use examples from both domains.
It is not about establishing superiority of FP over OO, is it just targeting the topic in the title.

You are likely do disagree with me.  You are welcome to do so.  My goal is to simply present my viewpoint and evidence and have you think about it.
I consider this topic scary.  The really scary would be any shallow attempts to fix the problem.  We (humans) are good at shallow and phenomenal at avoiding the real issues.  

## Who am I and how dare I

I started developing software professionally in late 90-ties. 
Before that I done research in mathematics. 
As software developer I worked for big tech, small startups, government agencies.  I have worked across many different business domains:  pharmaceutical, telecommunication, supply chain / inventory management, electric power, wildlife management, legal.  
I worked in product development teams and in R&D groups.   
I have used many mainstream programming languages (Java, JS, C#, Groovy, C++, ...) for over 20 years.  Finally, I ended up in Haskell (which I do not consider mainstream).  
I is -probably- clearly rare for a 50+ Java developer to become a professional Haskeller.  That should give me a viewpoint that is different with insights into both domains.  Whether you think of FP as a good think or a hype, this in itself may persuade you to continue reading.  

This post is not language specific, it is not about Haskell or FP, but it will use examples from both domains.
It is not about establishing superiority of FP over OO, is it just targeting the topic in the title.


## Case Study:  Separation of Concerns (UI / Business Logic Separation example)

Just for grins, let us agree that a code that mashes everything together is not the easiest to maintain. 
It is not hard to write: you just add a bit after a bit until to finish. 
But if you (or someone else, already feeling sorry for whoever that is) have to change it later...
There are many things that can be decoupled.  To keep this from being a book I will just focus on one:  separating user presentation form business logic. 


### UI from Bussiess Logic

shorten, make less client specific

I have to go historical on this one because the changes between 1990-ties and now can tell us something.
Late 90-ties were the fat client times.  Most of these applications mashed everything together, one line deciding on disabling / enabling a button, the next one was a DB select statement.  
The slogan was: RAD (Raid Application Development). Tools like Borland C++ Builder, PowerBuilder made writing fat clients easy.  They did not prevent developers from decoupling their code but done nothing to enforce it.  
Keeping UI separate is not a new concept.  MVC pattern is credited to smalltalk and dates back to 1970.  It was not a forgotten idea either. 
The information was available to anyone who cared but very few projects did.  
I have no way of quantifying this statement.  I can only offer stories from my personal experience.

In one of my first programming jobs (1998), we (...) managed to persuade the management to rewrite their legacy code using the new cool language called Java. 
The legacy code base had issues, was leaking memory (you know, you allocate it at the beginning of the function call and free it at the end, then someone 
modifies the function to return early ...).  
Java freed us from the memory leak issues.  But we really tried to do a good job beyond memory leaks. The code we were writing was quite elegant. In particular, we were very diligent about separating all kinds of concerns like presentation and business logic. This is a rare example where maintenance cost was considered to be an important part of the bottom line.
These were the times when I was completely infatuated with OO. 

That nirvana was in sharp contrast to the surroundings.  Our software was an outlier.  I remember implementing integration pieces for one of our clients.  That client used a very reputable consulting firm (which I will not name) to develop their website.  How do we integrate?  We were told that our best bet is to do HTTP calls. That website was built using a cutting edge technology called JSP (around 1999). 
It was a coding atrocity.  Files 1000+ lines long that created HTML, did database calls, and everything in-between.   

A similar experience (around 1999) was the _Great Plains_ ERP software (I believe now owned by Microsoft) integration.
I DUNNO how that looks now, but back then you interacted with it by programmatically interacting with its UI because there was no other way.  
Great Plains was a big software company back then.  I think I still have a certificate of attending their training in Fargo ND somewhere.
Big, successful have nothing to do with being well engineered. 

Most of the integrations fell into 2 categories:  going trough the UI or direct DB integration.

My last experience with fat client app was less than 10 years ago. It was a Borland C++ project I had to maintain as I was working on re-implementing it using browser based single-page app.  It was not any different. 

This has changed.  Today, most integration work is REST API calls,  some GraphQL.  I know of one example where the only possible thing to do was a screen-scap but what was the rule is now exception, what was exception is now the rule.

My claim is:

_The current separation of UI and business logic in the wild (when present) is forced the the back-end / front-end split in the technology stack_. 

Browsers with powerful UX capabilities and happened (buzzword "thin client") changing all of this. Technology stack has changed. The beauty of this is you can't very easily do DB calls from the browser.  UI and business logic got separated because technology forced that change. 
It did not have to be the browser.  Component architectures like CORBA were gained some popularity in very late 90-ties and early 2000s (precursors to todays microservices). Projects that used CORBA had similar benefits because you did not write UI code in COBRA.   

I am very much into P2P stuff.  I found this talk by _Brooklyn Zelenka_: https://www.youtube.com/watch?v=ulai-LGt0yU very interesting. 
The tile _A Universal Hostless Substrate: Full Stack Web Apps Without a Backend, and More!_ makes me think what will happen if developers are, again, allow lots of freedom.


_Pre Analysis_:   
Separation of Concerns (whatever the concerns are) requires discipline, lots of thinking, and is an up-front investment.
Some engineers will view it as needless complexity.  It is needless only for small, simple projects.
Decoupling, when done right, makes code more testable, easier to learn, easier to troubleshoot. 
You want your tea served separately from your stake.  

The project examples form the 90-ties were designed and programmed by capable engineers not afraid of learning new technology (JSP) or even investing in creation of a proprietary language (Great Plains).  


## My Hypothesis:  Technology limitation are the only effective way of lowering the cost 

My observation is:

_ average _

_High maintenance cost is the default state of a larger software project_   
_Technology stack limitations are effective in lowering that cost, on a large scale, nothing else is_

Enforcing good coding practices is like herding cats.  It may work well on few projects, it does not work at large.
(I will discuss reasons for this at the end).
The industry has put technology stacks on the pedestal to be worshiped.  We put these on our resumes, attend conferences
to learn about these.  But the real reason why this works is that technology stack limitation is impossible or hard to go around.   
An equivalent formulation:

_The Podocide Rule: We will shut ourselves in the foot unless it is hard for us to do so._


Software Development suffers from an acute case "temporal mico-economics":  Seemingly optimal short term choices are costly to the project in the long term.  
In my "Adam Smith"'s model of programming, the software developer takes the shortest / easiest / KISS path to accomplish the next incremental goal.  At the zoomed out level this is a maze and is far from shortest or simplest.   

This reminds me of how ants move.  An ant (not to be confused with the very old Java build tool) moves in straight short lines which look like a chaotic zig-zag when combined.  Unless, of course, we build a tiny ant tunnel _limiting_ their movements to only one direction.  

The other good analogy is Oregami dicection game.  ...
Unless, of course, we do _limit_ how the pieces can fit together.

That make sound like criticism of agile development.  I think this has much deeper roots than the process.   


__This is clearly very concerning.__ 
Ironically, it will get worse as technology improvements add flexibility and remove limitations.   
At a first, naive, glance, some may be tempted to limit developers creative freedom.   
This would be a very bad idea.  We do not want more waterfalls or other cures that will be even worse than the illness. 

Incidentally, Winston W. Royce did not propose Waterfall, he criticized the idea, "the redditers of that time" apparently did not read his paper, just skimmed it, missing the important bits.  
https://pragtob.wordpress.com/2012/03/02/why-waterfall-was-a-big-misunderstanding-from-the-beginning-reading-the-original-paper/


{ --
Some aspects of software development are restricted by the technology stack.  These include things like programming language, hardware and application architecture, protocols, frameworks, libraries, development environment.    
The software is constrained by its requirements and the technology stack, within that bound most project seek the lowest quality.
--}



## Exhibit 1: `null`

If there is one thing that everyone would agree has causes maintenance issue that would have to be the infamous `null`!   
The 'billion dollar mistake" quote dates 2009.  Language like ML new it is a bad idea in 1970 and stayed away from introducing it.  

I want to take a different view of the `null` problem.  What it suggests is:

> Gentlemen's agreements do not work at large

Developers were never forced to use nulls!  We could have just make 'a gentlemen's agreement' on using a more maintainable approach that avoids nulls. 
Why didn't we?

Any language that can express a higher order function can express `Option` / `Maybe`-like type. 
This is not an FP tutorial, my point is only to convince you that the `null` could have been easily avoided without any changes to your language.

The following pseudo code shows how this can be done _this is called Continuation Passing Style (Programming Lingo), Church Encoding (Lambda Calculus Lingo), special case of Yoneda (Category Theory Lingo), Haskellers will recognize `maybe` with lowercase "m"_)

``` 
option<T> = forall R . (R, T -> R) -> R
```

(uppercase chars represent types)

`Option` is simply a function of two parameters, one of them is a function.  This construction has been know at least since 1970-ties.
It can be replace `null` and does not need to use `null` in its implementation.

__Side Note:__  Compare this with, say, Java 8 implementation of `Optional` which is still is `null` based and full of booby traps.  It requires some "gentlemen agreements" to avoid the `NoSuchElementException`.  Is `NoSuchElementException` so much better than `NullPointerException`?   
I have not done Java for close to 3 years, let me try to implement CPS option:  

``` java
interface Option<T> {
    <R> R option(R empty, Function<T,R> some);
}
final class OptionSome<T> implements Option<T> {
    private T t;
    public OptionSome(T t) {
        this.t = t;
    }
    public <R> R option(R empty, Function<T,R> some) {
        return some.apply(t);
    }
}
final class OptionNone<T> implements Option<T> {
    public OptionNone() {
    }
    public <R> R option(R empty, Function<T,R> some) {
        return empty;
    }
}
```
I still got it!  I can write a one-liner in about 20 lines! 
Of course, you should change it by making constructors private and exposing static constructor methods
for `some` and `empty`, still will be (< 30 lines), then provide ways to check if it is empty or not (< 40 lines)...  
I think, this should work in any Java version >= 1.5 (2004), by adding a, new back then, `Function` class.  

You can convert _nullable_ data to it and back from it equally easy, here is the more interesting direction: 

``` java
class RecoverStupidity {
    static <T,R> T toNullable(Option<T> o) {
        return o.option(null, r -> r);
    }
}
```

Similarly, you can convert to and from the Java 8 `Optional`.

You may say this would be awkward to use. A weird construction like that!  It is is easy to convert back and forth from what you doing now, then it cannot be awkward.   
__End Side Note.__

Putting Java in the spotlight, here are some interesting questions: 

_How many (percentage) Java projects before Java 8 avoided using `null`?  
_How many projects avoid using `null` today?

My guess (to both questions) is: very, very few and my belief is that only a small percentage of today's projects take advantage of the somewhat better
Java 8 `Optional`.

_Pre Analysis:_ 

One can argue that software engineers do not know theoretical things like Lambda Calculus or Category Theory and the solution I have outlined would 
have been just to abstract and hard to understand for them.  I am completely not buying this argument.  
Do engineers think how sort function it is implemented?  We learned to code with black boxes.  This is just another black box that can expose very down to earth interface to novice users.

There is a ... bias against things theoretical.  TODO

'The billion dollar mistake' was just in providing programmers with ability to use `null` references, no one forced us them to use these.  
The only way to eradicate the issue is to use language that does not have `null`.  If `null` is available we will use it, ...
even when implementing the std library `Optional` class that is intended to get rid of it.   

Technology Stack that limits what programmers can do seem to be the only thing that works.  


### FP Option / Maybe types

This problem goes deeper.  `Option` like types are not a full solution to the `null` problem. 
`Option` is still problematic since it allows representation of _unspecified_ error conditions.
And I have seen this overused in the FP land, resulting in _unspecified errors_ that could have been specified.  
I wrote a whole post about it (...).

Lack of the error information, clearly, does not help in maintenance. 

_Pre Analysis:_  IMO there is a huge _sunny day scenario_ bias in software programming.  Sunny day is what ships the product out of the door.
We are focused on collecting _sunny day_ data and do not want to spend effort on collecting _rainy day_ data.  
`null` is used because it is easy to use and provides the same _sunny day_ output and most importantly delivers the first cut of the product slightly faster.
In languages like Haskell, `null` is not available so its nearest cousin gets used instead.



## Exhibit: Exceptions

This is the next logical stop after `null`.  I imagine nobody will argue that having meaningful errors is an important part of a maintainable software.

I will use Java as an example one more time.  It is interesting to think about what happened to Java checked exceptions.
They were not popular.  The critics argued that the force unnecessary boilerplate. Java forced developer to write code to handle certain exceptions
but there was often not much that the developer could do other than logging the exception or re-throwing as another one. 

The end result was that the community started avoiding checked exceptions.  C# dropped the concept from the start.

Using Haskell as example: I really wish that there were no untyped errors.  I dislike IOError too.   
Untyped errors in Utf8 decoding has resulted in many headaches in my current job.  The problem is that the concept of error (untyped) and exception (typically typed) in Haskell relies on a gentelmen's agreements like: "the term exception for expected but irregular situations at runtime and the term error for mistakes in the running program that can be resolved only by fixing the program"
"Errors can be prevented by (cheap) checks in advance, whereas exceptions can only be handled after a risky action was run"
(see https://wiki.haskell.org/Error_vs._Exception).  Many libraries violate such agreements, client programs lack diligence of confirming to these rules, and on top of all of this the rules themselves are not very consistent.

Incidentally, limiting the use of untyped exceptions is happening in Rust, and for a good reason.  It is very hard to provide type level guarantees about things if you can bypass the type system.  I can only hope that `panic` will not become easier to use and more popular.  

Why I do not like untyped errors?  Programming answer:  the concept of encapsulation is not a bad one if used correctly, information hiding is great if
the information can remain hidden.  You do not want a program you invoke to start boiling water and have you guess that you need to turn off the gas at the end.  Untyped exceptions do just that.   
Mathematical answer:  There is no place for untyped errors in Curry-Howard isomporphism, mathematics is gone.   


Incidentally, type level exceptions are making a comeback in Rust for a good reason.  It is very hard to provide type level guarantees about things
if you can bypass the type system.  Linear types do not like exceptions.  I can only hope that `panic` will not become easier to use and more popular.  


What is the lesson here? 

Java started in a good direction but offered only partial limitation (forcing handling of checked exceptions) with relatively easy work-around moving to unchecked `RuntimeException`.  Haskell makes it easy for users to define untyped errors asking that the users do not overuse them.   

Limitations with available workarounds do not work.  If unchecked exceptions were harder to use or limited in scope this might have worked.  Haskell makes it easy for users to define untyped errors asking that the users do not overuse them.  This has not worked 


Credit where is due:  Java stack traces. 

Java checked exception -> stop using it rather than improve it. 
C# removes these.  I missed them in Haskell (IOException).


## Code Reuse 

big ticket item.  Good to notice what works what does not.


## Exhibit 3: Mutating State vs Web Applications


Front-End - Back-end - DB

Struts. approching 200 instance variables.

vs
Front-End - Back-end (STM)



## Exhibit 4: Memory Bugs


This has been discussed a lot elsewhere, e.g.:
https://visualstudiomagazine.com/articles/2019/07/18/microsoft-eyes-rust.aspx
https://www.zdnet.com/article/chrome-70-of-all-security-bugs-are-memory-safety-issues/

This story has the bits that support TODO:

*  Best programming practices are not good enough
*  Partial solutions (like smart pointers in newer versions of C++) are not good enough
*  Limiting what developers can do works

Limiting what developer can do means:  no memory management (garbage collection handles it), 
no access to direct pointer programming, or type safety over memory and pointer use (linear types, Rust).


Again technology stack change.



## Exhibit 5:  Callback Hell

JS programs were famous for being not maintainable.  The apptly named _Callback Hell_ is one of the reasons for that reputation. 
Again, nobody twisted anyone's arm.  Continuation Style code dates back at least to 1950-ties.  
A more monadic style JS `promises` like constructions date back to 1994 in Haskell.  

The way forward was the technology stack change and introduction to ES6.


-- ## Exhibit 6:  Reactive Programming 

## The Configuration

### Hard-coding gets the job done

### yaml 



## Agile



## Final Analysis

I think maintenance cost (the lower the better) it the true measure of code quality.  
Not it is not the inverse of the WTF/minute rate during code reviews.
I imagine high WTF rate, depending on the audienceof course, to be actually proportional to the code quality.

### Industry Business Model

Maintenance cost is typically overshadowed by short term delivery goals.  
Time to market pressure tends to be the driving factor.  
Low defect rate and low maintenance cost are often not the high priorities.
For many projects, the maintenance cost is passed to the customers.  Such cost structure matches the process that emphasizes delivery timelines over quality.  

The fastest approach ends up being the least maintainable.  Hence the problem.

Big part of the issue is the corporate inertia.  Nobody gets fired for not changing the old process or for selecting 'proven' old tools, even if the old process and the old tools do not work well.  

For example, I know a whole industry segment which produces a lot of data outputs with high accountability and correctness requirements. Their standard practice is called "double programming" (2 indepedently programmed data output derivations need to agree) but, believe it or not, programmers do not use any version control!  Something like git is not a part of their technology stack.  Version control is not what any of these companies do and hence none feels any pressure to change.

### Explanations Why Coding Practices do not work

All examples.  Young work force,  perpetual lack of experience (Uncle Bob,  progammers double every 5 years.).
Education, especially mathematics (memorizing formulas not learn formal deduction methods).
Anti-theoretical attitudes (math has place in algorithms or data science but mathematical properties of computations ...)


### Group Bias against Theoretical 

Not unique to the industry, appears to be human nature (death of Socrates)

### False Prophecies and False Prophets

#### KISS

Process Simplicity not Simplicity of the final product. 
Oregami dicecion game analogy

#### OO


