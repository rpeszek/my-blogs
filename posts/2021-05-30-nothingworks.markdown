---
title: Almost nothing works. cost of an average large scale software project
author: Robert Peszek
toc: true
summary: todo
---



I like to think about the long term cost of a software project.  We call it, dismissively, the maintenance cost.  
What are these things: clean code, readable code, elegant code, good design, good documentation, best coding practices, agile process, test driven development?  These are all opinions.  What is the maintenance cost?  It is the factual verification of these options.   

It is really hard to measure such cost and what contributes to it.  The data is typically not collected and, if it were, it would not be public.  I never had to put a JIRA ticket number on the timesheet.  (ref to bugs paper)

But, we kinda know what is eating our lunch.   

I am going to discuss several aspects of coding which result in a high maintenance.  I am going to present, what feels like a dystopian reality, in which high maintenance cost is unavoidable.  The only thing that works it taking away developer's ability of doing _certain_ things. 

This article is a dump of my 25 years of experience in writing software.  It will touch on different technologies and languages. 
I will focus on languages where I have had large scale project experience: Java, JavaScript, Groovy, Haskell, and only touch on 
languages I coded is lesser anger.  

This post is not language specific, it is not about OO or FP, but it will use examples from both domains and will include code examples.
This post is not about establishing superiority of FP over OO, my goal is to focus on the topic in the title.

You are likely do disagree with me.  You are welcome to do so and argue a different point.  
I want to present my viewpoint and evidence and have you think about it.
I consider this topic scary.  The really scary would be any shallow attempts to fix the problem.  We (humans) are good at shallow and phenomenal at avoiding the real issues.  


## Case Study:  Separation of Concerns 

Many things can be decoupled.  I will just focus on one: 
separating business logic from user presentation.  Code that mashes everything together is not hard to write: you just add a bit after a bit until you finish. But if you (or someone else) have to change it later... You want your tea served separately from your steak. 

Separation of Concerns (whatever the concerns are) requires discipline, lots of thinking, and is an up-front investment.
Some engineers will view it as a needless complexity.  Do not stop reading if you are one of such people. The goal here is to examine historical evidence of how things change, not to criticize your culinary taste (tea served extra rare).

The changes between 1990-ties and now can tell us something.
Late 90-ties were the fat client times.  Most of these applications mashed everything together, one line deciding on disabling / enabling a button, the next one was a DB select statement.  
The slogan was: RAD (Raid Application Development). Tools like Borland C++ Builder, PowerBuilder made writing fat clients easy.  They did not prevent developers from decoupling their code but done nothing to enforce it.  

Keeping UI separate is not a new concept.  MVC pattern is credited to Smalltalk and dates back to 1970.  It was not a forgotten idea either. 
The information was available to anyone who cared.  The interesting bit is that very few projects cared about separating UI from the rest.
I have no way of quantifying this statement.  I can only offer stories from my personal experience.

In one of my first programming jobs (~ 1997), we managed to persuade the management to rewrite our legacy code using the new cool language called Java. The code we were writing was quite elegant. In particular, we were very diligent about separating all kinds of concerns, separation business logic from being one of them. This is a rare example where maintenance cost was considered to be an important part of the bottom line.
These were the times when I was completely infatuated with OO, design patterns ... 

Our design had a sharp contrast to the surroundings.  I remember implementing integration pieces for one of our clients.  That client used a very reputable consulting firm (which I will not name) to develop their website.  How do we integrate?  We were told that our best bet is to do HTTP calls. That website was built using a cutting edge technology called JSP (around 1999). 
It was a code melting pot of everything.  Files 1000+ lines long that created HTML, did database calls, and everything in-between. 
Obviously such integration would not have much chance of being stable.  User presentation changes would easily break it.

A similar experience (probably around 1999) was the _Great Plains_ (I believe now owned by Microsoft) ERP software integration.
I DUNNO how that looks now, but back then you interacted with it by programmatically interacting with its UI because there was no other way.  
Great Plains was a big software company. 
It was programmed by capable engineers not afraid of creating a proprietary language ([Dexterity](https://en.wikipedia.org/wiki/Dexterity_(programming_language))).  

Other integrations were not that different:  it was either going trough the UI or making direct DB calls.

My last experience with fat client app was less than 10 years ago. It was a Borland C++ project I had to maintain as I was working on re-implementing it using browser based single-page app.  That C++ project was not any different. 

This has changed.  Today, most integration work is REST API or GraphQL.  I know of one recent example where the only possible thing to do was a screen-scrap but what was the rule is now exception, what was the exception is now the rule.

**My claim:** _The current separation of UI and business logic (when present) should be credited to the the back-end / front-end split in the technology stack_. 

Browsers with powerful UX capabilities (old buzzword "thin client") changed all of this. Fortunately, you can't very easily do DB calls from the browser.  This is not just about the browser.  Component architectures like CORBA gained some popularity in very late 90-ties and early 2000s (precursors to todays microservices).  Projects that used CORBA had similar benefits because you did not write UI code in COBRA.   

Back to the future: I found this talk by _Brooklyn Zelenka_ 
[_A Universal Hostless Substrate: Full Stack Web Apps Without a Backend, and More!_](https://www.youtube.com/watch?v=ulai-LGt0yU) very interesting. But it makes me think what will happen when developers are, again, allowed to mix things together.

## Technology Limitations are the only Thing that Works

My observation:

_High maintenance cost is the default state of a typical, large software project_   
_Certain technology stack limitations are effective in lowering that cost, at large, nothing else is_

Enforcing good coding practices is like herding cats.  It may work well on few projects, it does not work in general.
To be clear, only certain technology restrictions can be beneficial.  For example, the original restricted JS Ajax API resulted
in the [Callback Hell](the-case-of-callback-hell-in-javascript) nightmare.  

An equivalent formulation:

_The Podocide Rule: We will shut ourselves in the foot unless it is hard for us to do so._

I like this one too:

_Convenience is the enemy of reason_

Software Development suffers from an acute case "temporal mico-economics":  Seemingly optimal short term choices are costly to the project in the long term.  In this Adam Smith's model of programming, the software developer takes the shortest / easiest / KISS path to accomplish the next incremental goal.  At the zoomed out level this is a maze and is far from shortest or simplest.   

This reminds me of how ants move.  An ant (not to be confused with the very old Java build tool) moves in straight short lines which look like a chaotic zig-zag when combined.  Unless, of course, we build a tiny ant tunnel _limiting_ their movements to only one direction.  

The other good analogy is Oregami dicection game.  ...
Unless, of course, we do _limit_ how the pieces can fit together.

That make sound like criticism of agile development.  I think this has much deeper roots than the process.   


__This is clearly very concerning.__ 

Ironically, it will get worse as technology improvements add flexibility and remove limitations.   
At a first, naive, glance, some may be tempted to limit developers creative freedom.   
This would be a very bad idea.  We do not want more waterfalls or other cures that will be even worse than the illness. 

Incidentally, Winston W. Royce did not propose [Waterfall](https://pragtob.wordpress.com/2012/03/02/why-waterfall-was-a-big-misunderstanding-from-the-beginning-reading-the-original-paper/), he criticized the idea, "the redditers of that time" apparently missed the important bits.

## Exhibit 1: Shared Mutating State vs Web Applications

Did you ever had to work on a code where flipping 2 lines was almost certain to break things?  Not all things, mind you, just 10%
of scenarios to make the testing more interesting.  Let us look at how technology stack impacted the situation. 

As discussed already: most today's apps are split: front-end - back-end - DB.  Back-end runs inside some web framework which runs many threads processing HTTP requests.  The bits of _back-end_ programs that developers write and execute on the web server have really no choice but to read the state from the DB at every single request and immediately write it back.  At the same time, a lot of state becomes encoded in URLs and and such (_Hypermedia as the Engine of Application State_).

This is a big change from fat client times.  This technology reduces the need for shared mutable state by at least a factor!
It is not because developers are concerned about shared state.  It is because the technology is reducing the need for such state.

Reducing does not mean preventing. You do not need to believe in FP to see pragmatic benefits of avoiding mutable state when you can.  Yet, such avoidance hardly something I was able to expect from the code in the past.  One of the worst examples of code I had to maintain in my career was a Java _struts_ page that included about 200 instance variables across inheriting classes.  There were many, many methods that would mutate some subset of these 200 using conditional logic.  Try changing order or inserting exiting method between other two... Several years have passed but I still remember how hard that was.  I imagine, trying to picture interaction of 200 interdependent pieces of state could be fun, unless, there is a deadline. Interestingly, this mess was blamed on _struts_.  The entry form may had 15 fields, how does that justify 200 instance variables?

Mutating global state is very tempting.  Haskell is one of few languages that comes with _Software Transactional Memory_. 
This is great a very useful and easy to use technology.  The ease of use == danger of overuse.  It not only rimes, it is true.
In my experience, developers are likely to consider the impact of state loss when application is terminated / restarted but are less
likely to consider implications on ability to scale a back-end process that uses STM.  Do not take me wrong, I am not saying STM always prevents scaling.   But, depending on the data and the design it might.  
You may notice the difference in the scope of the problem.  I am mentioning it to show that as long as there is opportunity, the pattern of overuse will exist.

My conclusion is that it is not enough if the technology solves or reduces the problem for us.
To be effective at large, the technology needs not only to reduce the problem but to limit our ability to create it.  

## Exhibit: `null`

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
They were not popular.  The critics argued that the force unnecessary boilerplate. Java forced developer to write code to handle certain exceptions but there was often not much that the developer could do other than logging the exception or re-throwing it as another one. 

The end result was that the community started avoiding checked exceptions.  C# dropped the concept from the start.

Using Haskell as example: I really wish that there were fewer untyped errors.  I dislike IOError too.   
Untyped errors in Utf8 decoding has resulted in many headaches in my current job.  The problem is that the concept of error (untyped) and exception (typically typed) in Haskell relies on a gentelmen's agreements like: "exception for expected but irregular situations at runtime and the error for mistakes in the running program that can be resolved only by fixing the program" or
"Errors can be prevented by (cheap) checks in advance, whereas exceptions can only be handled after a risky action was run"
(see https://wiki.haskell.org/Error_vs._Exception).  Many libraries violate such agreements, client programs lack diligence of confirming to these rules, and on top of all of this the rules themselves are not very consistent.  You head will spin, if, after reading this, you consider this definition:

```
type IOError = IOException
```

Incidentally, limiting the use of untyped exceptions is happening in Rust, and for a good reason.  It is very hard to provide type level guarantees about things if you can bypass the type system.  I can only hope that `panic` will not become easier to use and more popular.  

Why I do not like untyped errors?  Programming answer:  the concept of encapsulation is not a bad one if used correctly, information hiding is great if
the information can remain hidden.  You do not want a program you call to start boiling water and have you guess that you need to turn off the gas at the end.  Untyped exceptions do just that.   
Mathematical answer:  There is no place for untyped errors in Curry-Howard isomporphism,  mathematics is gone.   


Incidentally, type level exceptions are making a comeback in Rust for a good reason.  It is very hard to provide type level guarantees about things
if you can bypass the type system.  Linear types do not like exceptions.  I can only hope that `panic` will not become easier to use and more popular.  


What is the lesson here? 

Java started in a good direction but offered only partial limitation (forcing handling of checked exceptions) with relatively easy work-around moving to unchecked `RuntimeException`.  Haskell makes it easy for users to define untyped errors asking that the users do not overuse them.   

Limitations with available workarounds do not work.  If unchecked exceptions were harder to use or limited in scope this might have worked.  Haskell makes it easy for users to define untyped errors asking that the users do not overuse them.  This has not worked 


## Code Reuse 

big ticket item.  Good to notice what works what does not.

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


## Configuration

### Hard-coding gets the job done

### yaml 


## The case of Callback Hell in JavaScript

On the surface this looks very similar to the `null` problem.  People agree it is bad, Continuation Passing Style is an old technique...
The issue here is that a clean implementation of CPS would have been hard on top of . 

JS programs were famous for being not maintainable.  The apptly named _Callback Hell_ is one of the reasons for that reputation. 
Again, nobody twisted anyone's arm.  Continuation Style code dates back at least to 1950-ties.  
A more monadic style JS `promises` like constructions date back to 1994 in Haskell.  

The way forward was the technology stack change and introduction to ES6.

https://github.com/dmitriz/cpsfy

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


