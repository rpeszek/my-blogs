---
title: Almost nothing works. cost of an average large scale software project
title2:  Convenience is Not Cost Effective. On Limiting the Cost of Software Projects.
author: Robert Peszek
toc: true
summary: todo
---

## Introduction to Dystopia 

We rarely think about the long term cost of the software project.  We call it, dismissively, the maintenance cost.  
We talk about: clean code, readable code, elegant code, good design, best coding practices, agile process, test driven development ...  
These things are all opinions.  The maintenance cost is the factual verification of these options.   
I know, this is so disappointing, it is not the WTF/minute rate during code reviews, it is the maintenance cost. 

It is hard to measure such cost and what contributes to it. The data is not collected or is not public. 
I never had to put a JIRA ticket number on the timesheet. (ref to bugs paper)

But, we kinda know what is eating our lunch.   

I am going to discuss several well known aspects of coding which result in high maintenance.  I am going to show, what feels like a dystopian reality, in which high maintenance cost is unavoidable.  The only thing that works it taking away developer's ability of doing _certain_ things.  I will play devil's advocate a bit and will examine 
well known topics, like _mutating state_, _null_, _typed exceptions_ deriving conclusions that are seemingly counterintuitive and contrary to common belief.  

I consider this topic scary.  The really scary would be any shallow attempts to fix the problem.  We (humans) are good at shallow and phenomenal at avoiding the real issues.  

This article is a dump of my 25 years of experience in writing software.  It will touch on different technologies and languages. 
I will focus on languages where I have had large scale project experience: Java, JavaScript, Groovy, Haskell, and only touch on 
languages I coded is lesser anger.  

This post is not language specific, it is not about OO or FP, but it will use examples from both domains and will include code snippets.
This post is not about establishing superiority of FP over OO, my goal is to focus on the topic at hand.

You are likely do disagree with me.  You are welcome to do so.  I want to present my viewpoint and evidence and have you think about it.

I felt conflicted writing this.  I feel that I should write code not prose. I typically feel a need to write prose if something bugs me.
This is the case here.  ... 

## Case Study:  Separation of Concerns 

Many things can be decoupled.  I will just focus on one: separating business logic from user presentation.  Code that mashes everything together is not hard to write: you just add a bit after a bit until you finish. Such a melting pot of everything can get unruly and hard to maintain.  It is also hard to reuse (more on this later). You want your tea served separately from your steak. 

Separation of Concerns (whatever the concerns are) requires discipline, lots of thinking, and is an up-front investment.
Some engineers will view it as a needless complexity.  Do not stop reading if you are one of such people. The goal here is to examine historical evidence of how things have changed, not to criticize your culinary taste.  If you like your tea extra rare it is fine by me.

The changes between 1990-ties and now can tell us something interesting.
Late 90-ties were the fat client times.  Most of these applications mashed everything together, one line disabled / enabled a button, the next one was a DB select statement.  
The slogan was: RAD (Raid Application Development). Tools like Borland C++ Builder, PowerBuilder made writing fat clients faster.  They did not prevent developers from decoupling their code but done nothing to enforce it.   
Keeping UI separate is not a new concept.  MVC pattern is credited to Smalltalk and dates back to 1970.  It was not a forgotten idea either. 
The information was available to anyone who cared.  The interesting bit is that very few projects cared about separating UI from the rest.
I have no way of quantifying this statement.  I can only offer stories from my personal experience.

In one of my first programming jobs (~ 1997), we managed to persuade the management to rewrite parts of our legacy code using the new cool language called Java. The code we were writing was quite elegant. In particular, we were very diligent about separating all kinds of concerns, separation business logic from UI being one of them. I witnessed a rare example where maintenance cost was considered to be an important part of the bottom line.
These were the times when I was completely infatuated with OO, design patterns ... 

Our design had a sharp contrast to the surroundings.  I remember implementing integration pieces for one of our clients.  That client used a very reputable consulting firm (which I will not name) to develop their website.  How do we integrate?  We were told that our best bet is to do HTTP GETs and POSTs that mimicked they web pages. That website was built using a cutting edge technology called JSP (around 1999). 
It was a code melting pot of everything.  Files 1000+ lines long that created HTML, did database calls, and everything in-between. 
It was not fat client per se, but it was fat in spirit. 
Obviously such integration would not have much chance of being stable.  User presentation changes would easily break it.

A similar experience (and around the same time) was the _Great Plains_ (I believe now owned by Microsoft) ERP software integration.
I DUNNO how that looks now, but back then you interacted with it by programmatically interacting with its UI because there was no other way.  Great Plains was a big software company. It was programmed by capable engineers who were not afraid of creating a proprietary language ([Dexterity](https://en.wikipedia.org/wiki/Dexterity)).  

Other integrations were not that different:  it was either going trough the UI or making direct DB calls.

My last experience with a fat client app was less than 10 years ago. It was a Borland C++ project I had to maintain as I was working on re-implementing it using browser based single-page app.  That C++ project was not much different to what I remember from the 90-ties. 

This has changed. Today, most integration work is REST API or even GraphQL. SOAP is old school. I only know of one recent example where the only possible thing to do was a screen-scrap, but what was the rule is now an exception.

Browsers with powerful UX capabilities changed all of this. You can't easily do DB calls from the browser. 
A further change was the adoption of the mico-service architecture.  This not only forces the separation but also creates a need
to define a meaningful API for each micro-service.  

**My claim:** _The current separation of UI and business logic (when present) should be credited to the the back-end / front-end split and the micro-service design of the technology stack_. 

Back to the future: I found this talk by _Brooklyn Zelenka_ 
[_A Universal Hostless Substrate: Full Stack Web Apps Without a Backend, and More!_](https://www.youtube.com/watch?v=ulai-LGt0yU) very interesting. But it makes me think what will happen when developers are, again, allowed to mix things together.

## Claim: Convenience is Not Cost Effective

My observation:

_High maintenance cost is the default state of a typical, large software project._   
_Certain technology stack limitations are effective in lowering that cost. At large, nothing else is._

To be clear, only certain technology restrictions can be beneficial.  For example, the original JS Ajax API resulted
in the [Callback Hell](the-case-of-callback-hell-in-javascript) nightmare.  
Enforcing good coding practices is like herding cats.  It may work well on some projects, it does not work in general.  
Hard to bypass limiations seem to be the only direction that works. 
 
An equivalent formulation:

_The Podocide Rule: We will shoot ourselves in the foot unless it is hard for us to do so._



__This is clearly very concerning.__  I am right, it will get worse as technology improvements add flexibility and remove limitations.  At first and naive glance, some may be tempted to limit developers creative freedom.  This would be a very bad idea.  We do not want more waterfalls or other cures that will be even worse than the illness. 
Incidentally, Winston W. Royce did not propose the [waterfall](https://pragtob.wordpress.com/2012/03/02/why-waterfall-was-a-big-misunderstanding-from-the-beginning-reading-the-original-paper/) approach, he criticized the idea, and "the redditers of that time" apparently missed the important bits.

## Exhibit 1: `null`

If there is one thing that everyone agrees is causing maintenance issues that would have to be the infamous `null`!   
The 'billion dollar mistake" quote dates 2009.  Language like ML new it is a bad idea in 1970 and stayed away from introducing it.  

I want to take a different view of the `null` problem.  What it suggests is:

> Gentlemen's agreements do not work at large

Developers were never forced to use nulls!  We could have just make 'a gentlemen's agreement' on using a more maintainable approach that avoids nulls. 
Why didn't we?

Any language that can express a higher order function can express `Option` / `Maybe`-like type. 
This is not an FP tutorial, my point is only to convince you that the `null` could have been easily avoided without any changes to your language.

The following pseudo code shows how this can be done _this is called Continuation Passing Style (Programming Lingo), Church Encoding (Lambda Calculus Lingo), a special case of Yoneda (Category Theory Lingo), Haskellers will recognize `maybe` with lowercase "m"_)

``` 
option<T> = forall R . (R, T -> R) -> R
```

(uppercase chars represent types)

`Option` is simply a function of two parameters, one of them is a function.  This construction has been know at least since 1970-ties.
It can be replace `null` and does not need to use `null` in its implementation.

__Side Note:__  Compare this with, say, Java 8 implementation of `Optional` which is still is `null` based and full of booby traps.  It requires some "gentlemen agreements" to avoid the `NoSuchElementException`.  Is `NoSuchElementException` so much better than `NullPointerException`?   
I have not written Java for close to 3 years, let me try to implement the above construction:  

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
I can still write a one-liner in about 20 lines! 
Of course, you should change it by making constructors private and exposing static constructor methods
for `some` and `empty`, still will be (< 30 lines), then provide ways to check if it is empty or not (< 40 lines)...  
I think, this should work in any Java version >= 1.5 (2004), by adding a, missing back then, `Function` class.  

You can convert _nullable_ data to it and back from it equally easy, here is the more interesting direction: 

``` java
class RecoverStupidity {
    static <T,R> T toNullable(Option<T> o) {
        return o.option(null, r -> r);
    }
}
```

(this code will get a bit longer in older versions of Java.) 
Similarly, you can convert to and from the Java 8 `Optional`.  
__End Side Note.__

Putting Java in the spotlight, here are some interesting questions: 

*  How many Java projects before Java 8 avoided using `null`?  
*  How many projects avoid using `null` today?

My guess (to both questions) is: very, very few and my belief is that only a small percentage of today's projects take advantage of the somewhat better
Java 8's `Optional`.

You could argue that software engineers do not know theoretical things like Lambda Calculus or Category Theory and the solution I have outlined would 
have been just to abstract and hard to understand for them.  I am completely not buying this argument. 
Do engineers think how a sort function it is implemented when they use it?  We learned to code with black boxes.  This is just another black box that can expose very down to earth interface to novice users.

'The billion dollar mistake' was just in giving programmers the ability to use `null`, no one forced us to use it.  
The only way to eradicate the issue is to use language that does not have the `null`.  If `null` is available we will use it, ...
even when implementing the std library `Optional` class that is intended to get rid of it.   

### FP Option / Maybe like types

This problem goes deeper.  `Option` like types are not a full solution to the `null` problem.  `Option` is still problematic since (1) is convenient to use
(2) it represents an unspecified error condition.
The end result is a lot of code that has _unspecified errors_ that could have been specified. I wrote a whole post about it (...).

IMO most software projects have a big _sunny day scenario_ bias.  Sunny day is what ships the product out of the door while
_rainy day_ is what happens in production.   
`null` is used because it is easy to use and provides the same _sunny day_ output and most importantly delivers the first cut of the product slightly faster.
In languages like Haskell, `null` is not available so its nearest cousin gets used instead.

## Exhibit 2: Shared Mutating State vs Web Applications

Did you ever had to work on a code where flipping 2 lines was almost certain to break it?  Not completely, that would be simple, just a few scenarios break.  Shared mutable state can do that. The question is how technology stack can impact the amount of mutable state inside the app. 

A typical web app back-end when processing a request is going to read some data from the database,  do some data processing, write some data back and return some data to the front-end.  There is no long lived state that the app needs to keep in memory.  That is what allows apps to process concurrent requests from many users.
At the same time, a lot of state becomes encoded in URLs and and such (we call it _Hypermedia as the Engine of Application State_).

This is a big change from fat client times. The apps we write today are almost stateless by comparison.
It is not because developers are concerned about mutable state.  It is because the technology took it away from them.

Reducing does not mean preventing. One of more costly pieces of code I had to maintain was a Java _struts_ page that included about 200 instance variables across inheriting classes.  There were many, many methods that would mutate some subset of these 200 variables using conditional logic that depended on some other subset of these variables.  Try changing order or inserting exiting method between...  Trying to picture interaction of 200 interdependent pieces of state could be fun, unless, there is a deadline. Interestingly, this mess was blamed on _struts_.  The entry form may had maybe 15 fields, how does that justify 200 instance variables?
You may think that we do not overuse instance variables any more?  Try reading code for some vue.js components.

Haskell is one of few languages that comes with _Software Transactional Memory_. 
This is really nice and easy to use technology.  The ease of use == danger of overuse.  It not only rimes, it is true.
In my experience, developers are likely to consider the impact of keeping the data in memory on the data loss when application is terminated / restarted but are less
likely to consider implications on ability to scale a back-end process that uses STM.  Do not take me wrong, I am not saying STM always prevents scaling.   But, depending on the data and the design it might stay in the way.  
You may notice the difference in the scope of the problem.  I am mentioning it to show that as long as there is opportunity, the pattern of overuse will exist.

My conclusion is that it is not enough if the technology reduces the problem for us.
To be effective at large, the technology needs not only to reduce the problem but to limit our ability to create it.  



## Exhibit 3: Typed Exceptions

There is no consensus about typed exceptions being a good thing.  I consider type level information about exceptions to be good, but my goal is not to persuade you: rather to examine
when technology limitations can effectively impact how we write code.

Limiting the use of untyped exceptions is happening in Rust, and for a good reason.  It is very hard to provide type level guarantees about things if you can bypass the type system.  Linear types do not like untyped exceptions.  No logic does.   
In Rust, exceptions are typed but you also have ability to `panic!` bypassing the type limitations. 
However, there is no way to recover from `panic!` making it not a convenient all-purpose programming choice.  
It is possible to accidentally panic (e.g. by unwrapping `Option` / `Result` types). That seems to be a bit of a booby trap. 
But overall, currently, Rust is a great example of technology that limits what developers can do and ripping the benefits. 

Rust exceptions may have been influenced by Haskell. Haskell exceptions and errors are rather complex and I am not going to describe them in details.
It suffices to say that Haskell has `error` function that is a bit like Rust's `panic` and `Either` that is a bit like Rust's `Result`.
The `error` function is intended to be used for things like asserting program bugs.  Similarly to Rust's unwrapping of `Option`,  Haskell's `fromJust`
unwraps `Maybe` and uses `error` if there is no data to unwrap.  
There is one interesting difference,  in Haskell you can catch and recover from errors raised by the `error` function,  you can't recover from `panic` in Rust. 
__Rust is more limiting than Haskell in that respect.__  The use of `error` is quite popular in Haskell. 

Untyped errors when Utf8 decoding binary data has resulted in many headaches in my current job.  This is not unique to the code base I work on.  Here is a link ...
The end result is that you **need** to be conservative and catch untyped errors, especially if you implementing a long running process.
Catching all errors is a bit of a problem, for example, how do you decide if you should even try to recover from an error?  The `error` payload is `String`.

I think this was an interesting, counter intuitive example on flexibility and limitations:  

_Adding ability to catch errors made them more popular and harder to handle_. 

Just for grins, what would it feel like to program in a language that does not allow user to define untyped errors?  
Some people will argue now that, unless you check totality, you can introduce non-termination by simply looping forever. 
This is a common defense of the Haskell's `error` function.  
This also leads to a fascinating question: _Given no other choice would developers purposefully introduce infinite loops to satisfy the type checker?_ 

One of my absolute favorites, Idris, does not have (as far as I know) an equivalent of Rust's `panic`.  All user defined errors need to be typed.  
Well, Idris has unsafe coercions `belive_me : a -> b` and `really_believe_me : a -> b` which are documented as: "Use it with extreme care - it can result in segfaults or worse!".  
You could possibly implement `panic = believe_me` and segfault yourself out of the app.  
Unfortunately, Idris is not popular enough to make a good case study of effectiveness of its limitations.

Finally, it is interesting to think about what happened to Java checked exceptions.
They were not popular.  The critics argued that checked exceptions force unnecessary boilerplate. Java forced developer to write code to handle certain exceptions but there was often not much that the developer could do other than logging the exception or re-throwing it as another one. 
The end result was that the community started avoiding checked exceptions and moved to unchecked `RuntimeException`.  C# dropped the concept from the start.  
Limitations with available workarounds do not work.  If unchecked exceptions were harder to use or limited in scope this might have been different.

So I think this was an interesting example about flexibility vs limitation:  

_Easy to use untyped exceptions almost eliminated the use of checked exceptions in Java_. 

## Exhibit 4: Libraries and Code Reuse

Code reuse has been a programming goal. that is considered by many to be a failure.  What has been often pointed out 
is that code reuse does not work well on the project level.  But it does work for libraries. 
Why is this the case?

As library author you do not need to be concerned about code reuse.  Your code is being reused, that job is passed to others.
As library user your freedom is very limited.  You have to do what the library requires you to do.  
This is a perfect example of a limitation forced on programmers by the technology.  And it works.

There is no reason why the same level of code reuse could not be accomplished on the project level, other than 
extra freedom of how the code reuse is accomplished.

## Exhibit 5: Memory Bugs

This has been discussed a lot.
https://visualstudiomagazine.com/articles/2019/07/18/microsoft-eyes-rust.aspx
https://www.zdnet.com/article/chrome-70-of-all-security-bugs-are-memory-safety-issues/

And I do not want to only summarize what has been said:

*  Best programming practices are not good enough to avoid these problems
*  Partial solutions (like smart pointers in newer versions of C++) are not good enough either
*  Limiting what developers can do: works

Limiting what developer can do means:  no memory management (garbage collection handles it), 
no access to direct pointer programming, or type safety over memory and pointer use (linear types, Rust).


## Exhibit 6: Type Safety

## Exhibit 7: Configuration

### Hard-coding gets the job done

### yaml 

Yaml is my last exhibit.  That reminds me of a joke.  Izik


## Constraining Languages and The case of Callback Hell in JavaScript

On the surface this looks very similar to the `null` problem.  People agree it is bad, Continuation Passing Style has been known for ages...
The issue here is that a clean implementation of CPS has been very hard on top of what JavaScript offered before ES6.  
It would be not fair to criticizes JavaScript programmers for Callback Hell because of how the original Ajax API exposed looked like.
Not all technology limitations are good,  this one gave us a gun already pointed at the foot.  

The way I think about it that the mainstream language designers and library designers are not that different from the rest of us. 
They still need boundaries otherwise they will mess things up.  What would be the limitation for a language designer?
It cannot be technology it has to be something else.  Functional Programming languages derive their limitations from formalism.
PLT (Programming Lanaguage Theory) is very mathematical.  Not sure what that is for Java, ES6, or C++ committee.  
Sure, there are language specific goals (like performance for C++), but these do nothing for the maintenance cost. 

Similarly to Java `Optional`, JavaScript Promises are not ideal. Fortunately, it is possible to clean them up. There are some interesting alternatives that could be 
offered as a library.  For example: 

https://github.com/dmitriz/cpsfy

The interesting question how popular such libraries will be?  
My prediction is that they will not be used much.  

## Making Sense of the Dystopia
### The Industry, The Managers, The Bottom Line

Maintenance cost is typically overshadowed by short term delivery goals.  
In many places, the time to market pressure tends to be the driving factor.  
Low defect rate and low maintenance cost are often not the high priorities.
For many projects, the maintenance cost is passed to the customers.  Such cost structure matches the process that emphasizes delivery timelines over quality.  

Software Development suffers from an acute case "temporal mico-economics":  Seemingly optimal short term choices are costly to the project in the long term.  In this Adam Smith's model of programming, the software developer takes the shortest / easiest / KISS path to accomplish the next incremental goal.  At long term, this is far from shortest or simplest or maintainable.   

Another big part of the issue is the corporate inertia.  Nobody gets fired for not changing the old process or for selecting 'proven' old tools, even if the old process and the old tools do not work well.  Programmers do not have decision power.  MBAs are not competent to make
technical decisions and will resist change and prefer known evil. 

For example, I know a whole industry segment which produces a lot of data outputs with high accountability and correctness requirements. Their standard practice is called "double programming" (2 indepedently programmed data output derivations need to agree) but, believe it or not, programmers do not use any version control!  Something like git is not a part of their technology stack.  Version control is not what any of these companies do and hence none feels any pressure to change.


### Perpetual Immaturity and anti-math attitude

I think this observation came from Robert C. Martin (Uncle Bob):  The number of programmers doubles every 5 years.
Do the math.  Assuming none ever retires or leaves, that makes 1/2 of the workforce with less than 5 years of experience.
This profession is considered to be the domain of young.  

It is hard not to notice the anti-theoretical attitude in the industry.  Math has place in algorithms or data science but mathematical properties of computations?  How dare you! 
I blame the education.  Mathematics is now about memorizing formulas, not learning formal deduction methods.

A short quiz:  What does this mathematical formula have to do with programming:

a ^ (b ^ c) = a ^ (b * c)

How about this one:

a ^ (b + c) = a ^ b * a ^ c

If you know the answer, where did you learn it?  In school?  I have not learned it in school or in university. 

Here is evidence from my personal life:  At some point to improve my marketability I decided to remove my PhD (mathematics) from my resume.
It was something that was suggested by my friend and worked very well.  I felt bad about it because it felt like cheating and I 
made my history known after the interviews with the statement "Please do not hold it against me,  as you can see I am a down to earth guy".   
I have heard more then one conversation criticizing someone's decision of pursuing a PhD.  
Flipping the coin.  It feels like the community is discriminating against knowledge.  That cannot be beneficial!

### Popularity

The technical decisions are either made my the management or are subject to some form of democracy where developers make a group 
decision.  On a large scale this becomes a popularity contest. People read the TOBIE index, RedMonk index ...
This problem is not unique to software and has been an issue with humanity all the way back to Socrates death.  
Why we think that popular must be the best?  I guess because it is popular to think that.

### Can't Pull Request the World

In several encounters on reddit I have been schooled like this:

"if you think this is error prone, just don't use it"

### Agile

Agile projects vs linear learning
Learning from projects does not work.


### Adaptation 




Not unique to the industry, appears to be human nature (death of Socrates)

### KISS

Process Simplicity not Simplicity of the final product. 
Oregami dicecion game analogy


-- 
Software Development suffers from an acute case "temporal mico-economics":  Seemingly optimal short term choices are costly to the project in the long term.  In this Adam Smith's model of programming, the software developer takes the shortest / easiest / KISS path to accomplish the next incremental goal.  At long term, this is far from shortest or simplest.   

That make sound like criticism of agile development.  I think this has much deeper roots than the process.   
You may say, you are forgetting about refactoring. I do not. Refactoring will not address any of the situations I am about to cover.
It seems like 


This reminds me of a modified [tangram](https://en.wikipedia.org/wiki/Tangram) game where each piece is served one at the time
and the goal is to create the simplest possible design without moving previously placed pieces.  This does not work!

This reminds me of a modified [tangram](https://en.wikipedia.org/wiki/Tangram) game where each piece is served one at the time
and the goal is to create the simplest possible design without moving previously placed pieces.  This does not work!


This take on wining argument never worked for me:

_Hey, this is will work for sure, it has a formal verification of correctness_
