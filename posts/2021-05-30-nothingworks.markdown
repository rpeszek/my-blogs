---
title:  A Journey over Dystopia where Common Sense and Programming Guidelines Do Not Work.
title3: Almost nothing works. cost of an average large scale software project
title2:  Convenience is Not Cost Effective. On Limiting the Cost of Software Projects.
author: Robert Peszek
toc: true
summary: todo
---

> _"Good judgemnet comes from experience, and a lot of that comes from bad judgment"_,  Will Rogers

## Introduction to Dystopia 

We rarely think about the long term cost of the software project.  We call it, dismissively, the maintenance cost.  
We talk about: clean code, readable code, elegant code, good design, best coding practices, agile process, test driven development ...  
Let's call these things Programming Guidelines. These things are all opinions.  The maintenance cost is the factual verification of these options.   
I know, this is so disappointing, it is not the WTF/minute rate during code reviews, it is the maintenance cost. 

It is hard to measure such cost and what contributes to it. The data is not collected or is not public. 
I never had to put a JIRA ticket number on the timesheet. (ref to bugs paper)

But, we kinda know what is eating our lunch.   

I am going to discuss several aspects of coding that have been blamed for high maintenance.  I am going to show, what feels like a dystopian reality, in which high maintenance cost is unavoidable.  The only thing that works it taking away developer's ability of doing _certain_ things.  I will play devil's advocate a bit and will examine 
well known topics, like _mutating state_, _null_, _typed exceptions_ deriving conclusions that are seemingly counterintuitive and contrary to common belief.  

I consider this topic scary.  The really scary would be any shallow attempts to fix the problem.  We (humans) are good at shallow and phenomenal at avoiding the real issues.  

This article is a dump of my 25 years of experience in writing software.  It will touch on different technologies and languages. 
I will focus on languages where I have had large scale project experience: Java, JavaScript, Groovy, Haskell, and only touch on 
languages I coded is lesser anger.   
This post is not language specific, it is not about OO or FP, but it will use examples from both domains and will include code snippets.  
This post is not about establishing superiority of FP over OO, my goal is to focus on the topic at hand.

You are likely do disagree with me.  You are welcome to do so. I want to present my viewpoint and evidence and have you think about my examples.
You are also likely to agree with some of my examples and disagree with others.
I would like to ask you to focus on the examples we agree on instead of the once we do not.  I know, this is such a non-reddit thing to do.

I felt conflicted writing this.  I prefer writing code not prose. I typically feel a need to write prose if something really bugs me.
This is the case here.  ... 

## Case Study:  User Interface Separation

Keeping UI separate is not a new concept.  MVC pattern is credited to Smalltalk and dates back to 1970-ties. 
Separation of business logic from user presentation was a well known idea for as long as I remember.   
It is interesting to observe how a typicall business software app has eveloved in that regard.  The changes between 1990-ties and now can tell us something interesting.

Mid to late 90-ties were the fat client times.  Most of these applications mashed everything together, one line disabled / enabled a button, the next one was a DB select statement.  Early web applications were not any different.  

I am not claiming all projects mashed everything together.  I worked on quite a few that separated UI, but only on one where this was a design choice not forced by the technology. 
My first programming job, in late 90-ties, included a rewrite of parts of an old code base using a new and cool language called Java. 
I was one of 2 programmers wroking on the new code. Both or us were very much into OO, design patterns...
We were very diligent about separating all kinds of concerns, separation business logic from UI being one of them. The goal was to replace a
hard to maintain legacy code and the maintenance cost was considered important.  
I believe such projects were rare. 

I personally like the idea of separating concerns. A melting pot of everything can get unruly and hard to maintain. You want your tea served separately from your steak.  _My goal is not to evangelize separation of concerns, just to show the historical pattern._
If you like your tea extra rare, it is fine by me.

Here are some examples of projects I rememeber from the old days:

*  JSP pages we had to intergrate with (cutting edge in 1999) that mashed everything together (I remember being actually shocked by 1500+ line long files that done DB calls and everything else).  The only thing we could do it to mimic the UI in making GET and POST calls.
*  Integration work for ERP software called _Great Plains_ (I think now owned by Microsoft).  The only thing to do there was to programmatically interact with its UI using their tools.
*  Borland C++ and Java Swing apps I had to maintain, where the UI control and business logic was placed in event handlers.

It is worth noting that _Great Plains_ was programmed by capable engineers who were not afraid of creating a proprietary language ([Dexterity](https://en.wikipedia.org/wiki/Dexterity)).  Similarty JSP pages were the cutting age back then, the project was implemented but a very reputable consulting firm that hired good developers.  
Separation of Concerns (whatever the concerns are) requires discipline, lots of thinking, and is an up-front investment.
Some engineers always viewed it as a needless complexity.  But I do not think mashing the code together was intentional. 
It happens natrually when you just add a bit after a bit until you finish.

The key obeservation is that this has changed. 

**My claim:** _The current separation of UI and business logic (when present) should be credited to the the back-end / front-end split and the micro-service design of the technology stack_. 

You can't easily do DB calls from the browser. You would not control UI from a micro-service. Technology forced the change on us.

Back to the future: I found this talk by _Brooklyn Zelenka_ 
[_A Universal Hostless Substrate: Full Stack Web Apps Without a Backend, and More!_](https://www.youtube.com/watch?v=ulai-LGt0yU) very interesting. But it makes me think what will happen when developers are, again, allowed to mix things together.

## Claim: Convenience is Not Cost Effective

My observation:

_High maintenance cost is the default state of a typical, large software project._   
_Certain technology stack limitations are effective in lowering that cost. At large, nothing else is._

Enforcing good coding practices is like herding cats.  It may work well on some projects, it does not work in general.  
Hard to bypass limiations that enforce certain aspects of code seem to be the only direction that works. 

To be clear, only certain technology restrictions are beneficial.  For example, the original (restrictive) JavaScript Ajax API resulted
in the [Callback Hell](the-case-of-callback-hell-in-javascript) nightmare.   

An equivalent formulation:

_The Podocide Rule: We will shoot ourselves in the foot unless it is very hard for us to do so._

__This is clearly very concerning.__  I am right, it will get worse as technology improvements add flexibility and remove limitations.  At first and naive glance, some may be tempted to limit developers creative freedom.  This would be a very bad idea.  We do not want more waterfalls or other cures that will be even worse than the illness. 
Incidentally, Winston W. Royce did not propose the [waterfall](https://pragtob.wordpress.com/2012/03/02/why-waterfall-was-a-big-misunderstanding-from-the-beginning-reading-the-original-paper/) approach, he criticized the idea, and "the redditers of that time" apparently missed the important bits.

## Exhibit 1: `null`

If there is one thing that everyone agrees on is causing maintenance issues that would have to be the infamous `null`!   
The concept dates back to 1965.  The 'billion dollar mistake" quote dates 2009.  Some languages never introduced it.  One of them is ML 
created in (I think later) 1970-ties.  

I want to take a different view of the `null` problem.  What it suggests is:

> Gentlemen's agreements do not work at large

The least involved solution is to consistently name variables indicating that they are used with a possible null values.  E.g. `middleInitalNullable`. This would require diligence and consistency and some refactoring if we decided that, say, renaming `lastName` to 
`lastNameNullable` if it is sometimes unknown and we thought otherwise at the beginning.  ...

Developers were never forced to use nulls at all!  We could have just make 'a gentlemen's agreement' on using a more maintainable, less error prone approach. And why didn't we do that?

Any language that can express type variables and higher order functions, can express `Option` / `Maybe`-like type. 
This is not an FP tutorial, my point is only to convince you that the `null` could have been easily avoided without any changes to your language.

The following pseudo code shows how this can be done, this is called _Continuation Passing Style (Programming Lingo), Church Encoding (Lambda Calculus Lingo), a special case of Yoneda (Category Theory Lingo), Haskellers will recognize `maybe` with lowercase "m"_)

``` 
option<T> = forall R . (R, T -> R) -> R
```

(uppercase chars represent types)

`Option` is simply a function of two parameters, one of them is a function.  This construction has been know at least since 1970-ties.
It can be used to replace `null` and does not need to use `null` in its implementation.

__Side Note:__  Compare this with, say, Java 8 implementation of `Optional` which is still is `null` based and requires care to avoid getting the `NoSuchElementException` during both the construction and consumption.  Is `NoSuchElementException` so much better than `NullPointerException`?   
I have not written Java for close to 3 years, but let me try:  

``` java
interface Option<T> {
    <R> R option(R empty, Function<T,R> some);
}
final class OptionSome<T> implements Option<T> {
    private T t;
    //packate level scope to prevent constructing this object directly
    OptionSome(T t) {
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
Of course, you should change it by adding toString(), equals(), hashCode(), still will be (< 50 lines), then provide ways to check if it is empty or not (< 60 lines)...  
I think, this should work in any Java version >= 1.5 (2004), by adding a, missing back then, `Function` class.  

You can convert _nullable_ data to it and back from it equally easy: 

``` java
class ToFromOption {
    public static <T,R> T toNullable(Option<T> o) {
        return o.option(null, r -> r);
    }
    public static <T,R> Option<T> fromNullable(T t) {
        if (t == null) {return new OptionNone(); } else {return new OptionSome(t);}
    }
}
```

(this code would get a bit longer in older versions of Java.)   
Similarly, you can convert to and from the Java 8 `Optional`.  
__End Side Note.__

Putting Java in the spotlight, here are some interesting questions: 

*  How many Java projects before Java 8 avoided using `null`?  
*  How many projects avoid using `null` today?

My guess (to both questions) is: very, very few.

You could argue that software engineers do not know theoretical things like Lambda Calculus or Category Theory and the solution I have outlined would 
have been just to abstract and hard to understand for them.  I am completely not buying this argument. 
Do engineers think how a sort function it is implemented when they use it?  We learned to code with black boxes.  This is just another black box that can expose very down to earth interface to novice users.

'The billion dollar mistake' was just in giving programmers the ability to use `null`, no one forced us to use it.  
The only way to eradicate the issue is to use language that does not have the `null`.  If `null` is available we will use it, ...
even when implementing the std library `Optional` class that is intended to get rid of it.   

### FP Option / Maybe like types

This problem goes deeper.  `Option` like types are not a full solution to the `null` problem.  `Option` is still problematic since (1) it represents an unspecified error condition, (2) is convenient to use.
The end result is that a lot of code has _unspecified errors_ and with extra effort these errors could have been specified. I wrote a whole post about it here: 
[Maybe Overuse, Stories About Error Information Loss](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html).

IMO most software projects have a big _sunny day scenario_ bias.  Sunny day is what ships the product out of the door while
_rainy day_ is what happens in production.   
`null` is used because it is easy to use and provides the same _sunny day_ output and, most importantly, delivers the first cut of the product a bit faster.  This is a very localized optimization, lanaguages like Java or JavaScript are not fast in initial development, but they are familiar.  Easy and fast is in the eye of the beholder.   
In languages like Haskell, `null` is not available so its nearest cousin gets overused instead.


## Exhibit 2: Shared Mutating State vs Web Applications

Did you ever had to work on a code where flipping 2 lines was almost certain to break it?  Not completely, that would be simple, just some scenarios.  Shared mutable state can do that. 

A typical web app back-end, when processing a request, is going to: read some data from the database,  do some data processing, write some data back and return some data to the front-end.  There is no user request specific global state.  That design allows the web application to process concurrent requests from many users.
As a bonus, a lot of state becomes encoded in URLs and such (we call it _Hypermedia as the Engine of Application State_).

This is a big change from the fat client times. The apps we write today are almost stateless by comparison.
It is not because developers are concerned about mutable state.  It is because the technology took it away from them.

Reducing does not mean preventing. One of more costly pieces of code I had to maintain was a Java _struts_ page that included over 100 instance variables across inheriting classes.  There were many, many methods that would mutate some subset of these 100 variables using conditional logic that depended on some other subset of these variables.  Try changing order or inserting a method between...  Trying to picture interaction of 100+ interdependent pieces of state could be fun, unless, there is a deadline. Interestingly, this mess was blamed on _struts_.  The entry form may had maybe 15 fields, how does that justify 100 instance variables?
You may think that we do not overuse instance variables any more?  Try reading code for some vue.js components!

Haskell is one of few languages that comes with STM, the _Software Transactional Memory_. 
This is really nice and easy to use technology.  

_The ease of use == a danger of overuse_  

It not only rimes, it is true. In my experience, developers are likely to consider the impact of using STM on the data loss when application is terminated / restarted but are less likely to consider issues like scaling a back-end process that uses STM.  Do not take me wrong, I am not saying STM always prevents scaling.  But, depending on the design, it might.  
You may notice the difference in the scope of the problem too.  Functional Proramming and Haskell limit some freedoms preventing overuse of mutable state.  That improves the situation dramatically. But as long as there is opportunity, we will shoot our foot.  (At least with a rubber band!)

My conclusion is that it is not enough if the technology reduces the problem for us.
To be effective at large, the technology needs not only to reduce the problem but to limit our freedom to _create it_.  


## Exhibit 3: Typed Exceptions

There is no consensus about typed exceptions being a good thing.  I consider type level information about exceptions to be good, but my goal is not to persuade you: rather to examine when technology limitations can effectively impact how we write code.

Limiting the use of untyped exceptions is happening in Rust, and for a good reason.  It is very hard to provide type level guarantees about things if you can bypass the type system.  Linear types do not like untyped exceptions.  No logic does.   
In Rust, exceptions are typed but you also have ability to `panic!` bypassing the type limitations. 
However, there is no way to recover from `panic!` making it not a convenient all-purpose programming choice.  
It is possible to accidentally panic (e.g. by unwrapping `Option` / `Result` types). That seems to be a bit of a booby trap. 
But overall, currently, Rust is a great example of technology that limits what developers can do and ripping the benefits. 

Rust exceptions may have been influenced by Haskell. Haskell exceptions and errors are rather complex and I am not going to describe them in details.  I will only focus on untyped errors and exceptions of a fully typed variety. 
Haskell has `error` function that is a bit like Rust's `panic` and `Either` that is a bit like Rust's `Result`.  It has other things like
sync and async IO exceptions that I am not discussing.
The `error` function is intended to be used for things like asserting program bugs.  Similarly to Rust's unwrapping of `Option`,  Haskell's `fromJust` unwraps `Maybe` and uses `error` if there is no data to unwrap.  
There is one interesting difference,  in Haskell you can catch and recover from errors raised by the `error` function,  you can't recover from `panic` in Rust. 
_Haskell offers more flexibility in that respect._  As you can guess, `error` has been used in some places where `Either` would have
been a safer choice. 

Untyped errors when Utf8 decoding binary data have resulted in many headaches in my current job.  This is not unique to the code base I work on.  
Here is a link: [Haskell: The Bad Parts, part 2](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/). 
The end result is that you **need** to be conservative and catch untyped errors, especially if you are implementing a long running process.
Catching all errors is a bit of a problem, for example, how do you decide if you should even try to recover from an error?  The `error` payload is a `String`.

I think this was an interesting, counter intuitive example on flexibility and limitations:  

_Adding ability to catch errors made them more popular and, thus, harder to handle_. 

Just for grins, what would it feel like to program in a language that does not allow user to define untyped errors?  
Some people will argue now that, unless you check totality, you can introduce non-termination by simply looping forever. 
This is a common defense of the Haskell's `error` function. 
This argument leads to this, fascinating question: _Given no other choice would developers purposefully introduce infinite loops to satisfy the type checker?_ 

One of my absolute favorites, Idris, does not have (as far as I know) an equivalent of Rust's `panic`.  All user defined errors need to be typed.  
Well, Idris has unsafe coercions `belive_me : a -> b` and `really_believe_me : a -> b` which are documented as: "Use it with extreme care - it can result in segfaults or worse!".  You could possibly implement `panic = believe_me` and segfault yourself out of the app!  
Unfortunately, Idris is not popular enough to make a good case study of effectiveness of its limitations.

Finally, it is interesting to think about what happened to Java checked exceptions.
They were not popular.  The critics argued that checked exceptions force unnecessary boilerplate. Java forced developer to write code to handle certain exceptions but there was often not much that the developer could do other than logging the exception or re-throwing it as another one. 
The end result was that the community started avoiding checked exceptions and moved to unchecked `RuntimeException`.  C# dropped the concept from the start.  
Limitations with available workarounds do not work.  If unchecked exceptions were harder to use or limited in scope this might have been different.

So I think this was an interesting example about flexibility vs limitation:  

_Easy to use untyped exceptions almost eliminated the use of checked exceptions in Java_. 

On the subject of exceptions, it should be observed that the languages rerely limit the programmer from expressing
errors using a `Result` / `Either` like construction.  _The problem is typically not the language limitations, rather it is the freedoms._  
I can reply the arugment from my `null` section:

Any language that can express type variables and higher order functions, can express `Result` / `Either` like type:

```
result<E,T> = forall R . (E -> R, T -> R) -> R
```

## Exhibit 4: Libraries and Code Reuse

Code reuse has been a long standing goal of well written programs. 
Many believe that, at large, we have failed that goal.  Critics are pointing out 
that the code reuse does not work well on the project level.  But it does work when we use libraries!
Libraries is the mechanism with which we succeeded reusing the code.   
Why is this the case?

Here is a misplaced quote I can't locate who said it:

> _“A bad abstraction is much harder to maintain than duplicated code”_

As the library author you do not need to be concerned about code reuse.  Your code is being reused, that job is passed to others.
As the library user your freedom is very limited.  You have to do what the library requires you to do.
This is a perfect example of a limitation forced on programmers by the technology.  And it works.

There is no reason why the same level of code reuse could not be accomplished on the project level, other than the ton of
extra freedom developers have in deciding on how to reuse.


## Exhibit 5: Memory Bugs

This has been discussed a lot.  Here are some examples I pulled from the internet:  
[Microsoft eyes Rust](https://visualstudiomagazine.com/articles/2019/07/18/microsoft-eyes-rust.aspx)   
[Security bugs in Chrome](https://www.zdnet.com/article/chrome-70-of-all-security-bugs-are-memory-safety-issues/) 

And I want to just summarize what has been said:

*  Best programming practices are not good enough to avoid these problems
*  Partial solutions (like smart pointers in newer versions of C++) are not good enough either
*  Limiting what developers can do: works

Limiting what developer can do means:  no memory management (garbage collection handles it),  
no access to direct pointer programming, or type safety over memory and pointer use (linear types, Rust).   
The pattern repeats.

### Exhibit 6: Hard-coding gets the job done

This is an intersting example because it is virtually impossible for the technology to limit the use of Strings.
How to do limit the convenience of hardcoding email address or deployment specific configuration and force people
to create configuration files, parse them, handle what happens when they do not exist...

This gets even more interesting. In maintstream language any piece of code can read a configuration file.  
There is some work to do that but it is not unsurmountable.  
Reading a file is a non-referenially transparent side-effect and functional language will not let you do it that easily.  
Getting access to configuration data could require a bigger refactoring than in a maintream language. 

This is an amplified version of what we have examined so far:  Not only the technology does not prevent developer from 
doing wrong, it makes it hard to do right and easy to do wrong.  

I have seen a measurable increase in hard-coding since I moved to work on Haskell code bases.  
This also shows how complex the overall situation is. Technology can prevent
developers from certain errors, but piling up the restrictions it can make doing the right things harder too.


## Making Sense of the Dystopia

### Type Safety

Lots of developers think about type safety as a way to prevent accidental typos or maybe preventing using a String instead of an Int.  
I am not talking about such litte restrictions.

Im my current work, many apps, have just a few lines of code we call top level.  These lines can do almost anything
but there is a very few of them.  The rest can only do what is allowed.  For example: can call another micro-service
only it that service is listed in the app effect list, can only use environment variable if that variable is listed in the type level list
of allowed names...   

There is almost no limit to how far you can go with enforcing restrictions.   
One of my favorite examples is a `C` style `printf` where the data you can use is
restricted by the format.  E.g. 

```
printf "%d greetings from %s" n name
```
will compile only if `n` is a number and `name` is a String. 

We are talking some serious vulantary diet here!  Some developers consider strongly typed environments to be superior, some do not.
If 10% of what I wrote so far is correct than type safety is should be considered more than it is. 
The common theme of this post was: restrictions and limits are the only effective tool in reducing maintenance.
Static type systems are the most powerful way of imposing limitations on the program.

There are some caviats.  (1) Only some restrictions are good. Types will not prevent you from making bad or costy design choices. You can use types to define the original JavaScript Ajax API or something worse. 
A bad example to think about is a communcation between micorservices that pevents the program from returning error information.
(2) It would be very hard to use types to prevent some expensive practices like hardcoding, it will be impossible to use types to enforce meaningful error messages, logger messages, etc.

Types can make cutting corners harder but are not a replacement for the old school diligence. 
Types are not the panacea, they could still be a big part of the solution.


## Constraining Language Design?

On the surface JS callback hell looks very similar to the `null` problem.  People agree it is bad, Continuation Passing Style has been known for ages... The issue here is that a clean implementation of CPS would have been very hard on top of what JavaScript offered before ES6.  
It would be not fair to criticizes JavaScript programmers for Callback Hell because of how the original Ajax API looked like.
Not all technology limitations are good,  this one gave us a gun already pointed at the foot.  

The way I think about it that the mainstream language designers and library designers are not that different from the rest of us. 
They still need boundaries otherwise they will mess things up.  What would be the limitation for a language designer?
It cannot be technology it has to be something else.  Functional Programming languages derive their limitations from formalism.
PLT (Programming Lanaguage Theory) is very mathematical.  Not sure what that is for Java, ES6, or C++ committee.  
Sure, there are language specific goals (like performance for C++), but these do nothing for the maintenance cost. 

There is also the issue of expressiveness or Turing Completeness.  It is actually desired for the language to not be able to not do 
everything?  A good case study for this is the current K8 and mico-service craze. 
10K+ in yaml configuration, 3K+ dockerfile lines are not unusual even for mid size deployments.  These are monstrous amounts of configuration. These as programs that need to be maintained and will be a part of a long term cost of software products.  It is very possible that the wind is going to blow this away and we will return to monoliths or other completely different architecture and deployment environment.  Until then this is a concern.  yaml and Helm Charts are a good example of an approach that does not try to constrain much.
_dhall_ programming language is an example of limiting things in an opinionated way to create a special purpose language that can't do everything but is very good at configuration.  _nix.dev_ is another good examples of using a non-Turing complete language
for in the devops space.  

Semantic Functions???

These all seem like excellent ideas and will help improve the situation of a small <1% of software projects.

### The Industry, The Managers, The Bottom Line

It is not realistic to expect that developers will deliver a low maintenace code if the management cares about short term cost and short term goals. One and probably only hope seems to be the restrictions imposed by the management approved technology.

For many projects, the maintenance cost is passed to the customers. 
Such cost structure matches the process that emphasizes delivery timelines over quality.
Even if the customers do no pay for the maintenance, the cost is overshadowed by short term delivery goals. 
In many places, the time to market pressure tends to be the driving factor. 

Another big part of the issue is the corporate inertia.  Nobody gets fired for not changing the old process or for selecting 'proven' old tools, even if the old process and the old tools do not work well. Programmers do not have decision power.  MBAs are not competent to make
technical decisions and will resist change and prefer known evil. 

For example, I know a whole industry segment which produces a lot of data outputs with high accountability and correctness requirements. Their standard practice is called "double programming" (2 indepedently programmed data output derivations need to agree) but, believe it or not, programmers do not use any version control!  Something like git is not a part of their technology stack.  Version control is not what any of these companies do and hence none feels any pressure to change.

Software Development suffers from an acute case "temporal mico-economics":  Seemingly optimal short term choices are costly to the project in the long term.  In this Adam Smith's model of programming, the software developer takes the shortest / easiest / KISS path to accomplish the next incremental goal.  At long term, this is far from shortest or simplest or maintainable.   
This reminds me of a modified [tangram](https://en.wikipedia.org/wiki/Tangram) game where each piece is presented to the player one at the time and the goal is to create the simplest possible design without moving previously placed pieces.  This does not work!

That make sound like criticism of agile development.  I think this has much deeper roots than the process. 
It is also something that refactoring is unlikely to completely solve. 
I will return to this topic in the Agile section.

### Perpetual Immaturity, Education, Anti-Math Attitude

I think this observation came from Robert C. Martin (Uncle Bob):  The number of programmers doubles every 5 years.
Do the calculation.  Assuming nobody ever retires or leaves, that makes for 1/2 of the workforce with less than 5 years of experience.
This profession is considered to be the domain of young.  

It is hard not to notice the anti-theoretical attitude in the industry.  Mathematics has place in algorithms or data science but mathematical properties of computations?  How dare you! 
I blame the education.  Mathematics is now tought by memorizing formulas, not learning formal deduction methods.  
Mathematics as current tought has nothing to do with mathematics.

A short quiz:  What does this mathematical formula have to do with programming:

a ^ (b ^ c) = a ^ (b * c)

How about this one:

a ^ (b + c) = a ^ b * a ^ c

If you know the answer, where did you learn it?  In school, in university, yourself?  I have not learned it in school or in university.  

Here is another evidence from my personal life:  At some point to improve my marketability I decided to remove my PhD (mathematics) from my resume.
It was something that was suggested by my friend and worked very well.  I felt bad about doing it, it felt like cheating.  So I 
disclosed my education after the interview with the something like 
"Please do not hold it against me, as you can see I program, fix bugs, ...".   
I have heard more then one conversation criticizing someone's decision of pursuing a PhD.  
Flipping the coin.  It feels like the community is discriminating against knowledge.  That cannot be beneficial!

The way to solve the current dystopia would be to acknowlege that writing code is not easy.  That it takes significant time and effort to learn how to do it decently and that it requires a lot of continous learning. It requires linear learning that goes beyond stack-overflow or Quora.  Learning that includes textbooks. Learning concepts that make take longer than few days, few months, event few years to grasp. 
Learning needs to become a part of the developer's job.  

?? It should not be unusual for a developer to know that types form an algebra, what is structural induction, strong normalization, what is a Functor or a natural tranformation, or what proofs have to do with types. ??

This probably will not happen.

### Popularity

The technical decisions are either made my the management or are subject to some form of democracy where developers make a group 
decision.  On a large scale this becomes a popularity contest. People read TOBIE index, RedMonk index ...
This problem is not unique to software and has been an issue with humanity, it is older than Socrates' death.  
Why do we think that popular must be good?  I guess because it is popular to think that.  There is no other reason.  
There are strong arguments for thinking the opposite.  If it is popular, there is something wrong with it.

### Agile

Agile projects vs linear learning
Learning within projects does not work.
Ideal : incremental project development combined with linear continous learning. Learning is part of develper's job.

### KISS, MVP

### Can't Pull Request the World

In several encounters on reddit I have been confronted like this:

"if you think this is error prone, just don't use it, here how you do that instead: ..."

This assumes that  
(1) The code is in my control, I wrote it or have access to fix it.
(2) I have an unlimited time to fix all the issues in the code I am maintaining.

None of these is obviously true, and not just for me.  Identifying things that create maintenance issues has to be 
community driven effort.  




-- 

Similarly to Java `Optional`, JavaScript Promises are not ideal. Fortunately, it is possible to clean them up. There are some interesting alternatives that could be 
offered as a library.  For example: 

https://github.com/dmitriz/cpsfy

The interesting question how popular such libraries will be?  
My prediction is that they will not be used much.  Conclusion is ...
Expresiveness and Turing completness. Add yaml here.

This reminds me of a modified [tangram](https://en.wikipedia.org/wiki/Tangram) game where each piece is served one at the time
and the goal is to create the simplest possible design without moving previously placed pieces.  This does not work!

### yaml, Helm Charts, and Friends

Yaml is my last exhibit.  That reminds me of a joke I heard many years ago, it was translated from Chech and uses boy's first name _Izik_
I may have messed it a bit but it conveys the message.

_After the delivery, the new dad is told he can now see his wife and his new baby Izik.  
He enters a big room with sign "Well behaved babies".  There is no sign of his wife or his son.
The next room has a sign "Babies that cry a lot", no sign of Izik or his wife.
The next room sign says "Babies that cry all the time",  no sign of Izik either.
Next room sign: "Babies that cry and throw sharp things at you", no sign of Izik.
The last room has a single door and a sign: "Izik"_

Jokes aside, the industry is facing programming challenge in a completely new area. 
K8 deployments, micro-services... I expect 10K+ yaml code lines, 3K+ dockerfile lines to be not unusual even for mid size deployments.  These are monstrous amounts of configuration. These as programs that need to be maintained and will be a part of a long term cost of software products.  It is very possible that the wind is going to blow this away and we will return to monoliths or other completely different deployment environments.  Until then this is a concern.

Yaml is close to being unstructured JavaScript that can get unruly in much less than 10K lines.
Helm Charts templates provide a mustache-like control flow abilities to program on yaml files. 
It is a JavaScript like take on the problem. It does not try to restrict yaml, rather it adds expensiveness to it. 
Time will tell how hard to maintain this will get.

Copare this to using `dhall`.  Restrictive configuration language that is not Turing complete. 

