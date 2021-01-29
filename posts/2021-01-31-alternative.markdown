---
title: A Pessimist's Guide to the Alternative Typeclass
author: Robert Peszek
featured: true
summary: Error information loss with Alternative and an Alternative instance that cares
toc: true
tags: Haskell, code-correctness, code-maintenance
---

Code for this project can be found in my [_add_blank_target experiments](https://github.com/rpeszek/experiments) repo ([_add_blank_target alternative](https://github.com/rpeszek/experiments/tree/master/alternative) folder).  
This is my second post dedicated to the _error information loss_ in Haskell (first was [Maybe Overuse](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html)).  

## Nutshell
The `Alternative` typeclass is a very powerful tool in the FP toolbox.  `Alternative` instances are also known for confusing or missing error outputs. Is there a decent `Alternative` that cares about errors? 

Thinking about _the glass being half empty or half full_, look at this computation:
`a <|> b`
and assume that `a` fails and `b` succeeds.   
_A half empty glass_ makes us think about the failure of `a`:  
_Why_ did `a` fail?   
Would it be better if _some_ `a` failures caused the whole computation to fail?...   
_A half full glass_ makes us ignore the failure and focus on `b`...  this is exactly the behavior of `<|>`.   

My goal is to consider `Alternative` from the point of view of _error information loss_. This viewpoint yields an interesting prospective on the use of `Alternative` and on its limitations.  
My second goal is to present a useful instance that is missing in the standard library and, probably, on Hackage.  This `Alternative` instance is (pessimistically) constructed to preserve failure information.

I am using the term _error_ colloquially, the correct term is _exception_.  _Exception information loss_ just does not have a ring to it. 


## Pessimist's Intro to `Alternative`

``` haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

_Optimist, First Look_:

*  `empty` typically represents a failed computation
*  `(<|>)` combines 2 computations returning the first successful result. I like to think about it as right-catch semantics.

As we know, `MonadPlus` provides a similar semantics for monads.  `Alternative` and `MonadPlus` are most commonly used with parsers.  You are likely to use it with _aeson_, _parsec_ / _megaparsec_, _attoparsec_, etc.  
I this post the focus is `Alternative` and the examples use _attoparsec_.  

_Pessimist, First Look_:

* `empty` does not have any error information.  It represents a failure of some unknown reason.  
   I consider this problematic and an oversimplification.   
   Unless I can somehow introduce a meaningful zero-information (let me call it _noOp_) failure, this will bite.
* `(<|>)` semantics is unclear about error information. In particular, this definition prevents any typeclass inductive programming that does interesting things with errors.  
   
With a _true_ `Alternative`, the typically observed behavior is: If all alternatives fail, then the error information comes from the last tried computation.  

I am not that deeply familiar with GHC internals.  However, GHC compiler often behaves in a very similar way.  For example, GHC message could indicate a problem with unifying types, it may suggest that my function needs to be applied to more arguments, ... while the real issue is that the code is missing a typeclass instance somewhere or something else is happening that is completely not related to the error message.  Whatever the reason for this behavior is,  all Haskell developers get thrown for a loop from time to time.  
Using `Alternative`, we are likely to do the same to out users. 


**Side-Note:**  Error information that comes from the use of `(<|>)` can be much better with _not true_ `Alternative`-s.  
_parsec_ and _megaparsec_ packages implemented sophisticated ways to provide better error messages by looking at things like longest parsed path.  This functionality is related to the lack of automatic backtracking in _(maga)parsec_.  
Lack of backtracking is what makes the _(maga)parsec_ `Parser` not a true _Alternative_ (it violates Alternative laws).  
Arguably, having no automatic backtracking makes writing code hard and error prone.  There appears to be an interesting trade-off: _good error message_ vs _Alternative trueness and easier code_.    
A great, related, reading is: [_add_blank_target Parsec: “try a <|> b” considered harmful](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/).

The definition of `Alternative` begs this question:  Why the `Applicative` superclass?  As far as I know this is because of the intended use of `empty` and `<|>`.  We use them in the _Applicative_ context.  More on this later.

## Alternative Laws, Pessimistically

The required laws (copied from [_add_blank_target Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Laws_6), see also [_add_blank_target Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#Alternative_and_MonadPlus_laws)) are:

``` haskell
empty <|> u  =  u    -- (1)
u <|> empty  =  u    -- (2)
u <|> (v <|> w)  =  (u <|> v) <|> w  -- (3)
```

Note that these laws do not link `Applicative` and `Alternative` in any way.
That happens in the following, optional, set of laws: 

``` haskell
f <*> empty = empty                        -- (4) Rigth Zero
(a <|> b) <*> c = (a <*> c) <|> (b <*> c)  -- (5) Left Distribution
a <*> (b <|> c) = (a <*> b) <|> (a <*> c)  -- (6) Right Distribution
(pure a) <|> x = pure a                    -- (7) Left catch
```
For example, writing a parser you may decide to use one of these approaches:
``` haskell
p1 = Employee <$> employeeIdParser <*> (nameParser1 <|> nameParser2)
p2 = (Employee <$> employeeIdParser <*> nameParser1) <|> (Employee <$> employeeIdParser <*> nameParser2)
```
it is good to know if these approaches are equivalent.  
Any instance of `Alternative` that tries to accumulate failures is likely to have problem satisfying the distribution laws (5,6), as the _rhs_ combines 4 potential failures and _lhs_ combines 3.   
The question is: would you expect (5,6) to hold in the context of error information (e.g. _(mega)parsec_ error messages)?
My answer is: I do not!  
The end result is that the programmer needs to make an explicit choice between `p1` and `p2` selecting one with a more desirable error output.   
I think, that is OK.

_Pessimist Concerns_:   

*  `empty` typically represents a failure. (4) is problematic if you want to have other possible failures (e.g. failures with different error messages):  
    `otherFailure <*> empty` is likely to be `otherFailure` not `empty`.  
*  _(1 or 2)_ and _(4)_ prevent expressing the concept of a critical failure  
    here, `f` is a critical failure if `f <|> a = f` for any `a` and `b <|> f = f` for any non-failing `b`,   
    (i.e. terminate `<|>` with ability to recover outside of `<|>.`)   
   `empty` cannot represent a critical failure because the first requirement is prevented by _(1)_, the second by _(2)_.   
    non-`empty` cannot represent a critical failure because of (4).
*  _(5,6)_ are likely to prevent `<|>` semantics that accumulates error information

Let me return to the basic laws, particularly _(2)_: `u <|> empty  =  u`. The issue I am about to demonstrate is not just specific to parsers:

``` haskell
import qualified Data.Attoparsec.ByteString as A 

testSuccess :: A.Parser a -> A.Result a
testSuccess p = A.parse p "foo"

testFail :: A.Parser a -> A.Result a
testFail p = A.parse p "bar"

u = A.string "foo"

lhs = u <|> empty
rhs = u
```

Here are the results:

``` haskell
-- >>> testFail lhs
-- Fail "bar" [] "Failed reading: empty"

-- >>>  testFail rhs
-- Fail "bar" [] "string"
```
So we broke the required second law!  Incidentally, we would not be able to break the law using `testSuccess`. 

One way to look at this, and I believe this is how people are looking at this issue, is that any failure with any error message is considered equivalent to `empty`.  The laws hold if error information is ignored.  Somewhat of a downer if you care about the error information.

Breaking (4 - Rigth Zero) is left as an exercise. 


## Real-World `Alternative` (Optimism with Experience)

Here are some examples of problems arising from the use of `Alternative` semantics

### Failure at the end

Laws are important, functional programmers use laws (sometimes even subconsciously) when thinking about, implementing, or designing the code.  
The second law tells us that we can slap a computation that always errors out at the end without messing things up.  

Consider this (a slightly adjusted real-world) situation: your app needs to talk to an external website which can decide to do A, B, or C and will reply with A, B, or C json message.  Based on what happened, your app will need to do different things.  You need to parse the reply to know how to proceed.  
The good news is that only A and B are needed in the short term, C can wait. For now, you are only required to tell the user when C happens.   

This should be _aeson_ but I keep _attoparsec_ for consistency:

``` haskell
parseReply = parseA <|> parseB <|> parseC 
  where
    parseA = ...
    parseB = ...
    parseC = fail "C is not supported yet!"
```

You can improve your errors with `A.<?>` but, in this case, not that much.

The external website changed how they report A, now when A is processed, the user sees: "C is not supported yet!".

Arguably this would be a slightly better code, the user would see the B parsing error message instead ;) :

``` haskell
parseReply = parseC <|> parseA <|> parseB 
```

The other design risk is thinking about the second law as 'stable': We will not disturb the computation too much if we append (add at the end of the `<|>` chain) a very restrictive parser that fails most of the time.  This, obviously, suffers from the same problem.  

IMO, maintainable code should avoid including restrictive computations at the end of `<|>` chains.  

### Permissive computation at the end

This is the example I started with. Consider code like this:

``` haskell
specificComputation <|> bestEffortComputation
``` 

The specs may change and you will never learn that `specificComputation` no longer works because `bestEffortComputation` effectively hides the issue.  This is the behavior of all the pure instances of `Alternative` I know about.
This is _how Alternative_ works.  (... or is it? See next section)  

IMO, maintainable code using `<|>` should also avoid including "catch all" elements.    
So restrictive at the end is bad, permissive is bad

> _In fact, it's better if you don't speak at all, Peregrin Took._

`Alternative` makes it super easy to write code,  it does not make it easy to maintain it.

## Missing Instances

It would be ideal if all typeclasses, in the _base_ package, that have something to do with failures (e.g. `Alternative`, `MonadFail`) came with at least one instance allowing to recover the error information.   
This is not the case for `MonadFail` (especially when combined with `MonadPlus`: [_add_blank_target Monoid Overuse - MonadFail](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html#monadfail-and-maybe)).  
And, as we have seen in previous section, this is not the case for `Alternative`.  

Can we come up with `Alternative` instances that do a decent job of maintaining error information?  It seems, the answer is yes.  

### `Either [e] a`

This is a warm-up.

Something very similar already exists, e.g. in the [_add_blank_target _either_](https://hackage.haskell.org/package/either) package but with a non-standard `Applicative` instance.  I am using standard `Either` `Monad` and this is really
a `MonadPlus` (with a somewhat questionable right-zero law): 

``` haskell
instance Monoid e => Alternative (Either e) where 
    empty  = Left mempty
    Left e1 <|> Left e2 = Left $ e1 <> e2
    (Left _) <|> r = r
    r <|> _ = r
```
_(Note, transformers package has an obscure instance in the deprecated `Control.Monad.Trans.Error` module that conflicts with the above instance,  in a real code this would need a `newtype` to avoid the conflict)_   
The [_add_blank_target _either_](https://hackage.haskell.org/package/either)'s package `Validate` type uses the same `Alternative` code but with different non-monadic `Applicative` definition that also accumulates errors.  I believe both approaches have value.

The required _(1-3)_ laws are satisfied without resorting to any sort of questionable reasoning 
that treats all errors as `empty`.  Also, `empty` represents a _noOp failure_ computation. This is exactly what I wanted.

Optional _(4 Right Zero)_ law (`f <*> empty = empty`) is questionable (consider `f = Left e` with a non-trivial `e`).  
_(7 Left Catch)_ is OK.   
As we have predicted, the distribution laws are not satisfied.  
_(5)_ is NOT satisfied:

``` haskell
(f <|> g) <*> a = (f <*> a) <|> (g <*> a) 
```
If `f` and `g` represent successful computation and `a`
is a list of errors then the _rhs_ has twice as many errors as the _lhs_.

_(6)_ is not satisfied either:
``` haskell
f <*> (b <|> c) = (f <*> b) <|> (f <*> c) 
```
If `f` represents a failed computation then _rhs_ will duplicate `f` errors.   
This looks like a bigger problem than it really is.  The _lhs_ and _rhs_ contain the same amount of error information.

Expressions like this one
``` haskell
employeP = 
   Employee 
   <$> idP
   <*> (nameP1 <|> nameP2)
   <*> deptP
   <*> (bossP1 <|> bossP2 <|> bossP3)  
```
could contain 1, 2 or 3 errors depending on which field fails to parse.  This is seems somewhat complex but, IMO, should not be a show stopper.

### A Decent `Alternative`: `Either [e] ([w], a)` 

What would really be nice, is to have a standard "Right-catch with warnings" `Alternative` instance (please let me know if you have seen it somewhere on Hackage):

``` haskell
newtype ErrWarn e w a = EW {runEW :: Either e (w, a)} deriving (Eq, Show, Functor)
  
instance (Monoid e) => Alternative (ErrWarn e e) where 
    empty  = EW $ Left mempty
    EW (Left e1) <|> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <|> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    r@(EW (Right _)) <|> _ = r
```    
 
This approach, when computing `a <|> b`, does not try to compute `b` if `a` succeeds.
Thus, this instance matches the common semantics of computing only up to the first success. The approach accumulates all errors encountered up to the point of the first success and returns them as warnings.  
This is a lawful `Alternative` (satisfies required laws _(1-3)_) and it does not rely on any questionable unification of `empty` and errors.  
But we made a bit of a mockery of things:  `Alternative` no longer returns just the first successful computation, it annotates the result with warnings.  I say, that is completely OK.

But wait! To have `Alternative` we need `Applicative`.
It is possible to implement `Applicative` for this type in more than one way, one even leads to a valid `Monad` and `MonadPlus` (with the right-zero caveat discussed above).   
That approach does not try to accumulate `e`-s only accumulates `w`-s:

``` haskell
instance (Monoid w) => Applicative (ErrWarn e w) where
    pure x = EW $ Right (mempty, x)
    EW (Left e) <*> _ = EW $ Left e
    EW (Right (u, f)) <*> EW (Right (v, x)) = EW (Right (u <> v, f x))
    EW (Right (u, f)) <*> EW (Left e)  = EW $ Left e

instance (Monoid w) => Monad (ErrWarn e w) where 
    EW (Left e) >>= _  = EW $ Left e
    EW (Right (u, x)) >>= k = 
        case k x of 
            EW (Right (v, b)) -> EW (Right (u <> v, b))
            EW (Left e) -> EW (Left e)

instance (Monoid e) => MonadPlus (ErrWarn e e)    
```
To spare you reading the code, `ErrWarn` combines standard `Either e` and `Monoid w => (w,)` _Applicative_ and  _Monad_ semantics. The composition of these functors remains a legal _Monad_.

This instance exhibits similar problems with matching the `<*>` semantics as the `Monoid e => Either e` instance from the previous section (i.e. (5,6) are not satisfied).

### Code Example

Here is a very convoluted (and arguably, not a very good) parsing code that is intended only to demonstrate `ErrWarn` benefits.
  
This code will parse _ByteStrings_ like "id last-first-name dept boss2"
to produce, if successful, a hard-coded: id, name, department, and boss name:

``` haskell
idP = 123 `onKeyword` "id"
nameP1 = "Smith John"  `onKeyword` "last-first-name"
nameP2 = fail "first-last-name not implemented yet"
deptP =  "Billing" `onKeyword` "dept"
bossP1 = "Jim K" `onKeyword` "boss1"     
bossP2 = "Kim J" `onKeyword` "boss2"    
bossP3 = pure "Mij K bosses everyone" 

onKeyword :: a -> B.ByteString -> AT.Parser B.ByteString a
onKeyword val key = const val <$> A.manyTill ACh.anyChar
                    (A.lookAhead $ A.string key)
                    A.<?> show key


data Employee = Employee {
    id :: Int
    , name :: String
    , dept  :: String
    , boss :: String
   } deriving Show

emplP :: A.Parser B.ByteString Employee
emplP = 
   Employee 
   <$> idP
   <*> (nameP1 <|> nameP2)
   <*> deptP
   <*> (bossP1 <|> bossP2 <|> bossP3) 

emplP' :: B.ByteString -> ErrWarn [String] [String] Employee
emplP' txt = 
   Employee 
   <$> ew idP 
   <*> (ew nameP1  <|> ew nameP2 )
   <*> ew deptP 
   <*> (ew bossP1  <|> ew bossP2 <|> ew bossP3)
   where
        ew p = cnvt p txt

        cnvt :: A.Parser a -> B.ByteString -> ErrWarn [String] [String] a
        cnvt p s = singleErr $ A.parseOnly p s

        singleErr :: Either e a -> ErrWarn [e] [e] a
        singleErr (Left e) = EW $ Left [e]
        singleErr (Right r) = EW $ Right ([], r)

```

Trying it with a good input:
``` haskell
-- >>> A.parseOnly emplP "id last-first-name dept boss1"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})

-- >>> emplP' "id last-first-name dept boss1"
-- EW {runEW = Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})}
```

[Failure at the end](#failure-at-the-end) situation (typo in _last-first-name_):
``` haskell
-- >>> A.parseOnly emplP "id last-firs-name dept boss2"
-- Left "Failed reading: first-last-name not implemented yet"

-- >>> runEW $ emplP' "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
```

[Permissive computation at the end](#permissive-computation-at-the-end) situation ("boss" parsing error):
``` haskell
-- >>> A.parseOnly emplP "id last-first-name dept boss"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})

-- >>>runEW $ emplP' "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
```

We are no longer being thrown for a loop!  
I probably should revise my recommendations for not using a restrictive computation at the end or a permissive computation at the end!

### It is not just about Either

My goal here is point out that the above _right-catch with warnings_ semantics is a decent principled computation.   
I think such semantics could find its way into parser internals.  
In particular, instead of using `Either e (w, a)` it is more convenient to use `newtype` around `r -> Either e (w, a)`.
Example prototype code is in my repo.

## Relevant work on Hackage

[_add_blank_target _free_](https://hackage.haskell.org/package/free) package contains a semantic (free) version of _Alternative_.
This is interesting.  However, I am starting to think that `Alternative` is just a wrong abstraction for dealing with failures.

[_add_blank_target _semigroupoids_](https://hackage.haskell.org/package/semigroupoids-5.3.5/docs/Data-Functor-Alt.html) offers a _Alt_ that is just a _Functor_ and does not need to have `empty`.  

A list of interesting packages that implement `Monoid`-like semantics for `Applicative` (most also implement `Alternative`) to accumulate errors provided by [_add_blank_target u/affinehyperplane](https://www.reddit.com/user/affinehyperplane/) on 
[_add_blank_target reddit](https://www.reddit.com/r/haskell/comments/kyo4xk/maybe_considered_harmful/gji7fmx?utm_source=share&utm_medium=web2x&context=3):

[_add_blank_target _either_](https://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Validation.html)   
[_add_blank_target _validation_](https://hackage.haskell.org/package/validation-1.1/docs/Data-Validation.html)  
[_add_blank_target _validation-selective_](https://hackage.haskell.org/package/validation-selective-0.1.0.0/docs/Validation.html)  
[_add_blank_target _monad-validate_](https://hackage.haskell.org/package/monad-validate-1.2.0.0/docs/Control-Monad-Validate.html) provides an interesting 
(not completely lawful) validation _Monad_ transformer that can accumulate errors, it does not implement `Alternative`

This list is not complete.  Please let me know if you see a relevant work elsewhere.

## Conclusions, Thoughts

Problems with a questionable handling of failures is not restricted to the `Alternative` typeclass. 
The reasons why this is the case must be very complex.
The list I assembled in my [_add_blank_target Maybe Overuse](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html#why-maybe-is-overused-possible-explanations) post seems relevant here as well.

The Pessimist theme was partially inspired by these two concepts: 
[_add_blank_target _Negativity Bias_](https://en.wikipedia.org/wiki/Negativity_bias) and, its opposite, the [_add_blank_target _Positivity Bias_](https://link.springer.com/referenceworkentry/10.1007%2F978-94-007-0753-5_2219#:~:text=Definition,favor%20positive%20information%20in%20reasoning.) are are psychological notions that, I think, have deep relevance to the programming in general.   
_Negativity Bias_ seems both rare and beneficial to the software programming and to the software design.   
_Negativity Bias_ comes with a _tendency to favor negative information in reasoning_ and will make you think about "rainy day scenarios", error handling, error information.   
_Positivity Bias_ comes with a _tendency to favor positive information in reasoning_ and will make you think about "happy path" and "sunny day scenarios".   
I think we should embrace some form of _pessimism_ and put in on the pedestal next to the principled design.  

Is `Alternative` a wrong abstraction for what it is trying to do? 
I think it is.  
IMO any abstraction intended for handling failures should include failures in its semantics.   
A typical `Alternative` instance dumps all failures into one bucket because the typeclass lacks any typing that is about failures.     

I hope this continues the discussion about _error information loss_ in Haskell.   
My particular interest is in discussing:

*   is `ErrWarn` somewhere on Hackage and I did not see it
*   other interesting `Alternative` instances that care about errors
*   your views on pessimism in programming
*   your views on error information loss in Haskell code bases
*   obviously, anything that I got wrong

reddit discussion (TODO)  
github discuss (TODO)

Thank you for reading! 

