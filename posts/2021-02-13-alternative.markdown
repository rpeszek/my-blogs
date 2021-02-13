---
title: Is Alternative a Wrong Abstraction for Handling Failures?
author: Robert Peszek
featured: true
summary: Rethinking Alternative and its instances 
toc: true
tags: Haskell, maintainability, correctness, general_functional_programming
---
**_subtitle:_ A Constructive ~~Criticism~~ Pessimism about the Alternative Typeclass**

Code for this project can be found in my [_add_blank_target experiments](https://github.com/rpeszek/experiments) repo ([_add_blank_target alternative](https://github.com/rpeszek/experiments/tree/master/alternative) folder).  
This is my second post dedicated to the _error information loss_ in Haskell (the first was about [_add_blank_target Maybe Overuse](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html)).  

## Nutshell
`Alternative` is a popular functional programming concept and the name of a frequently used Haskell typeclass. `Alternative` helps in writing elegant, concise code. `Alternative` instances are also known for producing confusing errors. 

I realized that there is an interesting connection between `Alternative` and optimism:    
Thinking about _the glass being half empty or half full_, look at this computation:
`a <|> b`
and assume that `a` fails and `b` succeeds.   
_A half empty glass_ makes us think about the failure of `a`:  
_Why_ did `a` fail?   
Would it not be better if some `a` failures caused the whole computation to fail?...   
_A half full glass_ makes us ignore the failure and focus on `b`...  this is exactly the semantics of `<|>`.    
A half full glass is not what you always want.

My goal is to consider `Alternative` instances from the point of view of the errors. This "pessimistic" viewpoint yields an interesting prospective on the use of `Alternative` and on its limitations.   
My second goal is to show a useful instance that is missing in the standard library and is different from the instances I have found in Hackage.  This instances is pessimistically constructed to preserve the failure information.    
My third goal is to briefly consider a possibility of rethinking the `Alternative` typeclass itself.

I am using the term _error_ colloquially, the correct term is _exception_.  _Exception information loss_ just does not have a ring to it. 


## Pessimist's Intro to `Alternative`

``` haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a] -- optional
  many :: f a -> f [a] -- optional
```

_Optimist, First Look_:

*  `empty` typically represents a failed computation
*  `(<|>)` combines 2 alternatives returning one that is successful. The most common approach is to return the left-to-right first success. This is called left-bias, I like to think about it as _right-catch semantics_.
*  `some` and `many` run the computation until first failure and return the successful results, `some` expects at least one success, otherwise it will fail. `some` and `many` are a nod towards parsers or other computations that change state. `some` and `many` are likely to yield bottom (e.g.
 `many (Just 1)` does not terminate). 

`Alternative` is the `Monoid` for the `* -> *` types,  `empty` representing `mempty` and `<|>` representing `mappend`. 
This equivalence is "witnessed" by the [_add_blank_target `Data.Monoid.Alt`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Alt) monoid instance.  The left-bias semantics is equivalent to the [_add_blank_target `Data.Monoid.First`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:First.) monoid.

The definition of `Alternative` begs this question:  Why the `Applicative` superclass?  As far as I know this is because the intended use of `empty` and `<|>` is in the applicative context.  More on this later.

As we know, `MonadPlus` provides a similar semantics for monads.  `Alternative` and `MonadPlus` are most commonly used with parsers.  You are likely to use it with _aeson_, _parsec_ / _megaparsec_, _attoparsec_, etc.  
In this post the focus is the `Alternative` with the typical right-catching, left-biased `<|>` and the examples use _attoparsec_.  


_Pessimist, First Look_:

* `empty` does not accept any error information.  It represents a failure of some unknown reason.  
   I consider this problematic and an oversimplification.   
   Unless we introduce a zero-information (let me call it _noOp_) failure, this probably will bite.   
   I will leave it to you to ponder philosophical questions about _noOp_ (e.g. `Left []`) failure.    
   What does: nothing went wrong but the computation failed mean?  
* `(<|>)` semantics is unclear about error information. 
*  `some` and `many` provide no error information about the failure that ended the production of the result list. Moving forward, I will not discuss `some` or `many` in this post.  

With a _true_ `Alternative` instance, a somewhat popular behavior is: If all alternatives fail, then the error information comes from the last tried computation.  

I am not that deeply familiar with GHC internals.  However, as a black box, the GHC compiler often behaves in a very similar way.  For example, GHC message could indicate a problem with unifying types; it may suggest that my function needs to be applied to more arguments; ... while the real issue is that I am missing a typeclass instance somewhere or something else is happening that is completely not related to the error message.  From time to time, GHC will throw Haskell developers for a loop.  
With many of the `Alternative` instances, we are likely to do the same to out users. 


**Side-Note:** 
_parsec_ and _megaparsec_ packages implemented sophisticated ways to provide better error messages by looking at things like longest parsed path.  Lack of backtracking is what makes the _(maga)parsec_ `Parser` not a lawful _Alternative_.  The _megaparsec_ haddock suggest adding `try` to improve the lawfulness.  Adding `try` can mess up error messages. There appears to be an interesting pragmatic trade-off: _good error messages_ vs _more lawful Alternative_.    
A great, related, reading is: [_add_blank_target Parsec: “try a <|> b” considered harmful](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/).   

Most parsers offer very similar combinators allowing to write a parser agnostic code.  This scary quote is not something I have came up with (I consider good error messages in deployed production code to be crucial): 

> _The trick is to use the `parsers` library, which lets you switch out parsing backends. You can prototype with the `trifecta` library (which has good error messages) and then switch to `attoparsec` when you're done_

So, this is clearly a bit of a mess and we are looking for crazy workarounds. I will delve deeper into alternative error outputs by looking at the alternative laws next.



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
For example, when writing a parser you may decide to use one of these approaches:
``` haskell
p1 = Employee <$> employeeIdParser <*> (nameParser1 <|> nameParser2)
p2 = (Employee <$> employeeIdParser <*> nameParser1) <|> (Employee <$> employeeIdParser <*> nameParser2)
```
it is good to know that these approaches are equivalent.  

Note that any instance of `Alternative` that tries to accumulate failures is likely to have a problem satisfying the distribution laws _(5,6)_, as the _rhs_ combines 4 potential failures and _lhs_ combines 3.   
The question is: would you expect _(5,6)_ to hold in the context of a failure (e.g. _(mega)parsec_ error messages)?
My answer is: I do not!  
The end result is that the programmer needs to make an explicit choice between `p1` and `p2` selecting one with the more desirable error output.   
I think this is OK.  The trade-off is similar to one made by the _monad_validate_ package linked at the end of this article.



_Pessimist's Concerns_:   

*   `empty` typically represents a failure. _(4)_ is problematic if you want to have other possible failures (e.g. failures with different error messages):  
    `otherFailure <*> empty` is likely to be `otherFailure` not `empty`.   
    _(1 - 3)_ force a monoidal structure on the failure type.  That seems to be overly restrictive 
    and questionable (e.g. what is `empty` error?). A semigroup structure would make much more sense here.  
*   The laws actually prevent me from defining alternatives that do interesting things with errors.   
    For example, _(1)_ and _(4)_ prevent expressing the concept of a critical failure   
    a sane definition would be: `f` is a critical failure if `f <|> a = f` for any `a`,   
    (i.e. terminate `<|>` with ability to recover outside of `<|>`.)   
   `empty` cannot represent a critical failure because of _(1)_   
    non-`empty` cannot represent a critical failure because of _(4)_
*  _(5,6)_ are likely to prevent `<|>` semantics that accumulates error information (as discussed above)

Let me return to the basic laws, particularly _(2)_: `u <|> empty  =  u`: 

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
So we broke the required second law.  Incidentally, we would not be able to break this law using `testSuccess`.   
This should not be surprising, (1-3) imply a monoidal structure on failures and this is not what _attoparsec_ does.


_attoparsec_ gets a lot of blame for its error output.  Let's try `IO` alternative:
``` haskell
uIO = fail "foo" :: IO a

lhsIO = uIO <|> empty
rhsIO = uIO
```

ghci:

``` haskell
>>> lhsIO
*** Exception: user error (mzero)
>>> rhsIO
*** Exception: user error (foo)
```
We see the same issue.

One way to look at this, and I believe this is how people are looking at this issue, is that any failure with any error message is considered equivalent to `empty`.  The laws hold if the error information is ignored.  Somewhat of a downer if you care about errors.   

**Side-Note:**  Numerous instances of `Alternative` manage to satisfy _(2)_.  
That includes `ExceptT`, the `Validatation` type listed at the end of this post.  Here is the law working for 'trifecta'

``` haskell
import Text.Trifecta
u = string "foo"

lhs = parseTest (u <|> empty) "bar" :: IO ()
rhs = parseTest u "bar" :: IO ()
```
ghci:
``` haskell
>>> lhs
(interactive):1:1: error: expected: "foo"
1 | bar<EOF> 
  | ^        
>>> rhs
(interactive):1:1: error: expected: "foo"
1 | bar<EOF> 
  | ^    
```

We will see the `Monoid` fix for _(2)_ in the [`Either [e] a`](#either-e-a) section.  

## Real-World `Alternative` (Optimism with Experience)

Here are some examples of problems arising from the use of `Alternative` semantics

### Failure at the end

Laws are important, functional programmers use laws (sometimes even subconsciously) when thinking about, implementing, or designing the code.  The second law tells us that we can slap a computation that always errors out at the end without messing things up.  

Consider this (a slightly adjusted real-world) situation: your app needs to talk to an external website which can decide to do A, B, or C and will reply with A, B, or C json message.  Based on what happened, your app will need to do different things.  You need to parse the reply to know how to proceed.  
The good news is that only A and B are needed in the short term, C can wait. For now, you are only required to tell the user when C happens.   

This should be _aeson_ but I keep _attoparsec_ for consistency (the behavior is the same):

``` haskell
parseReply = parseA <|> parseB <|> parseC 
  where
    parseA = ...
    parseB = ...
    parseC = fail "C is not supported yet!"
```

The external website changed how they report A, now when A is processed `parseA` fails, the user sees: "C is not supported yet!".

The following would be a slightly better code, the user would see `parseB` error message instead ;) :

``` haskell
parseReply = parseC <|> parseA <|> parseB 
```
_(sigh)_

One way out is to parse A, B, and C separately and handle the results (and the parsing errors) outside of the `Parser`.     

The other design risk is in thinking about the second law as 'stable': We will not disturb the computation too much if we append (add at the end of the `<|>` chain) a very restrictive parser that fails most of the time.  
An example would be fixing an existing parser `p` with a missed corner case parser, `p <|> cornerCaseP`.
Errors from `p` are now almost not visible.  

So would `cornerCaseP <|> p` be a better solution?  Next section covers that case.

### Permissive computation at the end

This is the example I started with. Consider code like this:

``` haskell
specificComputation <|> bestEffortComputation
``` 

The specs may change and you will never learn that `specificComputation` no longer works because `bestEffortComputation` effectively hides the issue.  

The way out is to run `specificComputation` and `bestEffortComputation` separately and handle results (e.g. parsing errors if the computation is a parser) outside, or come up with a different way of not using the alternative.   

_Failure at the end_ situation improves a bit with certain (other than _aeson_ or _attoparsec_) alternatives, _Permissive computation at the end_ does not seem to 
have a good available solution.

## Pessimistic Instances

It would be ideal if the following property was true:

_If a typeclass A is defined in the **base** package and A has something to do with failures, then
there exist at least one instance of _A_ in the **base** allowing to recover the error information_

`MonadFail` fails this property (especially when combined with `MonadPlus`: [_add_blank_target Maybe Overuse - MonadFail](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html#monadfail-and-maybe)).  

`Alternative` fails it as well.  

Can we come up with `Alternative` instances that do a decent job of maintaining error information?  It seems that the answer is yes.  

### `Either [e] a`

This is a warm-up.

This instance is not new. It matches `ExceptT` alternative instance from _transformers_ / _mtl_.
It uses standard `Either` monad and this is a `MonadPlus` (with a somewhat questionable right-zero law): 

``` haskell
instance Monoid e => Alternative (Either e) where 
    empty  = Left mempty
    Left e1 <|> Left e2 = Left $ e1 <> e2
    (Left _) <|> r = r
    r <|> _ = r
```
_(Note: transformers package has a conflicting instance in the deprecated `Control.Monad.Trans.Error` module,  in a real code a `newtype` would be needed to avoid this conflict)_   

I have included `Monoid e => Alternative (Either e)` instance as a warm-up and to discuss the laws.

**Laws:**  
The required _(1-3)_ laws are satisfied without resorting to any sort of questionable reasoning 
that treats all errors as `empty`. However, `empty` represents a _noOp failure_ computation (somewhat questionable meaning).

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

So, overall, `Either [e] a` has done quite well as an alternative!

### A Decent Blueprint: `Either [e] ([e], a)` 

What would really be nice, is to have a standard "right-catch with warnings" `Alternative` instance (please let me know if you have seen it somewhere on Hackage):

``` haskell
newtype ErrWarn e w a = EW {runEW :: Either e (w, a)} deriving (Eq, Show, Functor)
  
instance (Monoid e) => Alternative (ErrWarn e e) where 
    empty  = EW $ Left mempty
    EW (Left e1) <|> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <|> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r) -- coupling between @Either e@ and @(e,)@
    r@(EW (Right _)) <|> _ = r
```    

This approach, when computing `a <|> b`, does not try to compute `b` if `a` succeeds.
Thus, this instance matches the common left bias semantics. The approach accumulates all errors encountered up to the point of the first success and returns them as warnings.  
This is a lawful `Alternative` (satisfies required laws _(1-3)_) and it does not rely on any questionable unification of `empty` with non-trivial errors.  

I now feel justified using `Monoid e` constraint. Empty failure makes no sense, but empty warnings make a lot of sense!

But wait! To have `Alternative` we need `Applicative`.
It is possible to implement `Applicative` for this type in more than one way, one even leads to a valid `Monad` and `MonadPlus` (with the right-zero caveat discussed above).   
That approach is equivalent to `WriterT w (Except e)`, it does not try to `<*>`-accumulate `e`-s, it only accumulates `w`-s:

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

Please note the small difference.  Standard _transformers_ / _mtl_ `ExeptT` and `WriterT` both support `Alternative`
``` haskell
 (Monad m, Monoid e) => Alternative (ExceptT e m)	
 (Monoid w, Alternative m) => Alternative (WriterT w m)
``` 
but in a decoupled way, `ErrWarn` couples these two by "writing" `e`-s.

This instance exhibits similar problems with matching the `<*>` semantics as the `Monoid e => Either e` instance from the previous section (i.e. _(5,6)_ are not satisfied).  Overall it is a very well behaved alternative.

### Code Example

Here is a very convoluted (and not very good) parsing code that is intended only to demonstrate how `ErrWarn` works.
This code creates a natural transformation from the _attoparsec_ parser to `ErrWarn` and compares the error outputs from both.
  
This code will parse _ByteStrings_ like "id last-first-name dept boss2"
to produce, if successful, a hard-coded _id, name, department, and boss name_:

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
        ew :: A.Parser a  -> ErrWarn [String] [String] a
        ew p = singleErr $ A.parseOnly p txt

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

Trying [failure at the end](#failure-at-the-end) situation (typo in `"last-first-name"`):
``` haskell
-- >>> A.parseOnly emplP "id last-firs-name dept boss2"
-- Left "Failed reading: first-last-name not implemented yet"

-- >>> runEW $ emplP' "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
```
(A similar benefit can be achieved by using one of the Hackage _validation_ packages listed
at the end of this post or using `ExceptT` alternative.)

Trying [permissive computation at the end](#permissive-computation-at-the-end) situation (`"boss"` parsing error):
``` haskell
-- >>> A.parseOnly emplP "id last-first-name dept boss"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})

-- >>>runEW $ emplP' "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
```
(A similar benefit cannot be achieved by using a _validation_ package from the list at the end of this post or a _transformers_ stack. Please let me know if something like this exists elsewhere.)

We are no longer being thrown for a loop!  


### Extending `Either [e] ([e], a)`

The _right-catch with warnings_ semantics of `Either [e] ([e], a)` is a decent principled computation that can be extended to other types. 
For example, a similar semantics could find its way into some parser internals.   

I have created several prototype applicative instances (including a primitive `WarnParser` parser and `ErrWarnT` transformer) that follow the same semantics, they can be found in the linked [_add_blank_target repo](https://github.com/rpeszek/experiments/tree/master/alternative).   

`ErrWarnT` allows to program in `ErrWarnT e e f` alternative (e.g. `ErrWarnT e e Parser`) and annotate additional error information on `f` (e.g. during parsing). This allows, for example, to _pattern match on errors_ to figure out which alternatives in `<|>` have failed even if the overall computation has succeeded.  
`WarnParser` accumulates `<|>` similar errors and warnings out of the box.  

## Rethinking the Typeclass Itself

Is `Alternative` a wrong abstraction for what it is trying to do? 
I think it is.  IMO any abstraction intended for handling failures should include failures in its semantics. 
`Alternative` typeclass does not do that.  

`Alternative` is widely used and replacing it would, probably, be very hard or even impossible.  Replacement 
would be useful only if the ecosystem accepts it.   

One conceptually simple improvement would be to split `Alternative` to mimic the `Semigoup` / `Monoid` split.  
This would clean up some instances like `ExceptT` (the above [`Either [e]`](#either-e-a)) or [`Validation`](#relevant-work-on-hackage) and remove the need for questionable `empty` definitions
like `Left []`.  Incidentally, this would be the opposite of the [_add_blank_target `MonadZero`](https://wiki.haskell.org/MonadPlus_reform_proposal) proposal.

I would really like to see `e`-s in the typeclass definition:
``` haskell
class Applicative f => Semigroup1 f where
   (<|>)  :: f a -> f a -> f a 

class Applicative (f e) => Semigroup2 e f where
   (<||>)  :: f e a -> f e a -> f e a 
```
_(Roman numerals can be useful)_

The linked [_add_blank_target repo](https://github.com/rpeszek/experiments/tree/master/alternative) contains
some loose replacement ideas for `Alternative` and `MonadPlus`.  It is a work in progress. 


## `Alternative` Beyond Parsing

It should be mentioned that there are instances of `Alternative` such as 
the list `[]`, or `ZipList` where failures are not a concern.  Sorting algorithms using MonadPlus are thumbs up.  Examples like `LogicT` or other backtracking search mechanisms should be in the same boat (at least from the failure point of view, other aspects can be questionable
and fascinating [_add_blank_target stackoverflow on mplus associativity](https://stackoverflow.com/questions/15722906/must-mplus-always-be-associative-haskell-wiki-vs-oleg-kiselyov)).   

Also, these instances are rather cool.    
Languages like JavaScript, Python, Groovy have a concept of _truthiness_. _Truthy_ _Falsy_ are a thing and come with a Boolean algebra of sorts.  Try evaluating this in you browser's console:

``` javascript
> "hello" || ""
"hello"
> "" || "hello"
"hello"
```
_Truthiness_ is questionable because Boolean algebra laws (like `a || b = b || a`) no longer hold.

Now try these in ghci:
``` haskell
>>> "" <|> "hello"
"hello"
>>> "hello" <|> ""
"hello"
```
Alternative is a principled version of the _truthiness_.  The laws properly state the algebra limitations.   
As we have seen, the problem is in going with this generalization too far.

_async_ package uses `<|>` to return result form the computation that finishes first.  This seems a good use to me.

Several types like `ExceptT`, `Validation` (see [hackage section](#relevant-work-on-hackage) below) allow to use user defined monoid error types.  `mempty` may not have much sense as an error, but this setup offers interesting options for accumulating errors.  For example, using it with `Data.Monoid.Max` could be very interesting.


**Not so good:**   
An interesting case is the `STM` monad. `a <|> b` is used to chain computations that may want to `retry`.  I imagine, composing `STM` computations this way is rare.  If you wanted to communicate why `a` has decided to retry, how would you do that?  I consider `STM` use of alternatives problematic. 

`IO` itself is an `Alternative` and uses `<|>` as a `catch` that throws away the error information. 
I dislike the `IO` instance.  "Launching missiles" and not knowing what went wrong seems not ideal. 

## Relevant work on Hackage

[_add_blank_target _free_](https://hackage.haskell.org/package/free) package contains a semantic (free) version of _Alternative_.

[_add_blank_target _semigroupoids_](https://hackage.haskell.org/package/semigroupoids-5.3.5/docs/Data-Functor-Alt.html) offers an _Alt_ that is just a _Functor_ and does not need to have `empty`.  

[_add_blank_target _transformers_](https://hackage.haskell.org/package/transformers-0.5.6.2) ExceptT implements `Alternative` which accumulates "Lefts".  

A list of interesting packages that implement `Monoid`-like semantics for `Applicative` (most also implement `Alternative`) to accumulate errors provided by [_add_blank_target u/affinehyperplane](https://www.reddit.com/user/affinehyperplane/) on 
[_add_blank_target reddit](https://www.reddit.com/r/haskell/comments/kyo4xk/maybe_considered_harmful/gji7fmx?utm_source=share&utm_medium=web2x&context=3):

[_add_blank_target _either_](https://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Validation.html) defines `Either` like `Validation e a` applicative, both `<|>` and `<*>` accumulate errors   
[_add_blank_target _validation_](https://hackage.haskell.org/package/validation-1.1/docs/Data-Validation.html) defines a similar `Validation` type,  it does not define alternative instance.   
[_add_blank_target _validation-selective_](https://hackage.haskell.org/package/validation-selective-0.1.0.0/docs/Validation.html) defines a similar `Validation` type loaded with (non-monad) instances  
[_add_blank_target _monad-validate_](https://hackage.haskell.org/package/monad-validate-1.2.0.0/docs/Control-Monad-Validate.html) provides an interesting and very useful 
validation _monad_ transformer (this is lawful if you do not compare error outputs) that can accumulate errors, it does not implement `Alternative`.  

In the context of parsers, it should be noted that packages like _trifecta_, _(mega)parsec_ do nice job returning error messages when `<|>` fails.  

A good references about Alternative and MonadPlus in general is the [_add_blank_target Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Failure_and_choice:_Alternative.2C_MonadPlus.2C_ArrowPlus) and [_add_blank_target wikibooks](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus), both contain interesting links.

There are many stackoverflow answers about Haskell solutions to accumulating errors. These typically refer to some of the packages in the above list, I am not linking them here.   
There are many, many discussions about error output from different parsing libraries.  These are typically focused on criticizing 
a particular package (typically _attoparsec_) not the `Alternative` typeclass itself.   
I am sure, this list is not complete.  Please let me know if you see a relevant work elsewhere.   


## Conclusions, Thoughts

It is possible to implement instances that do a decent error management but it feels like
this is accomplished despite of the `Alternative` typeclass definition and its laws.  To answer my title: IMO `Alternative` is a wrong abstraction for managing computational failures.

Why errors are being overlooked? I assembled a possible list when writing about the [_add_blank_target Maybe Overuse](https://rpeszek.github.io/posts/2021-01-17-maybe-overuse.html#why-maybe-is-overused-possible-explanations) and that list seems to translate well to the alternative typeclass.  For example,  code using `<|>` is very terse, something with a stronger error semantics will most likely be more verbose; coding with `<|>` is simple, stronger error semantics 
will likely be more complex ...     
I could be wrong on this: the original usages of MonadPlus were probably related to sorting/searching and lists. Alternative computations with more complex error structure were probably introduced later?  ... and, the instances ended up outgrowing the typeclass? 

The _pessimist_ theme was partially inspired by the following two concepts:  
[_add_blank_target _Positivity Bias_](https://link.springer.com/referenceworkentry/10.1007%2F978-94-007-0753-5_2219#:~:text=Definition,favor%20positive%20information%20in%20reasoning.)
and, its opposite, the [_add_blank_target _Negativity Bias_](https://en.wikipedia.org/wiki/Negativity_bias) are psychological notions that, I believe, have a deep relevance to the programming in general.   
_Positivity Bias_ includes a _tendency to favor positive information in reasoning_ and, by definition, will make you think about "happy path" and "sunny day scenarios".   
_Negativity Bias_ includes a _tendency to favor negative information in reasoning_ and, by definition, will make you consider "rainy day scenarios", corner cases, error handling, error information.   
I think we should embrace some form of _pessimism_ and put in on the pedestal next to the principled construction.   

I hope this post will motivate more discussion about the _error information_ handling in Haskell.   
My particular interest is in discussing:

*   your views about rethinking the `Alternative` typeclass
*   your views on pessimism in programming
*   your views on the error information loss in Haskell code
*   is `ErrWarn` somewhere on Hackage and I did not see it?
*   other interesting `Alternative` instances that care about errors
*   obviously, anything that I got wrong

reddit discussion (TODO)  
github [_add_blank_target discussions](https://github.com/rpeszek/rpeszek.github.io/discussions/1)

Thank you for reading! 

