---
title: Maybe Considered Harmful
author: Robert Peszek
---

**Motivating References:**  
I was very happy to find Michael Snoyman's [_add_blank_target Haskell The Bad Parts](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/) blog.    
I was also motivated by [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html)
post and its discussion on [_add_blank_target reddit](https://www.reddit.com/r/haskell/comments/kst0d3/elementary_programming/).

## Nutshell

`Maybe` is the functional answer to `null` - the [_add_blank_target billion dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare).  I claim that using `Maybe` can still be problematic.  

This post is focused on Haskell and my examples may not translate easily to other languages.  However, the overall concern about overuse of `Maybe` like types is not Haskell specific.

IMO `Maybe` is often overused.  I have started to question the use of `Maybe` every time I see it in the code base I maintain.  The result is either accepting its usage or rewriting the code to use `Either err`.  This approach has been effective in creating more robust code.  I am not claiming that `Maybe` has no place in well written code, only that its use should be closely examined.  This post shares a prospective of someone who maintains a complex Haskell code base. 

`Maybe` improves over `null`.  But it does not supersede it. Languages that have `null` also have easy access to logging, stack traces etc.  

`Maybe` typically represents data that can be _missing_ or computation that can result in an _unknown error_.  
What you typically care about is _what data is missing_ and _what is the error_.  

"Reasoning about code" is a very popular term in the FP community.  It typically refers to some advanced use of the type system or formal methods.   
IMO "reasoning about code" should start with reasoning about errors and corner cases (like missing data). This is why the use of `Maybe` needs to be examined and questioned.  In my experience this aspect of reasoning about code is often overlooked.

Reasoning about errors is not easy

*  Types wan't help with errors that bypass the type system (e.g. `error :: String -> a`)
*  Types wan't help with intentionally suppressed errors

This write-up focuses on the second bullet.

## Error Clarity Rule  


_What does `Nothing` mean?  If the reason behind it can be disambiguated to one root cause, then I consider the use of `Maybe` justified.  Otherwise,  I question it use._  

Consider this code:

``` haskell
import Data.Map
import Prelude hiding (lookup)

type Key = String
type Value = ...

-- OK
phone :: Map Key Value -> Maybe Value
phone = lookup "phone"

-- OK
email :: Map Key Value -> Maybe Value
email = lookup "email"

-- OK
creditCardNum :: Map Key Value -> Maybe Value
creditCardNum = lookup "card-number"

data FormData = FormData {
   fdPhone :: Value
   , fdEmail :: Value
   , fdCardNum :: Value
   }

-- less OK
formData :: Map Key Value -> Maybe FormData  
formData map = FormData <$> phone map <*> email map <*> creditCardNum map 
```

you can clearly explain the first 3 functions:

``` haskell 
explain :: err -> Maybe a -> Either err a
explain = ...

data Err = MissingEmail | MissingPhone | MissingCardNum 

email' :: Map Key Value -> Either Err Value
email' = explain MissingEmail . email
```

How do I explain `formData`?  I am stuck with:

``` haskell
data UnknownFieldMissing = UnknownFieldMissing 

formData' :: Map Key Value -> Either Err Value
formData' = explain UnknownFieldMissing . formData
```

and that "Unknown" is not a field name.  
The bigger the record the bigger problem this is.

This post provides real-world examples of similar problems.


## Harmful Real-World Examples

I am trying to focus here on things that can be found in the public domain. Some pain points from my day job have impacted my choice of examples.  
These are in no particular order, other than my presentation reuses types defined earlier.


### `Maybe` on Hackage

If you used older versions of [_add_blank_target servant-multipart](https://hackage.haskell.org/package/servant-multipart) you are familiar with 

_status code 400, message "fromMultipart returned Nothing"_. 

You must have noticed that your logs have been silent as well.  
The fix was implemented in 0.11.6

New version (much better):

``` haskell
-- version 0.11.6
class FromMultipart tag a where
   fromMultipart :: MultipartData tag -> Either String a
```

Old version:

``` haskell
-- version 0.11.5
class FromMultipart tag a where
  fromMultipart :: MultipartData tag -> Maybe a
```

Any typo, missed form field, wrong form field type submitted from the calling program result was a meaningless 400 error.  
To work around this issue I ended up resorting to
implementing `fromMultipart` in `Either MultiformError` monad and converting it to `Maybe` with something like that:

``` haskell
loggedMaybe ::  Either MultipartException a -> Maybe a
loggedMaybe (Left err) = do
    let logDetails = ...
    seq 
      (debugLogger logDetails) -- uses unsafePeformIO to match your logging style
      Nothing
loggedMaybe (Right r) = Just r 
```

to, at least, get some logs.

In just one project, that saved me hours in troubleshooting cost.
I have implemented both `FromMultipart` and `ToMultipart` which yields a property that
`fromMultipart . toMultipart` is the identity.  Just verifying that property would have been hard
without some information about errors.


It should be noted that many popular libraries offer convenience `Maybe` functions even though it is very easy to write this natural transformation: 

``` haskell
unExplain :: Either err a -> Maybe a
``` 

Why is that?  Why not just provide `Either` versions?  Looking just at _aeson_:
``` haskell
decode :: FromJSON a => ByteString -> Maybe a
eitherDecode :: FromJSON a => ByteString -> Either String a 
```
the name `decode` is suggestive of being the one commonly used.

Having `aeason` in spotlight, I like this part of the [_add_blank_target documentation](https://hackage.haskell.org/package/aeson-1.5.5.1/docs/Data-Aeson-Types.html##v:parse):

> The basic ways to signal a failed conversion are as follows:
>
> *  fail yields a custom error message: it is the recommended way of reporting a failure;
> *  empty (or mzero) is uninformative: use it when the error is meant to be caught by some (<|>);



### HKD pattern

Higher-Kinded Data pattern is super cool and can be very useful.  It would take me quite a bit of space to describe it here, fortunatelly this post does it form me:  [_add_blank_target reasonablypolymorphic on HKD pattern](https://reasonablypolymorphic.com/blog/higher-kinded-data/).
My example follows reasonablypolymorphic blog closely.

In nutshell, we can create a record type like 

``` haskell
data Person f = Person { 
    pName :: f String
  , pAge  :: f Int
  -- imagine a lot more fields here
  } 
``` 
parametrized by a type of kind `* -> *` like `Maybe` or `Identity`.  All or some of the fields in that record type have an `f` in front of them.  

_HKD Patter_ is about using generic programming to transform that record based on simple atomic operations that work on the fields.  
For example, we can hoist `forall a . f a -> g a` functions to `hkd f -> hkd g` (here `Person f -> Person g`). 

The example described in the above post is interesting since it uses `Maybe`.  Imagine that `Person` has a long list of fields and there is a web form for entering them.  

We get, for free, a completely generic validation function that, when restricted to our `Person` type, looks like this:

``` haskell
validate :: Person Maybe -> Maybe (Person Identity)
```

I hate when this is done to me:  it took 5 minutes to enter the information, the submit button is grayed out and I see no way to move forward.  Typically, when that happens, it is caused by a JavaScript error.  

In this example it is not a programming bug, it is a design decision:  a very sophisticated way to check that user entered all fields that does not provide information about which fields were missed. 

Web form data entry aside, I challenge you to find one meaningful example where the above `validate` is useful. Maybe a data science code that processes massive amount of data and requires all fields to be present to be useful?  All examples I can come up with seem far-fetched.

Questions I am asking:

- Would you expect a production code somewhere out there that validates user input using HKD pattern and actually uses `Maybe`?
- Did reasonablypolymorphic confuse or simplify things by using `Maybe` in its example?

Is this a case of an _abstraction blindfold_?

In reasonablypolymorphic blog post `Maybe` is not just in the `validate` function. The post defines the whole `GValidate` boilerplate that assumes `Maybe`. 

Fortunately, the approach can be generalized to other `f` types.

A meaningful validation would have a type

``` haskell
data FieldInfo = ...

validate :: Person Maybe -> Either [FieldInfo] (Person Identity)
```

This is arguably more work to do and beyond what _HKD pattern_ can offer.  However, it is quite possible to do something like this generically:

``` haskell
data FieldInfo = ...

validate :: Person (Either FieldInfo) -> Either [FieldInfo] (Person Identity)
```

Checkout the documentation for the [_add_blank_target barbies](https://hackage.haskell.org/package/barbies) package, it comes with exactly such example!  Notice, some boilerplate work is still needed to annotate missing values with field information:

``` haskell
addFieldInfo ::  Person Maybe -> Person (Either FieldInfo) 
```

`addFieldInfo` would need to happen outside of the _HKD pattern_.  
One can argue that a better solution would be to never ever use `Person Maybe`
and convert user form data entry directly to `Person (Either FieldInfo)`.



### `Traversable` with `Maybe`

_barbies_ validation of `Person` required a traversal of the _HKD_ type.  So, maybe, we should consider a somewhat simpler design 
where fields are unified into one type. Keeping up with the _reasonablypolymorphic_ example:

``` haskell
{-## LANGUAGE DeriveFunctor ##-}
{-## LANGUAGE DeriveFoldable ##-}
{-## LANGUAGE DeriveTraversable ##-}

data Person' a = Person' { 
    pName' :: a
  , pAge'  :: a
  -- imagine a lot more fields here
  } deriving (..., Functor, Foldable, Traversable)
```

And "now we are cooking with gas"!

``` haskell
data FieldData = ... -- unifying type for Person' fields

validateMaybe :: Person' (Maybe FieldData) -> Maybe (Person' FieldData)
validateMaybe = traverse id

validateEither :: Person' (Either FieldInfo FieldData) -> Either FieldInfo (Person' FieldData)
validateEither = traverse id
```

New `validateEither` is not as good as _barbies_ version, it gives user only one of the fields they missed.  
IMO `validateMaybe` is useless for data entry form validation.  

It is the same story. `Maybe` is useful only if you do not care about why you are getting `Nothing`.  

`validateMaybe` example is here for a reason.  It directly mimics the example discussed in [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html), which used

``` haskell
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = traverse
```
as its only example.

`Maybe` is viral.


### Cut `catMaybes`

I am yet to see real world example where the replacement of 

``` haskell
catMaybes :: [Maybe a] -> [a]
```

with

``` haskell
partitionEithers :: [Either e a] -> ([e], [a])
```
would not improve the code robustness.

It is mostly the same principle.  If you do not care about what data was rejected and why, then use `catMaybes`. If you care use `partitionEithers`. I question use of `catMaybes` in production code.  Processing large amounts of data being one possible exception.


## Questioning Record Types with all `Maybe` Fields

Besides being a natural fit for data coming in from something like a web form, 
there are other reasons for having many `Maybe` fields in a record.  
Here is my attempt at debunking some of them.

### Recreating _Java Beans_ with `Maybe` 

Defining record types with many `Maybe` fields allows to construct such records easily if you care about only some of the fields.

This can be done via some `empty` defaulting mechanism (I will use the `Person` type defined above to serve as example):

``` haskell
emptyPerson :: Person Maybe
emptyPerson = Person Nothing Nothing
```

or with a use of `Monoid` and `mempty`.  `emptyPerson` could be defined in a generic way as well
([_add_blank_target hkd-default](https://hackage.haskell.org/package/hkd-default).

Say your code cares about `pAge` only, you can just set `pAge`:

``` haskell
isDrinkingAge :: Person Identity -> Bool
isDrinkingAge p = runIdentity (pAge p) >= 21

test10YearOld = emptyPerson {pAge = Identity 10}
test = isDrinkinAge test10YearOld
```

I do not like this approach. It feels like a poorly typed code.   

It also reminds me of `null` and hence the _Java Bean_ title.  
([_add_blank_target Java Bean](https://stackoverflow.com/questions/1612334/difference-between-dto-vo-pojo-javabeans), was a popular pattern in Java ecosystem, a _Bean_ needs to have an empty constructor, a `setter` / `getter` method for each field.  _null_ fields could be considered as a code reuse mechanism:  programs need fewer "classes" of "objects" if some fields can be left as _null_.)

An easy improvement would be to create `Age` type

``` haskell
newtype Age = Age Int 

data Person'' f = Person'' { 
  ...
  , pAge''  :: f Age
  -- imagine a lot more fields here
  } 

isDrinkingAge' :: Age -> Bool
isDrinkingAge' (Age a) = a >= 21
```

But this is the type proliferation the `null` and _Bean_ concepts have been trying to avoid in the first place.  
So if we feel strongly about checking age on `Person` we can use Haskell's ability to program with polymorphic fields: 

``` haskell
{-## LANGUAGE TypeApplications ##-}
{-## LANGUAGE DataKinds ##-}
{-## LANGUAGE FlexibleContexts ##-}
{-## LANGUAGE DuplicateRecordFields ##-}

import GHC.Records

isDrinkingAge :: HasField "pAge" p (Identity Int) => p -> Bool
isDrinkingAge p = runIdentity (getField @ "pAge" p) >= 21

newtype AgeTest = AgeTest { pAge :: Identity Int}

test40YearOld = isDrinkingAge $ AgeTest (Identity 40)
```

IMO creating record types with lots of `Maybe` fields for the benefit of easy construction is not a good pattern.


### _Maybe-First_ `Monoid` fields

`Maybe (First _)` is a valid `Monoid`.  
Using Maybe-First semantics, `mappend` selects the first non-`Nothing` element. Appending can be implemented as:

``` haskell
import Data.Semigroup (First (..))

a <> b = fmap getFirst $ fmap First a <> fmap First b

-- or in more "elemenary" way as:

Nothing <> b = b
a <> Nothing = a
a <> b = a
```

You can use this approach on each field to define `Monoid` instances for large record types that consist of `Maybe` fields. 
I will use the previously defined `Person Maybe` to "represent" such a record. 

``` haskell 
instance Semigroup (Person Maybe) where
   Person a b <> Person c d = Person (a <-> c) (b <-> d)
     where a <-> b = fmap getFirst $ fmap First a <> fmap First b

instance Monoid (Person Maybe) where
   mempty = emptyPerson   
```

This pattern provides a convenient defaulting mechanism and allows to set groups of fields at once using `mappend`.  
``` haskell
overrides <> person
```

This approach can also result in very weird data combinations if one is not careful: 

``` haskell
gd = (mempty :: Person Maybe) {pName = "grandpa"} -- missing age
defaultperson = (mempty :: Person Maybe) {pName = Just "baby", pAge = Just 1} 

grampatoddler = gd <> defaultperson -- it is easy to create surprising data
```
  
The reverse: `First (Maybe _)` `Monoid` instance is far less convenient to use but is much less surprising.  

I do not think that `Maybe-First` `Monoid` is necessarily bad.  There is simply a trade-off between
the conveniences it offers and its gotchas.  I prefer designs that provide more safety over accidental bugs.


### `Alternative` typeclass

`Alternaive` `(<|>)`is very convenient tool often used with parsers.  It can be dangerous on its own merit.  
For example, consider code like this:

``` haskell
  secificParser <|> bestEffortParser
``` 

The specs may change and you will never learn that `secificParser` no longer works because `bestEffortParser` effectively hides the issue.  
IMO maintainable code using `<|>` should avoid including "catch all" elements. 

There is currently no `Alternative` instance for `Either err` but there is one for `Maybe`.  This creates temptations...

The main concern about `Alternative` is that it can silence errors in your code that you want to know about.  
If you are using it with `Maybe`, then you do not care about the error anyway.  It is, again, about confidence in the code and in the data.
I question it when I see it.


## Good uses of Maybe

I think the following old design principle (Postel's law) is still valid (on the implementation side)  

_Lenient input, Strict Output_.

This means `Maybe` is great as input parameter, less so in the result.   
The above _Error Clarity Rule_ should be the overriding factor here.
If the call-site can disambiguate what `Nothing` is, then `Maybe` results are fine.

In particular, _prisms_ are typically used on not nested coproducts, thus, the call-site can disambiguate
at which level the pattern match failed.  `lookup` in `Data.Map`, `find` for a `Foldabe` are all perfectly good choices for a `Maybe` result type.

_HKD pattern_ used with lots of `Maybe` fields for JSON inputs or DB records is a perfectly good example of good `Maybe` usage as well.


## Why `Maybe` is Overused? Possible Explanations

IMO these are the main causes of the overuse:

1.  Using `Maybe` is simpler that `Either`. If doing the right thing takes more time and effort it will often not be done.

2. Coding with `Maybe` is terser. Coding with `Maybe` may, thus, seem more elegant.

3.  `Maybe` is more expressive. Examples: `Maybe` has `Alternative` instance `Either err` does not. To use `Monad`, `Applicative` with `Either err` you need to unify on the `err` type. `Monad` and `Applicative` are, thus, more available for `Maybe`. 

4.  Sophisticated abstractions can obscures common sense.  `Maybe` is likely to fit the abstraction more often and easier than `Either`.  

Oversimplifications are nothing new in mathematical modeling.  Anyone who studied, for example, mathematical physics has seen a lot of hair raisingly scary oversimplifications.  Code design appears not that different.  

5.  Non production code.  Lots of Haskell code is about CS research.  Lots of Haskell code is about pet projects.  Such code does not need to be maintained in production. `Maybe` is good enough.

6.  Developer has a lot of confidence in computation returning `Maybe` (knows exactly what can go wrong) so `Either` is redundant.  This is not the overuse case and is justified.  

I tried to present a common sense criticism of overuse of `Maybe` type in Haskell ecosystem.
It is quite possible that you learned nothing new.  It seems that sometimes trees prevent us from seeing the forest, I wrote the post on the off chance that this could be the case. 
I started with [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html) link and want to end with it.  Would more explicit "elementary" programs help in spotting obvious things like error information loss?  I am not taking a position
I am just asking the question.

I am sure I do not have a full understanding of why and how `Maybe` is overused.  The intent of this post is to try to start a discussion.

Discussion links:

*  github [_add_blank_target discussions](https://github.com/rpeszek/rpeszek.github.io/discussions)
*  reddit - TODO
