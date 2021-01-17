---
title: Maybe Overuse, Stories About Error Information Loss
author: Robert Peszek
---


#### Motivating References:

I was very happy to find Michael Snoyman's [_add_blank_target Haskell The Bad Parts](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/) series.    
I was also motivated by [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html)
post ([_add_blank_target reddit](https://www.reddit.com/r/haskell/comments/kst0d3/elementary_programming/)).

**Similar:** [_add_blank_target reddit data_maybe_harmful](https://www.reddit.com/r/haskell/comments/jxj8i/data_maybe_harmful/)


This post is Haskell specific.   
This post treats the term 
error colloquially, it does not try to distinguish between [_add_blank_target _exceptions_ and _errors_](https://wiki.haskell.org/Error_vs._Exception).  I particular, the term _error information_ is used here to mean the content of `err` in `Either err`.

## Nutshell

`Maybe` is the functional answer to `null` - the [_add_blank_target billion dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare).  I claim that using `Maybe` can still be problematic.  

IMO `Maybe` is often overused.  I have started to question the use of `Maybe` every time I see it in the code base I maintain.  The result is either accepting its usage or rewriting the code to use `Either`.  This approach has been effective in creating more robust code.  I am not claiming that `Maybe` has no place in a well written code, only that its use should be closely examined.  I have seen brilliant code that has been hard to maintain because of its overuse of `Maybe`. This post shares a perspective of someone who maintains a complex Haskell code base. 

`Maybe` improves over `null`.  But it does not supersede it. Languages that have `null` also have easy access to logging, stack traces etc.  

`Maybe` typically represents data that can be _missing_ or a computation that can result in an _unknown error_.  
What you typically care about is _what data is missing_ and _what is the error_.  

_Code correctness_, _reasoning about code_ are, arguably, the defining aspects of FP.  _Reasoning about code_ typically refers to some advanced use of the type system or formal methods.   IMO "reasoning about code" should start with reasoning about errors and corner cases (like missing data). This is why the use of `Maybe` needs to be examined and questioned.  In my experience this aspect of reasoning about code is often overlooked.

Reasoning about errors is not easy:  The type system can't help with errors that bypass it (e.g. `error :: String -> a`).
It can't help with exceptions which were intentionally suppressed into `Nothing` _(the focus of this post)_.  And, the list goes on...

My points / pleas are:

*  The ecosystem would be better off without offering convenience combinators that return `Maybe` if an equivalent returning `Either` exists 
*  Examples, tutorials, and blog posts should favor `Either` over `Maybe`
*  Developers should be careful about not overusing `Maybe`

## Error Clarity Rule  


_What does `Nothing` mean?  If the reason behind it can be disambiguated to one root cause, then I consider the use of `Maybe` justified.  Otherwise,  I question its use._  

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
The bigger the record, the bigger the problem.


## Harmful Real-World Examples
 
These are in no particular order, other than this presentation reuses types defined earlier.

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

Any typo, missed form field, wrong form field type submitted from the calling program resulted in a meaningless 400 error.  
To work around this issue I ended up 
implementing `fromMultipart` in `Either MultiformError` monad and converting it to `Maybe` with something like that:

``` haskell
loggedMultipartMaybe ::  Either MultipartException a -> Maybe a
loggedMultipartMaybe (Left err) = do
    let logDetails = ...
    seq 
      (debugLogger logDetails) -- uses unsafePeformIO to match your logging style
      Nothing
loggedMultipartMaybe (Right r) = Just r 
```
to, at least, get some logs.

In just one project, that saved me hours in both new development and troubleshooting cost.  
For a more complex multipart form that implements both `FromMultipart` and `ToMultipart` by hand, verifying that 
`fromMultipart . toMultipart` is the identity would have been very hard
without some information about errors.  If the multipart is called from a different program, different language ...


#### Convenience Combinators:

It should be noted that many popular packages offer convenience `Maybe` functions even though it is very easy to write this natural transformation: 

``` haskell
unExplain :: Either err a -> Maybe a
``` 

Why is that?  Why not just provide `Either` versions?  Looking at _aeson_ as an example:
``` haskell
decode :: FromJSON a => ByteString -> Maybe a
eitherDecode :: FromJSON a => ByteString -> Either String a 
parseEither :: (a -> Parser b) -> a -> Either String b 
parseMaybe :: (a -> Parser b) -> a -> Maybe b 
```
the name `decode` is suggestive of being the one commonly used.

Having `aeason` in the spotlight, I like this part of their [_add_blank_target documentation](https://hackage.haskell.org/package/aeson-1.5.5.1/docs/Data-Aeson-Types.html##v:parse):

> _The basic ways to signal a failed conversion are as follows:_
>
> *  _fail yields a custom error message: it is the recommended way of reporting a failure;_
> *  _empty (or mzero) is uninformative: use it when the error is meant to be caught by some (<|>);_

Overuse of `mzero` in parsing code is as bad as the overuse of `Maybe`.

### Cut `catMaybes`

Replacing

``` haskell
catMaybes :: [Maybe a] -> [a]
```
with

``` haskell
partitionEithers :: [Either e a] -> ([e], [a])
```
very often improves the robustness of code.

Consider `Contact` record type with `cEmail :: Maybe Email` field.  We can get 

* lists of emails by using `catMaybes`
* list of emails and the information which `Contact`-s do not have an email using `partitionEithers`.  

The `Contact` list could come from a parsed JSON and could contain a list of company employees or is
a parsed mail-mime _CC:_ header.  Missing email should be rare.  

``` haskell
import Control.Arrow ((&&&))

-- maybe version
getEmails :: [Contact] -> [Email]
getEmails = catMaybes . map cEmail

-- either version
data MissingData a = MissingEmailData a | ...

getEmails' :: [Contact] -> ([MissingData Contact], [Email])
getEmails' = partitionEithers . map (uncurry explain . (MissingEmailData &&& cEmail))
```

These are "it is rare, therefore can be ignored" vs "it is rare and therefore cannot be ignored" approaches.   
I do not know what is the proper name for software that does not handle corner cases. 
I call it _expensive_ ;)  
I question the use of `catMaybes` every time I see it.  

I try to be terse in the above example, but it is clear that `Either` is more work.  Terseness and ease of programming
are IMO some of the reasons for `Maybe` overuse.


### HKD pattern

Higher-Kinded Data pattern is super cool and can be very useful.  This post explains what it is:  [_add_blank_target reasonablypolymorphic on HKD pattern](https://reasonablypolymorphic.com/blog/higher-kinded-data/).
My example follows _reasonablypolymorphic_ blog closely.

In nutshell, we can create a record type like 

``` haskell
data Person f = Person { 
    pName :: f String
  , pAge  :: f Int
  -- imagine a lot more fields here
  } 
``` 
parametrized by a type of kind `* -> *` e.g. `Maybe` or `Identity`.  All or some of the fields in that record type have an `f` in front of them.   
_HKD Patter_ is about using generic programming to transform that record based on operations that work on the fields.  
For example, we can hoist `forall a . f a -> g a` functions to `hkd f -> hkd g` (here `Person f -> Person g`). 

Imagine that `Person` has a long list of fields and there is a web form for entering them.   
The post descibes a completely generic validation function that, when restricted to our `Person` type, looks like this:

``` haskell
validate :: Person Maybe -> Maybe (Person Identity)
```

I hate when this is done to me:  it took 5 minutes to enter the information, the submit button is grayed out, and I see no way to move forward.  Typically, when that happens, it is caused by a JavaScript error.  

In this example it is not a programming bug, it is a design decision:  a very sophisticated way to check that user entered all fields that does not provide information about which fields they missed.  
Web form data entry aside, I challenge you to find one meaningful example where the above `validate` is useful. A one off data science code that processes massive amount of data and requires all fields to be present to be useful?  All examples I can come up with seem far-fetched and still would benefit from having `Either`.

Questions I am asking:

- Would you expect a production code somewhere out there that validates user input using HKD pattern and actually uses `Maybe`?
- Did _reasonablypolymorphic_ confuse or simplify things by using `Maybe` in its example?

In _reasonablypolymorphic_ post `Maybe` is not just in the `validate` function. The post defines the whole `GValidate` boilerplate that assumes `Maybe`. 

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

Check out the documentation for the [_add_blank_target barbies](https://hackage.haskell.org/package/barbies) package, it comes with exactly this example!  Notice, some boilerplate work is still needed to annotate missing values with field information:

``` haskell
addFieldInfo ::  Person Maybe -> Person (Either FieldInfo) 
```

`addFieldInfo` would need to happen outside of the _HKD pattern_.  
One can argue that a better solution would be not to use `Person Maybe`
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

New `validateEither` is not as good as the _barbies_ version. It gives the user only one of the fields they missed.  
IMO `validateMaybe` is useless for any data entry form validation.  

The `validateMaybe` example is here for a reason.  It directly mimics the example discussed in [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html), which used

``` haskell
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = traverse
```
as its only example.

`Maybe` is viral.





## Questioning Record Types with all `Maybe` Fields

Besides being a natural fit for data coming from something like a web form, 
there are other reasons for designing record types with many `Maybe` fields.  
Here is my attempt at debunking some of them.

### Recreating _Java Beans_ with `Maybe` ;)

Defining record types with many `Maybe` fields allows to construct such records easily if you care about only some of the fields.

This can be done via some `empty` defaulting mechanism (I will use the `Person` type defined above to serve as an example):

``` haskell
emptyPerson :: Person Maybe
emptyPerson = Person Nothing Nothing
```

or with a use of `Monoid` and `mempty`.  `emptyPerson` could be defined in a generic way as well
(see [_add_blank_target hkd-default](https://hackage.haskell.org/package/hkd-default)).

Say, your code cares about `pAge` only, you can just set `pAge`:

``` haskell
isDrinkingAge :: Person Identity -> Bool
isDrinkingAge p = runIdentity (pAge p) >= 21

test10YearOld = emptyPerson {pAge = Identity 10}
test = isDrinkinAge test10YearOld
```

I do not like this approach. It feels like a poorly typed code.   

It also reminds me of `null` and hence the _Java Bean_ reference. 
([_add_blank_target Java Bean](https://stackoverflow.com/questions/1612334/difference-between-dto-vo-pojo-javabeans) was a popular pattern in the Java ecosystem. A _Bean_ needs to have an empty constructor, and a _setter_ / _getter_ method for each field. It seems very similar to a record type with lots of `Maybe` fields.)

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

If we feel strongly about checking age on the `Person` type, we can use Haskell's ability to program with polymorphic fields: 

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
This pattern provides a convenient defaulting mechanism and allows to set groups of fields at once using `mappend`.  
``` haskell
overrides <> record
```

This approach can also result in very weird data combinations if one is not careful: 

``` haskell
gd = (mempty :: Person Maybe) {pName = "grandpa"} -- missing age
baby = (mempty :: Person Maybe) {pName = Just "baby", pAge = Just 1} 

grampatoddler = gd <> baby -- it is easy to create surprising data
```
  
The reverse: `First (Maybe _)` `Monoid` instance is far less convenient to use but is much less surprising.  

I do not think that `Maybe-First` `Monoid` is necessarily bad.  There is simply a trade-off between
the conveniences it offers and its gotchas.  I prefer designs that provide more safety over accidental bugs.


### `Alternative` typeclass

`Alternaive` `(<|>)` is very convenient tool often used with parsers.  It can be dangerous on its own merit. It can suppress error information in questionable ways if overused. 

There is currently no `Alternative` instance for `Either err` but there is one for `Maybe`. 
This creates temptation to `unExplain` the `err` ...   


## Good uses of Maybe

I think the following old design principle (Postel's law) is still valid (on the implementation side)  

_Lenient input, Strict Output_.

This means `Maybe` is great as input parameter, less so in the result. 
The above _Error Clarity Rule_ should be the overriding factor here.
If the call-site can disambiguate what `Nothing` is, then `Maybe` results are fine.

In particular, _prisms_ are typically used on not nested coproducts, thus, the call-site can disambiguate
at which level the pattern match failed.  `lookup` in `Data.Map`, `find` for a `Foldabe` are all perfectly good choices for a `Maybe` result type.


## Why `Maybe` is Overused? Possible Explanations

IMO these are the main causes of the overuse:

1.  Using `Maybe` is simpler that `Either`. If doing the right thing takes more time and effort it will often not be done.

2. Coding with `Maybe` is terser. Thus, coding with `Maybe` may seem more elegant.

3.  `Maybe` is more expressive. Examples: `Alternative` instance; to use `Monad`, `Applicative` with `Either err` you need to unify on the `err` type which is extra work.

4.  Sophisticated abstractions can obscure common sense.  `Maybe` is likely to fit the abstraction more often and easier than `Either`.  

Oversimplifications are nothing new in mathematical modeling.  Anyone who studied, for example, mathematical physics has seen a lot of crazy oversimplifications.  Code design appears not that different.  

5.  Non production code.  Lots of Haskell code is about CS research.  Lots of Haskell code is pet projects.  Such code does not need to be maintained in production. `Maybe` is good enough.

If developer can disambiguate the reason for `Nothing`, then use of `Either` is optional.  This is not an overuse case and is justified.  

I started with link to [_add_blank_target Elementary Programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html) post and want to end with it.  Would more explicit "elementary" programs help in spotting obvious things like error information loss?  I think they could.  Starting from requirements and going back to most elementary solution would probably never arrive at:
``` haskell
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
```
if only the requirements cared about errors.   
I do not advocate avoiding abstractions,  just do not forget errors on the way. Don't throw the baby out with the bathwater.

I am sure I do not have a full understanding of why and how `Maybe` is overused.  The intent of this post is to start a discussion.

Discussion links:

*  github [_add_blank_target discussions](https://github.com/rpeszek/rpeszek.github.io/discussions)
*  [_add_blank_target reddit](https://www.reddit.com/r/haskell/comments/kyo4xk/maybe_considered_harmful/)
