---
title: Probability Tree Diagrams. Recursion Schemes. Why Finding the Right Solution is Sometimes Hard?
author: Robert Peszek
lastmodified: Jul 18, 2021
featured: true
summary: About a simple tree recursion problem that gave me grief. 
toc: true
tags: Haskell
---

Code for this project can be found in my [_add_blank_target experiments](https://github.com/rpeszek/experiments) github repo ([_add_blank_target probability-tree-schemes](https://github.com/rpeszek/experiments/tree/master/probability-tree-schemes) folder).   

The goal of this post is to present a simple example that uses recursion schemes.
My other goal is to discuss my thoughts about reasons for getting stuck on finding the right solution. 
It will be a bit of a rant.

**Motivation:**  
Last week I encountered a programming problem that kept defeating me. 
It took me forever just to figure out how to translate the requirements into something I can code. 
The gist of this problem turned out to be very straightforward: implement a probability tree diagram (a decision tree annotated with probabilities).  Well not really, but close enough.   
I think, now, that the problem is simple, but it was not the case during 
the few hours I initially have spent on it.  This experience was somewhat surprising to me as I have worked for several years with decision graphs (not trees, but still). I just I failed to make the association. 

I am sharing my experience in this post.  This post shows the solution program, and as such, ended up becoming a "tutorial" of sorts about the recursion schemes.  I tried to make it easy to read even if some Haskell code is more at an intermediate level.

At the end of this post ([Why Finding the Right Solution is Sometimes Hard?](#why-finding-the-right-solution-is-sometimes-hard)) I rant about my beliefs pertaining to the question in the title. 


## The Problem

Consider a tree with edges (or nodes) annotated with probability weights.  The problem I am going to solve is simple:
calculate the path likelihoods for each of the _leaves_.  Basically multiply all the probabilities down each branch all the way to the leaf. 

This is obviously not so hard, and the biggest challenge was to figure out that this was what the requirements needed.  
The solution seems like a good exercise and I am sharing my solutions here.

All the goodies (`a`) are at the leaves, in addition, each node is annotated with some probability `p` and a label `String`:


```Haskell
import           Data.Functor.Foldable.TH

data ProbTree p a =
    Leaf p String a
    | Branches p String [ProbTree p a]
    deriving (Eq, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''ProbTree
```

I will be using [_add_blank_target _recursion-schemes_](https://hackage.haskell.org/package/recursion-schemes) package and `makeBaseFunctor` generates the code I will need to fold or unfold the tree.
All the instances I have declared, `Functor, Foldable, Traversable`, are intended for the consumption of the goodies `a`.
I will not care much about `a`-s in this post.  I will care about the `p`-s.  

This type is really a `Bifunctor` (Functor in both `p` and `a`), and most of the code I am going to show could just use that.
However, for some examples I will need something stronger, I need it to be `Traversable` in `p`.  
I will just use the [_add_blank_target _lens_](https://hackage.haskell.org/package/lens) package to get 
what I need.  You do not need to know much about lenses to read on. I will try to explain 
the usage as I go.

Keeping `p` as a type variable is convenient and, for example, could allow me to use it for (`Natural`) distribution counts or other things that probabilities can morph into.

### Why is this useful?

Decision Trees are popular because of their ability to visualize outcomes of various processes.  
It is sometimes useful to know the distribution of the final outcomes. Think about using _QuickCheck_ to randomly generate final outcomes or, maybe, randomly pick these outcomes from some big dataset of samples.  
`ProbTree p a` would use `a` to hold the generated values or to describe them in some way.
It is convenient to keep the data samples `a` separate from probabilities `p` (instead of just using something like `RoseTree (p,a)`) because we care about these samples only at the final outcome / leaf level.  
The extra `String` label is for my own convenience.  It allows me to trace how the recursion schemes work and will be handy in this presentation.

### Plumbing 

So I have a lens that will let me 'focus' on the `p`-s:

```Haskell
import           Control.Lens

-- | needs to be non-polymorphic because of nesting
probability :: Lens' (ProbTree p a) p
probability = lens get set 
  where
    get (Leaf p _ _) = p
    get (Branches p _ _) = p
    set (Leaf _ l x) r = Leaf r l x
    set (Branches _ l x) r = Branches r l x
```

It is considered a bad karma to use Record syntax for types that have several constructors. So I coded this by hand.
Otherwise template Haskell `makeLenses` could create `probability` if both constructors had a field with the same name `_probability :: p`.
If you new to lenses, think that the above code _encapsulates_ getting and setting `p`-s.  
This thing is what is called a simple lens. That means it does not allow me to change the `p` type.  I will need something more flexible than that(`makeLenses` does not create what I need anyway).

In a dream, dependently typed, world there would be a type level guarantee that in `Branches (p1 :: p) String [ProbTree (pn :: p) a]` we have `1 ~ sum pn` for probability weights and `p1 ~ sum pn` for cumulative case.  
In reality, it would be nice to, at least, not use `p ~ Float` all the time.   
Since I will be changing the meaning of what `p` is from a probability assigned to a node to calculated cumulative probability of getting to this node:

```Haskell
newtype NodeProb = NodeProb {unNodeProb :: Float} deriving Eq
newtype CumulativeProb = CumulativeProb {unCumulativeProb :: Float} deriving Eq

-- Show instances implemented, but not shown (pun not intended)
```

To deal with swapping `p`'s I need to implement an honest to god traversal.  Consider this as just a boilerplate:

```Haskell
probabilityT :: forall p r a f . Applicative f => 
    (p -> f r) 
    -> ProbTree p a 
    -> f (ProbTree r a)
probabilityT fn (Leaf p l a) = Leaf <$> fn p <*> pure l <*> pure a
probabilityT fn (Branches p l xs) = Branches <$> fn p <*> pure l <*> fx xs
  where fx :: [ProbTree p a] -> f [ProbTree r a]
        fx xs = traverse (probabilityT fn) xs
```

Now I can implement cumulative probabilities working with `Float`s and expose them to external code using more descriptive types:

```Haskell
compWithFloats :: 
   (ProbTree Float a -> ProbTree Float a) 
   -> ProbTree NodeProb a 
   -> ProbTree CumulativeProb a
compWithFloats fn = over probabilityT CumulativeProb . fn . over probabilityT unNodeProb
```

### Example Tree

I am now in the position to define a simple example tree:

``` Haskell
exTree :: ProbTree NodeProb ()
exTree = over probabilityT NodeProb $ Branches 1 "R" [
   Branches 0.5 "1" [
      Leaf 0.5 "11" ()
      , Leaf 0.5 "12" ()
   ]
   , Branches 0.5 "2" [
      Leaf 0.3 "21" ()
      , Leaf 0.3 "22" ()
      , Leaf 0.4 "23" ()
   ]
 ]
```
the lens `over` allows me to type the Floats and `map` these to the `NodeProb` type to indicate what the numbers represent at the type level. 
I will use this example moving forward. The final result of what I want to compute annotated with labels should look like this:

```
(0.25,"11"),(0.25,"12"),(0.15,"21"),(0.15,"22"),(0.2,"23")
```



### Traditional Solution, State Threads

There is a good possibility of finding a ready solution for the problem on _stackoverflow_ or even in _wikipedia_.  
The code in some psedo-language that mutates variables would look something like:

```
// some C-like language
// mutate in-place all probabilities on all nodes on a tree. 

calculateProbNode (weight, node) = 
   node.prob = weight * node.prob
   children = node.children
   for (child :: children) {
     calculateProb (node.prob, child)
   } 

calcuateProb (node) =  calculateProbNode (1, node)    
```

Since I moved to Haskell, code like this started to scare me. It mutates things, returns nothing, and it is easy to get it wrong.  
However, there are reasons (e.g. performance) when code like this is needed.  
There it the misconceptions about Haskell not being able to mutate variables.  In particular, variable mutation 
in internal implementation that leads to a referentially transparent result can be even implemented as a pure function.  The tooling for this is called `ST` monad. 

This is the above code implemented in Haskell:

```Haskell
import           Control.Monad.ST
import           Data.STRef

type MutableFloat s = STRef s Float

computeProbMutable :: forall a . ProbTree NodeProb a -> ProbTree CumulativeProb a 
computeProbMutable = compWithFloats (
    \t -> runST $ do
        mutable <- makeMutable t 
        mutableres <- recursion 1 mutable
        makeNonMutable mutableres
    )
 where 
   recursion :: Float -> ProbTree (MutableFloat s) a -> ST s (ProbTree (MutableFloat s) a)   
   recursion n (Branches mutp l xs)  = do 
     modifySTRef mutp (n *) -- modify probability in place
     n1 <- readSTRef mutp   -- read modified probablity
     xs1 <- mapM (recursion n1) xs -- recursively apply modified value to children
     return $ Branches mutp l xs1  

   recursion n x@(Leaf mutp _ _) = do 
     modifySTRef mutp (n *)  -- on Leaf there is not much to do, only modify the propability
     return x

-- | Traverse all Tree nodes and replace Float's with `MutableFloat`-s.
makeMutable :: ProbTree Float a ->  ST s (ProbTree (MutableFloat s) a)
makeMutable =  
   traverseOf probabilityT (newSTRef @Float)

-- | Traverse all Tree nodes and replace `MutableFloat`-s with not mutable regular `Float`s
makeNonMutable ::  ProbTree (MutableFloat s) a -> ST s (ProbTree Float a) 
makeNonMutable = traverseOf probabilityT readSTRef

-- >>> tstMutable
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
tstMutable :: String
tstMutable = printLeaves $ computeProbMutable exTree  
```

The bulk of the code is translating from not mutable Haskell to mutable variable `type MutableFloat s = STRef s Float`.  The rest is very similar to the psedo-code above.  

If you are not familiar with `ST`, the `s` is existential and prevents mutable Floats from escaping 
the `ST` Monad. You can use them inside `ST` but not after you leave it. 

What is mind-blowing to me is that Haskell introduced this in 1993!  Now such existential tricks are used a lot
(e.g. in dependently typed code) so the concept probably does not need much introduction.



### Simple Recursion Schemes

The above solution leaves a bad taste in my mouth. The idea I will pursue is that I can compute these values by _folding_
or maybe _unfolding_ the tree.  This example uses the [_add_blank_target _recursion-schemes_](https://hackage.haskell.org/package/recursion-schemes) package.
I am not going to delve deep into recursion schemes. My goal is to show the code with high level explanations
for how and why it works.

Great information about some of the category theoretical background can be found in Bartosz Milewski's [_add_blank_target CTFP Book, F-Algebras secion](https://bartoszmilewski.com/2017/02/28/f-algebras/).  
There is a series of blogs devoted to recursion-schemes by Patrick Thomson that are very good reading as well 
[_add_blank_target An Introduction to Recursion Schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html).


**Quick intro to recursion-schemes.**   

The Template Haskell instruction, `makeBaseFunctor ''ProbTree`, we have started with generated a new type that looks like this:

```Haskell
data ProbTreeF p a r = LeafF p String a | BranchesF p String [r]
```
As you can see, `ProbTreeF` is very similar to `ProbTree`, except the original 'recursive' `ProbTree p a` in 
`Branches p String [ProbTree p a]` got replaced with a new type variable `r` in `BranchesF p String [r]`.

_recursion_schemes_ uses `Base t` type family and `ProbTreeF p a r` is the _Base_ functor for `ProbTree`.

```Haskell
type family Base t :: * -> *
type instance Base (ProbTree p a) = ProbTreeF p a
```

and the library uses `Base` in all of its definitions. I will use `ProbTreeF` for clarity.   

Classic recursion scheme code often starts with the base functor and derives the recursive target type by applying
[_add_blank_target _Fix_](https://hackage.haskell.org/package/data-fix-0.3.1/docs/Data-Fix.html#g:1) from [_add_blank_target `data-fix`](https://hackage.haskell.org/package/data-fix).  Both are isomorphic:

```
Fix (ProbTreeF p a) =~ ProbTree p a
```
If this is new to you it will take some time digest.  (**end**)


#### Catamorphism

As a warm-up let's fold the tree by collecting information about leaves onto a single String:

```Haskell
import qualified Data.List as L

printLeaves :: forall p a. (Show p, Show a) => ProbTree p a -> String
printLeaves = fold fn
  where
      fn :: ProbTreeF p a String -> String
      fn (LeafF n lb a) = show (n, lb, a)
      fn (BranchesF _ _ xs) = L.intercalate "," xs
```

This code folds the tree onto a String by spelling out the `LeafF` content and then by concatenating the strings using comma when processing `BranchesF`. 

**Back to a quick into:**  
The _Base_ functor for _List_ (i.e. `[]`) looks like this:

```Haskell
data ListF a b = Nil | Cons a b 
```
folding a list would amount to:

```Haskell
printList :: (Show a) => [a] -> String
printList = fold fn
  where
      fn :: ListF a String -> String
      fn Nil = "(START)"
      fn (Cons a str) = show a <> ":" <> str
```

compare it to 
```Haskell
printList' = L.foldr (\a str -> show a <> "," <> str) "(START)" 
```

These are effectively the same!

Ability to fold arbitrary recursive data is rely very convenient.  For example, JSON data served
by some micro-services may lack consistency (not all micro-services are implemented in Haskell, you know)

> "What if I told you it is possible to fold JSON values?"

Morpheus, The Matrix

 _recursion-schemes_ defines _cata_ as an alternative name for _fold_.
_cata_ seems to be more popular and stands for _catamorphism_. (**end**)

**Continuing the warm-up:** 

We can do effectful things too!

```Haskell
printIO :: forall p a. (Show p, Show a) => ProbTree p a -> IO String
printIO = fold fn
  where
      fn :: ProbTreeF p a (IO String) -> IO String
      fn (LeafF n lb a) = do 
          print (n, lb, a) -- as before
          return $ "done leaf " <> lb
      fn (BranchesF _ lb ixs) = do
           xs <- sequence ixs
           print $ "Processed so far: " ++ show xs
           return $ "done branch " <> lb
```
This will print content of leaves, when processing branches will print what it processed before.

It produces this output:

```Haskell
*Schemes> tstPrintIO
(0.5,"11",())
(0.5,"12",())
"Processed so far: [\"done leaf 11\",\"done leaf 12\"]"
(0.3,"21",())
(0.3,"22",())
(0.4,"23",())
"Processed so far: [\"done leaf 21\",\"done leaf 22\",\"done leaf 23\"]"
"Processed so far: [\"done branch 1\",\"done branch 2\"]"
"done branch R"
```


**Solution using fold:**

A common trick, when using recursion schemes, is to use the folding type itself as the _carrier_, that is to fold `ProbTree Float a` onto itself.
_recusion-schemes_ provides a convenient `embed` function that typically can be used to implement all the uninteresting cases:

```Haskell
-- |
-- >>> printLeaves . computeProb $ exTree
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
computeProb :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb = compWithFloats (cata fn)
  where
      fn :: ProbTreeF Float a (ProbTree Float a) -> ProbTree Float a
      fn (BranchesF n l xs) = Branches n l (L.map (over probability (* n)) xs)
      fn x = embed x
```

the only interesting case is folding the `BranchesF n l xs` constructor.  It is folded into `BranchesF n l xs'` where `xs'` is a list of children with probability modified by the current value on the branch.
This line makes the computation flow towards the leaves.  Thus, we can assume that the current probability value `n`
has been already computed. 

#### Anamorphism

We have folded the tree onto itself.  That does not really fold / destroy much. 
We can as well do the same trick with unfolding, since we will not be unfolding much either.  
We can unfold Tree onto itself:

```Haskell
-- >>> printLeaves . computeProb $ exTree
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
computeProb' :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb' = compWithFloats (unfold fn)
  where
    fn :: ProbTree Float a -> ProbTreeF Float a (ProbTree Float a)
    fn (Branches n l xs) = BranchesF n l (L.map (over probability (* n)) xs)
    fn x = project x
```

Again we have a convenience function to handle all the not interesting cases called `project`.
And, again, the interesting case is flowing the computation down the branched towards the leaves. 
Other that for swapping `ProbTree` with `ProbTreeF`, this computation is identical on the `fold`.  
Alternative name for `unfold` is `ana` for _anamorphism_.

#### ST

If you looked closely at the `printIO` example in the [Catamorphism](#catamorphism) section, you may have noticed that that the fold of `Branches` was able to effecty things on the children.  We can use the same approach to our advantage.   
We will go into a mutating
franzy and fold `ProbTree` into just `MutableFloat s`:

```Haskell
computeST :: forall s a . ProbTree (ST s (MutableFloat s)) a -> ST s (MutableFloat s)
computeST = fold fn
  where
      fn :: ProbTreeF (ST s (MutableFloat s)) a (ST s (MutableFloat s)) -> ST s (MutableFloat s)
      fn (LeafF n _ _) = n

      fn (BranchesF stmn lb ixs) = do
           mn :: MutableFloat s <- stmn
           n :: Float <- readSTRef mn -- read branch proability
           mxs :: [MutableFloat s] <- sequence ixs -- get access to mutable probablities for children 
           mapM_ (`modifySTRef` (n *)) mxs -- mutate the children's probabilities (This even sounds wrong!)
           stmn -- return back the untouched branch probability
```
 
Making this into a pure function requires even crazier mutations (this reuses previously defined `makeMutable` and
`makeNonMutable`):

```Haskell
computeProbMutable :: forall a . ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProbMutable = compWithFloats (
    \t -> runST $ do
        mutable <- makeMutable t
        mutable' <- traverseOf probabilityT (pure . pure) mutable
        computeST mutable'
        makeNonMutable mutable
    )

-- |
-- >>> tstMutable
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
tstMutable :: String
tstMutable = printLeaves $ computeProbMutable exTree    
```

the result of `computeST mutable` is discarded, and I use `pure . pure` (double effectful?), how crazy is that!  
I am actually surprised that it even works.

I clearly do not like code like this, but it does provide more imperative intuitions about the recursive fold.



## Why Finding the Right Solution is Sometimes Hard?

**Rant Alert.** This is about the cases where we know or knew how to do thing well but we get stuck anyway. 
Obviously, finding better and even better ways for writing code is a subject of a never ending research, I am
not going to discuss that.  

Haskell is an amazingly efficient tool for writing new code when our fingertips know what to do.  
Sometimes they do not, and then it is not much better than other languages.  It lets us brute-force the code to satisfy the requirements.
It is Turing complete, meaning that you can write as much nonsense as you want.  

What bit me is:

* Short amount of time (few hours)
* Difficulty in identifying a nice abstraction to solve the problem
* The abstractions I ended up wanting to use were not in my short memory

So instead I went pushing a brute-force solution...

This experience put me in a philosophical mood of trying to pin-point the reasons why sometimes the code just writes itself and sometimes we get stuck.  

### Human condition

The underlying reason for why I do something wrong is typically me.  
We are not perfect, probably nothing shows it better than the software we write.  

> "The universal aptitude for ineptitude makes any human accomplishment an incredible miracle"

Stapp's Law

Stapp and Murphy (his coworker) laws are not about giving up, these statements are about understanding our limitations and acting on this understanding.  

### Time Pressure   

A hard deadline of a few hours does not (typically) happen in software engineering when
writing a new code.  It does for some data scientist.  It also happens in competitive programming. 
I am not a competitive programmer and I believe very few programmers are.

Software engineers have to (sometimes) work with a short hard timeline when troubleshooting critical application errors. I believe, seasoned developers get years of conditioning that puts them into a troubleshooting mindset when under stress. This is not a creative mindset effective in designing a new code.

For code that is not a cookie-cutter,  I believe I write the best code at night, when I sleep.  Sometimes I need a few nights. It took two nights (no involvement otherwise) and then < 1 hour of coding for the core examples presented here.  It is not just about the time spend on the task, it is often about the latency.

Time pressure can be very destructive. It can reduce both the development speed and the code quality.  
That applies to longer time scales as well. Software industry, in my experience, is focused on short term goals.
Quality is a long term goal.  
No project can afford to have no time constraints, so a balance needs to be struck. I believe the trade-offs are not well
understood by the decision makers.  ... But this is well beyond what this post is about.

'Uncle Bob' (Robert C. Martin) likes to talk about a process in which you are not done when you finish writing the code and it works. It is the sweat time for making the code nice.

### Large Bag of Tricks

In this analogy a programmer is a magician with a bag of tricks. The bigger the bag the better the programmer.
It takes more effort to sort through a bigger bag.  There are some interesting indexers in that solution space 
allowing really good programmers to find a trick faster.  One such indexer is the Category Theory. 
In general, any conceptual understanding of something is a record in some imaginary index.

The index record does not contain all the details, it is just a pointer, it takes time to load the actual details into short memory.  That could involve looking up some stuff in the code or re-reading an article...
Developer is a magician that needs time to prepare before he pulls out an **old** trick.  


### Not exactly Muscle Memory  

_Muscle Memory_ is a sports term.  You train muscle memory so the movements happen on an auto-pilot.  
Muscle memory tends to be a long term memory ("it is like riding a bike"), it is also hard to change.  This resistance to change could manifest itself as 'bad muscle habits' 
(e.g. a ski instructor skiing with a slight wedge, static body when making a shot in tennis...). 
Muscle memory is a long term memory, however athletes still require a warm-up lap / game / run...

For a long time, I have observed something very similar in programmers.  Programming work is surprisingly repetitive,  we develop something that resembles 'muscle memory' without even realizing it.  We can do things much faster when we work on familiar problems. We are expert cookie-cutters.

Programmers tend to repeat the previously used patterns, even if these patterns are inferior to other methods ("bad muscle habits").  

Mainstream resistance to FP can be viewed as an example of resistance to change the "muscle memory". 

The analogy is not perfect. We do not need to do a warm-up typing when we start work, instead, we do need longer warm-up period when switching to a programming area we neglected for months or years.  For me, that applies especially to the ability to translate 'real world requirements' to code design. 
I will just not think in terms that are not in my short memory, especially when under time pressure.

### Cookie cutters

Even looking at the job interview process, notice the focus on finding someone with a very closely fitting background.  
Have you ever seen a job posting asking for someone with a significantly different skill set from what the group already has? Hiring managers want to find cookie-cutters that can make cookies of a very similar shape that their team bakes.  
I think this is not optimal in long term.  This bias creates groups with a narrow pool of experience and 
expertise.

I have to consider myself a cookie cutter too. The choices I made in the code I design are be biased 
by my other work.

**Summary**

We can't change who we are, we are unlikely to change the industry.

Instead, we can try to train "good movements" into our "muscle memory" and grow our bag of tricks.

> "In short, I suggest that the programmer should continue to understand what he is doing, that his growing product remains firmly within his intellectual grip."

Edsger W. Dijkstra


My conclusion is this: it does not really matter if I get stuck and can't figure it out for a few days. 
For some problems I will be stuck for much longer.   
I am talking about pursuit of a well designed code, not finding a brute-force kludge.   
What really matters is not giving up and eventually figuring it out. 


> “You must understand, young Hobbit, it takes a long time to say anything in Old Entish. And we never say anything unless it is worth taking a long time to say.”   

J.R.R Tolkien and Treebeard





