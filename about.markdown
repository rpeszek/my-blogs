---
title: About
---

**Infrequent:** I do not blog much.  
**Passionate:**  I will commit a post if something really bugs me or I feel very passionate about a topic that is not otherwise covered by people smarted than me  
It will be about programming, most likely functional programming.  

``` haskell
blog = do 
    liftIO $ threadDelay veryLongTime1
    ideas <- ask @PriorityQueue
    a <- peek @Annoyance ideas
    liftIO $ threadDelay veryLongTime2
    file <- writeBlog a 
    liftIO $ threadDelay sleepOnIt 
    publish file
    blog
```

**About me:** Currently, I work as a Haskell Developer.  
I have a long history of mainstream programming work, mostly using Java and friends (Groovy, JS, C#, ...).  
My pet projects use Haskell and friends (Idris, Purescript, Elm, Scalaz, ...).
 
