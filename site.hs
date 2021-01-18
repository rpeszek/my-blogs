--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import qualified Data.Text as T
import qualified Text.Pandoc.Options as P
import qualified Text.DocTemplates as DT
import           Data.Functor.Identity

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- contact is currently not visible
    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= sanitizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do 
            underlying <- getUnderlying
            toc        <- getMetadataField underlying "toc"
            let writerOptions' = maybe defaultHakyllWriterOptions (const withTOC) toc
            pandocCompilerWith defaultHakyllReaderOptions writerOptions'         
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= sanitizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            posts_ <- mapM sanitizeUrls posts    
            renderAtom myFeedConfiguration feedCtx posts_

    create ["RSS.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            posts_ <- mapM sanitizeUrls posts    
            renderRss myFeedConfiguration feedCtx posts_

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Infrequent, Pragmatic, Lambda Blog"
    , feedDescription = "Infrequent, Pragmatic, Lambda Blog Feed"
    , feedAuthorName  = "Robert Peszek"
    , feedAuthorEmail = "rpeszek.io@gmail.com"
    , feedRoot        = "https://rpeszek.github.io/"
    }


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


sanitizeUrls :: Item String -> Compiler (Item String)
sanitizeUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (addTargetHack . relativizeUrlsWith (toSiteRoot r)) item

addTargetHack :: String  -- ^ HTML to relativize
           -> String  -- ^ Resulting HTML
addTargetHack  = T.unpack . T.replace ">_add_blank_target " " target=\"_blank\">" . T.pack     


-- * TOC

-- from https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
withTOC :: P.WriterOptions
withTOC = defaultHakyllWriterOptions
        { P.writerNumberSections  = False
        , P.writerTableOfContents = True
        , P.writerTOCDepth        = 3
        , P.writerTemplate        = Just tocTemplate
        }

tocTemplate :: DT.Template T.Text
tocTemplate = either error id . runIdentity . DT.compileTemplate "" $ T.unlines
  [ "<div class=\"toc\"><div class=\"header\">Table of Contents</div>"
  , "$toc$"
  , "</div>"
  , "$body$"
  ]        