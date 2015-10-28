--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
-- fixing issue with file encodin
-- https://groups.google.com/forum/#!topic/hakyll/jrGATyI1omI/discussion

import           GHC.IO.Encoding

-- common stuff for RSS items
foldlabsFeedConfiguration :: FeedConfiguration
foldlabsFeedConfiguration = FeedConfiguration
    { feedTitle       = "FoldLab Blog"
    , feedDescription = "Random musings on code..."
    , feedAuthorName  = "Arnaud Bailly"
    , feedAuthorEmail = "arnaud@foldlabs.com"
    , feedRoot        = "http://blog.foldlabs.com"
    }

escaped :: String -> Context String
escaped fieldName = field fieldName $ \item -> do
    content <- getMetadataField (itemIdentifier item) fieldName
    case content of
      Just s  -> return $ escapeHtml s
      Nothing -> return ""

--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

-- should factorize templates application
    match "cours/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/cours.html"    postCtx
            >>= relativizeUrls
            >>= loadAndApplyTemplate "templates/default.html"    postCtx

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= relativizeUrls
            >>= loadAndApplyTemplate "templates/default.html"  postCtx

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= relativizeUrls
            >>= loadAndApplyTemplate "templates/draft.html"  postCtx

    match "training/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/training.html"    postCtx
            >>= relativizeUrls
            >>= loadAndApplyTemplate "templates/default.html"  postCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "content"
          renderAtom foldlabsFeedConfiguration feedCtx posts

    create ["index.html"] $ do
        route idRoute
        compile $
          postList recentFirst
          >>= makeItem
          >>= loadAndApplyTemplate "templates/posts.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" homeCtx
          >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
homeCtx :: Context String
homeCtx = titleField "Welcome" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
  escaped "title" `mappend`
  dateField "date" "%B %e, %Y" `mappend`

  defaultContext


--------------------------------------------------------------------------------
postList sortFilter = do
  list    <- loadAll "posts/*"
  posts   <- sortFilter list
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts
