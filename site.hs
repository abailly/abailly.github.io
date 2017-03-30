--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Hakyll
import           Slides
import           System.FilePath
-- fixing issue with file encodin
-- https://groups.google.com/forum/#!topic/hakyll/jrGATyI1omI/discussion

import           GHC.IO.Encoding

-- common stuff for RSS items
foldlabsFeedConfiguration :: FeedConfiguration
foldlabsFeedConfiguration = FeedConfiguration
    { feedTitle       = "Arnaud Bailly's  Blog"
    , feedDescription = "Random musings on code..."
    , feedAuthorName  = "Arnaud Bailly"
    , feedAuthorEmail = "arnaud@igitur.io"
    , feedRoot        = "http://abailly.github.io"
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

    match "reveal.js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "slides/*.md" $ do
        route   $ setExtension "html"
        compile $ pandocSlideCompiler
            >>= loadAndApplyTemplate "templates/slides-reveal.html"    slidesCtx

-- should factorize templates application
    match "cours/*.md" $ do
        route $ setExtension "html"
        compile $ pandocSlideCompiler
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
  escaped   "title"            `mappend`
  escaped   "subtitle"         `mappend`
  field     "slug" slugify     `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

slidesCtx :: Context String
slidesCtx =
  escaped   "title"                  `mappend`
  escaped   "subtitle"               `mappend`
  field     "revealjs-url"  revealJs `mappend`
  dateField "date" "%B %e, %Y"       `mappend`
  defaultContext
  where
    revealJs = const $ return "/reveal.js"

slugify :: Item String -> Compiler String
slugify item = do
  metadata <- getMetadata (itemIdentifier item)
  let slug  = dropExtension . takeFileName . toFilePath
  return $ slug (itemIdentifier item)

--------------------------------------------------------------------------------
postList sortFilter = do
  list    <- loadAll "posts/*"
  posts   <- sortFilter list
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl homeCtx posts

stripPrefix :: Routes
stripPrefix = customRoute stripPrefixRoute
  where
    stripPrefixRoute ident = dropFirstDirectory p </> takeFileName p
      where
        p = toFilePath ident
    dropFirstDirectory = joinPath . tail . splitPath . takeDirectory

