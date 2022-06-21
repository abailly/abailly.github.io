--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Time
import Hakyll
import Slides
import System.FilePath
import Text.Pandoc.Options

-- fixing issue with file encodin
-- https://groups.google.com/forum/#!topic/hakyll/jrGATyI1omI/discussion

import GHC.IO.Encoding

-- common stuff for RSS items
foldlabsFeedConfiguration :: FeedConfiguration
foldlabsFeedConfiguration =
    FeedConfiguration
        { feedTitle = "Arnaud Bailly's  Blog"
        , feedDescription = "Random musings on code..."
        , feedAuthorName = "Arnaud Bailly"
        , feedAuthorEmail = "arnaud@igitur.io"
        , feedRoot = "http://abailly.github.io"
        }

escaped :: String -> Context String
escaped fieldName = field fieldName $ \item -> do
    content <- getMetadataField (itemIdentifier item) fieldName
    case content of
        Just s -> return $ escapeHtml s
        Nothing -> return ""

pandocMathCompiler =
    let mathExtensions =
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions =
            defaultHakyllWriterOptions
                { writerExtensions = newExtensions
                , writerHTMLMathMethod = MathJax ""
                }
     in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8
    hakyll $ do
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "reveal.js/**" $ do
            route idRoute
            compile copyFileCompiler

        match "reveal.js-3.8.0/**" $ do
            route idRoute
            compile copyFileCompiler

        match "js/*" $ do
            route idRoute
            compile copyFileCompiler

        match "cv/*.pdf" $ do
            route idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route idRoute
            compile compressCssCompiler

        match "slides/*.md" $ do
            route $ setExtension "html"
            compile $
                pandocSlideCompiler
                    >>= loadAndApplyTemplate "templates/slides-reveal.html" slidesCtx

        -- should factorize templates application
        match "cours/*.md" $ do
            route $ setExtension "html"
            compile $
                pandocSlideCompiler
                    >>= loadAndApplyTemplate "templates/cours.html" postCtx
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/default.html" postCtx

        match "posts/*" $ do
            route $ setExtension "html"
            compile $
                pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= saveSnapshot "content"
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/default.html" postCtx

        match "pages/*" $ do
            route $ (gsubRoute "pages/" (const "")) `composeRoutes` setExtension "html"
            compile $
                pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/page.html" postCtx
                    >>= saveSnapshot "content"
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/default.html" postCtx

        match "drafts/*" $ do
            route $ setExtension "html"
            compile $
                pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= saveSnapshot "content"
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/draft.html" postCtx

        match "training/*.md" $ do
            route $ setExtension "html"
            compile $
                pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/training.html" postCtx
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/default.html" postCtx

        create ["atom.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx `mappend` bodyField "description"
                posts <-
                    fmap (take 10) . recentFirst
                        =<< loadAllSnapshots "posts/*" "content"
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
homeCtx =
    titleField "Welcome"
        <> defaultContext

postCtx :: Context String
postCtx =
    escaped "title"
        <> escaped "subtitle"
        <> field "slug" slugify
        <> dateField "date" "%B %e, %Y"
        <> defaultContext

slidesCtx :: Context String
slidesCtx =
    escaped "title"
        <> escaped "subtitle"
        <> field "revealjs-url" revealJsUrl
        <> dateField "date" "%B %e, %Y"
        <> defaultContext
  where
    revealJsUrl = const $ return "/reveal.js-3.8.0"

slugify :: Item String -> Compiler String
slugify item = do
    metadata <- getMetadata (itemIdentifier item)
    let slug = dropExtension . takeFileName . toFilePath
    return $ slug (itemIdentifier item)

--------------------------------------------------------------------------------
postList sortFilter = do
    list <- loadAll "posts/*"
    posts <- sortFilter list
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl homeCtx posts

stripPrefix :: Routes
stripPrefix = customRoute stripPrefixRoute
  where
    stripPrefixRoute ident = dropFirstDirectory p </> takeFileName p
      where
        p = toFilePath ident
    dropFirstDirectory = joinPath . tail . splitPath . takeDirectory
