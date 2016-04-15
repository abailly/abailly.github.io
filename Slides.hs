module Slides where

import qualified Data.Set            as S
import           Hakyll
import           Text.Pandoc.Options


-- | Generate slides from given input file
pandocSlideCompiler :: Compiler (Item String)
pandocSlideCompiler = pandocCompilerWith defaultHakyllReaderOptions writeHtmlSlide
  where
    writeHtmlSlide = defaultHakyllWriterOptions { writerIncremental = True
                                                , writerSlideVariant = RevealJsSlides
                                                }
