module Slides where

import qualified Data.Set as S
import Hakyll
import Text.Pandoc.Options

-- | Generate slides from given input file
pandocSlideCompiler :: Compiler (Item String)
pandocSlideCompiler = pandocCompilerWith defaultHakyllReaderOptions writeHtmlSlide
  where
    writeHtmlSlide =
        defaultHakyllWriterOptions
            { writerIncremental = True
            , writerSectionDivs = False
            , writerSlideLevel = Just 2
            , -- , writerSlideVariant = RevealJsSlides
              -- , writerIgnoreNotes = True
              -- , writerHtml5 = True
              writerHTMLMathMethod = MathML
              -- , writerHighlight = True
            }
