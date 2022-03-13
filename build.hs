#!/usr/bin/env stack
-- stack runghc --package shake --

import Control.Monad (forM)
import Data.Function (on, (&))
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (catMaybes)
import Development.Shake
import Development.Shake.FilePath (dropDirectory1)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.FilePath
import System.Posix.User (getRealUserID)

options =
  shakeOptions
    { shakeReport = ["report.html"],
      -- Generate a report that can be opened in a browser to analyze build
      shakeThreads = 0,
      -- Use all CPUs provided by the platform
      shakeFiles = "_build",
      -- Put Shake's database in directory '_build'
      shakeChange = ChangeDigest
      -- Rebuild only if content changes, not if date/time changes
    }

main = do
  pwd <- getCurrentDirectory
  uid <- toInteger <$> getRealUserID
  putStrLn $ "Building site `in directory " <> pwd <> " as user " <> show uid
  args <- getArgs
  withArgs args $ runShake pwd uid

runShake :: FilePath -> Integer -> IO ()
runShake pwd uid = shakeArgs options $ do
  want ["_site/index.html"]

  -- clean generated files
  phony "clean" $ do
    putInfo "Cleaning files in _site"
    removeFilesAfter "_site" ["//*"]

  -- build index file, listing all posts
  "_site//index.html" %> \index -> do
    cs <- getDirectoryFiles "posts" ["//*.md"]
    let posts = ["_site" </> "posts" </> c -<.> "html" | c <- cs]
    need ["template-index.html"]
    need posts
    makeIndex cs
    cmd
      "pandoc"
      [ "-o",
        index,
        "--template",
        "template-index.html",
        "-s",
        "_site/content.html"
      ]

  -- build post entries
  "_site//posts//*.html" %> \html -> do
    need ["template.html"]
    let post = dropDirectory1 $ html -<.> "md"
    need [post]
    cmd
      "pandoc"
      [ "-o",
        html,
        "--template",
        "template.html",
        "-s",
        post
      ]

makeIndex :: [FilePath] -> Action ()
makeIndex posts = do
  titlesAndDates <-
    catMaybes
      <$> forM
        posts
        ( \post -> do
            titles <- filter (\l -> "title: " `isPrefixOf` l || "date: " `isPrefixOf` l) . lines <$> readFile' ("posts" </> post)
            let href = "/posts" </> post -<.> "html"
            case titles of
              [t, d] -> pure $ Just (href, t, d)
              _ -> pure Nothing
        )
  let postList =
        concatMap formatEntry $ sortBy reversedDate titlesAndDates

      formatEntry (href, t, d) =
        "<li>"
          <> "<a href=\""
          <> href
          <> "\">"
          <> drop 7 t
          <> " - "
          <> drop 5 d
          <> "</a></li>"

      reversedDate (_, _, d) (_, _, d') = compare d' d

  writeFile' "_site/content.html" ("<ul>" <> postList <> "</ul>")
