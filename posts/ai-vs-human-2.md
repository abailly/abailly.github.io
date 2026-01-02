---
title: Claude Code vs. Hand-written Code - Part 2
description: Another experiment where I contrast developing the same feature using Claude Code agent versus manually coding it.
author: Arnaud Bailly
date: 2026-01-01
---

In a [previous blog post](https://pankzsoft.leaflet.pub/3mb4wklp2zx7t) I wrote about a first experiment I ran comparing code written manually with code written autonomously by [Claude Code](https://claude.com/product/claude-code) from some prompt. I concluded that I was impressed by the quality of the result and the speed with which Claude was able to solve the problem: It found a solution which was as good or maybe even better than mine, it wrote more tests, and it did all that in about 20 minutes where it took me 2-3 hours.

But I wanted to see how it fared with something more substantial, a feature that would require changes across different modules and layers of the system, rather than one focusing on a single algorithmic problem. This article summarizes this second experiment and the conclusion I drew from it. For the impatient reader, I can already tell this second experiment was almost as impressive as the first one: Claude Code produced a solution to the problem in a fraction of the time it took me to implement it, including tests. However, the code could have been better: Some repetition could have been factored away, and it went overboard with image extraction logic, implementing features that were not needed.

## The problem

Up to that point I had been able to publish markdown blog posts to [Leaflet](https://pankzsoft.leaflet.pub) only if they did not contain images: Image markup `![some title](some/path/or/url)` was simply `undefined` hence it blew up the parser when hit. But I wrote blog posts with images and I do want to be able to migrate those and of course publish new posts with whatever images I fancy. The goal, therefore, was to implement a feature that could be expressed quite simply as: "I want to publish or update articles that contain link to images, such that those images are displayed on `leaflet.pub` when browsing the published article."

While the goal is straightforward, the implementation is a tad involved:

* [commonmark-hs](https://github.com/jgm/commonmark-hs/blob/master/commonmark/src/Commonmark/Types.hs#L93) parses images as _inline_ elements, but the [Leaflet document](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/pages/linearDocument.json#L36) format expects images to appear as _blocks_, so some post-processing of parsed elements has to happen,
* Images in leaflet's `ImageBlock` are _blobs_ referenced by their [CID](https://atproto.com/specs/data-model), which means Sensei has to upload the blob first, and use the result of the upload as the `Image` inside the block. Note that all those elements could be computed locally assuming the image's data is available, but it would nevertheless have to be uploaded to be displayed properly,
* Therefore, image references have to be _resolved_ before article's publication, which requires reading or downloading files, then uploading them to the PDS.

## Manual implementation

> The final implementation I wrote "by hand" is available in [this branch](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad:z3PQYrvBT8B2zP3XdGoXdUd2Enftc/remotes/z6MkhgPg6WShnhJcmfwox4G5yL3EvJ2zW8L31SZLD95yUi11/tree/human-publish-image).

My first step was to write down a test in `LeafletSpec` to ensure sensei could parse markdown articles referencing images. From this test I was lead to implement the `Image` data structure. I started with the exact lexicon specification, including `Blob`s, as I knew all the data could be computed locally but quickly realised this would imply the images resolution process would have to happen in the parser, which was somewhat annoying as interleaving complex effectfult actions with parsing would lead to scope creep and breaking the layers' abstraction between various stages of the publication process.

I therefore turned the `image` field of `Image` as an `ImageSource`, a sum type which could either be a `Stored Blob` or an arbitrary `Ref Text`: In markdown, a link is not necessarily a URL so I deferred to the resolution process its validation and handling. Because I did not have the image's data available at parsing time, I also needed to set a default `AspectRatio`, which I fixed to be `AspectRatio 4 3` to be later updated. I added tests for serialisation which required some changes in `Generators`.


While implementing `Image` serialization to/from JSON I wasted about 30' scrathing my head over a puzzling _`key not found` error_ failure in roundtrip test, only to find out I had inverted two fields in the `parseJSON` function! This was a reminder that positional construction of data structures is fraught with perils and should be avoided.

Instead of

```
instance FromJSON Image where
  parseJSON = withObject "Image" $ \v ->
    Image <$> v .: "aspectRatio"
          <*> v .: "image"
          <*> v .:? "alt"
```

I ended up writing

```
instance FromJSON Image where
  parseJSON = withObject "Image" $ \v -> do
    aspectRatio <- v .: "aspectRatio"
    image <- v .: "image"
    alt <- v .:? "alt"
    pure $ Image {..}
```

Note that while `RecordWildcards` is generally frowned-upon, I think it's fine in this case: A missing field will trigger a warning, and I have `-Wall -Werror` as default GHC options.

Once this problem was solved, writing the parsing process was "easy":

* Introduced a new `Img Text Text` constructor for `Inline` elements,
* Then extracted all inline `Img` elements in the `extractFacet` function that was used by block elements,
* And finally added the collected images, if they existed, as `ImageBlock` _after_ the block they would have been collected in.

This obviously makes the transformation destructive and once more highlights the _impedance mismatch_ between a Leaflet document and the implicit tree structure expected by the `commonmark-hs` parser. Not a big deal in my case but this makes the parser less general than one could wish.

I then moved "up" the chain and tackled image resolution, blob uploading, and document post-processing. I segregated this logic into a `resolveImages` function (should have been named `resolveAndUploadImages`) passing it some functions to make it easier to write tests while stubbing (or is it mocking, or faking maybe 🤔?) the actual I/Os. I went for local file images resolution first as it was simpler, and then wrote more tests to cover I/O failures (Eg. `FileNotFound`).

Here is how the happy path test case looks like:

```haskell
 it "upload and resolve local images" $ do
   bytes <- BS.readFile "test/image.png"
   let expectedCID = computeCID bytes
   let Right document = mkMarkdownDocument "![test image](test/image.png)"
   resolved <- resolveImages (const $ pure bytes) successfulBlobUploader document

   case resolved of
     Right LinearDocument {blocks = [Block {block = ImageBlock Image {image, aspectRatio}, alignment = Nothing}]} -> do
       image `shouldBe` Stored Blob {ref = BlobRef expectedCID, mimeType = "image/png", size = 3798}
     other -> fail $ "Unexpected document resolution result: " <> show other
```

Working on this part I definitely realised how messy the testing of Bluesky protocol logic was, and this should be the focus of a major refactoring to implement a proper _simulator_ of a PDS.

Another (late) realisation that dawned on me when I wanted to test the publication logic is that the image resolution I had implemented would not work on the server. Sensei implements a client-server architecture, with an eye towards being able to replicate changes between various instances for decentralisation and team-work: The client calls an [API]() with the raw data and the whole processing happening on the server. An image referencing a file, whether absolutely or relatively, would therefore not work on the server which means I needed to implement web-based resolution, e.g. make sure images referenced through `http(s)` would be downloaded, and then uploaded as blobs.

I did some refactoring first, making the `mkMarkdownDocument` function pure as IO was only useful to assign a TID which would be reassigned upon publication, then wrote a test introducing an `imageDownloader` that would use the `ClientMonad` machinery to resolve requests against an actual WAI server running around the test. I wasted some time figuring out some type error which was caused by the use of wai's `with` which has `()` as its state while the test Servant client requires a `Maybe (Encoded Hex)` to hold some state. And then figured while threading the needed function into the publication logic that this was way overkill and a simple monadic handle `Text -> IO ByteString` would be just fine to abstract away the whole image resolution logic.

Adding the code to download the image required some perusing of the documentation on [Hackage](https://hackage.haskell.org) but was mostly straightforward. I then spent some more time refining the error handling logic, with a basic strategy: lower level functions were expected to throw various exceptions which are handled by the `resolveImages` function which turns those exceptions into a `Either ImageResolutionError a`:

```haskell
resolveImages ::
  forall m.
  (MonadIO m, MonadCatch m) =>
  (Text -> m BS.ByteString) ->
  (BS.ByteString -> m BlobUploadResponse) ->
  LinearDocument ->
  m (Either ImageResolutionError LinearDocument)
resolveImages imageResolver blobUploader LinearDocument {id, blocks} =
  (Right . LinearDocument id <$> mapM resolveBlock blocks)
    `catches` [ Handler $ \(err :: IOError) ->
                  pure $ Left $ FileNotFound $ Text.pack $ show err,
                Handler $ \(err :: ServerError) ->
                  pure $ Left $ FileNotFound $ Text.pack $ show err,
                Handler $ \(err :: ClientError) ->
                  pure $ Left $ ImageUploaderError (Text.pack $ show err),
                Handler $ \(err :: CIDError) ->
                  pure $ Left $ InvalidCID err
              ]
```

I was finally able to publish a document containing an image, but the image was not displayed. Looking at the source HTML I found out the image was there, but it had a size of 4x3 pixels! This obviously was caused by the fixed `AspectRatio 4 3` which the renderer interprets literally as meaning `4px x 3px`! It seems from [this example](https://fireside.leaflet.pub/3mb6ao3m5hs2a) that if the width/height are set very large, they are actually displayed within the bounds of the enclosing text, so it works like a true aspect ratio and not a definition of the bounds of the image.

So it seems the code should get some info about the image and not only the raw bytes, which is somewhat annoying. A quick search on Hackage lead me to [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture.html) which can easily decode images and give me its width and height, and using `decodeImage` on the raw downloaded bytes was easy enough. I was then able to publish most of the articles that had images from my [old blog](https://abailly.github.io) to my [leaflet blog](https://pankzsoft.leaflet.pub), but for one article using a Wikipedia image that does not seem to be available anymore, or requires handling redirection logic which I do not care about right now.

All in all, it took me about 4-5 hours spread over 2 days, interspersed with preparation for New Year's Eve dinner and various family activities. Here is the list of commits I did:

```
* c9e80ee (rad/master) Can read image's size and update it before publication
* bf9f2bb Group image resolution I/O into a single function
* 18c63c2 Can resolve local or remote images
* 7689e0b Make markdown conversion pure
* a239349 Wire in blob upload in publish/update
* 63df4db Refactor to use exceptions in sub functions
* 6536881 Handle reading and uploading file and some error cases
* 973fb16 Can resolve a local file image given upload succeeds
* e0dc9ff Added test for resolving local images
* 50468d6 Convert lonely markdown image to block
* 4231a4e Convert markdown inline image to leaflet block
* fb46c12 First failing test for markdown image parsing
* a71bd5a Implement Image type and JSON serialisation
```

## Claude Implementation

I reverted my local workspace to the commit before `a71bd5a` and then prompted Claude to autonomously implement the exact same feature. Here is the prompt I used:

> It's currently not possible to publish or update articles from markdown when the source document contains images, eg. with markup `![some title](path/or/url)`, and the corresponding parsing function in the Markdown module is `undefined`. I would like you to implement this feature. The acceptance test would be that one can use publishArticle or updateArticle functions, passing it a markdown  document referencing images, and the document should appear in the PDS with the images correctly referenced as Blob. This requires the following changes:
>
> 1. fully implement ImageBlock and Image data structure as per their specification (lexicon), making sure serialisation roundtrip works correctly and updating the Generators for Article to include images
> 2. fix the Markdown parser to allow images. Note that because images are parsed as Inline elements but leaflet `LinearDocument` only accepts `ImageBlock` you will need to modify the Inline structure and to post-process all block elements that contain inline elements to "lift" the parsed images as blocks
> 3. in the publishArticle and updateArticle, after the document has been converted, post-process it to resolve the images it contains:
>    a. for each image thereof, you will need to retrieve it either from local file storage or a remote URL
>    b. then compute its aspect ratio to fix it in the Image data structure
>    c. finally, the bytes comprising the images should be uploaded as Blob using the uploadBlob function and the returned content if successful used to populate the Image's image field with the correct mimeType, size, and CID
>
> Please make sure that each of the various steps are unit tested and possible errors are taken into account as unit tests.
>
> Before making changes, make a detailed plans of the anticipated changes and I will review it.

I used Simon Willison nice [transcript export tool](https://github.com/simonw/claude-code-transcripts) to publish the full transcript [here](https://www.punkachien.net/claude-transcript-1/) of the session. The most interesting part is the plan which Claude Code was able to define before doing any work:

```
Summary

The plan implements image support in 5 phases:

* Data Structures - Add AspectRatio, complete Image type per lexicon, create UnresolvedImage for parsing
* Markdown Parser - Add ImageInline variant, implement image parsing, lift images from inline to block level
* Image Resolution - New module with resolveImage function using JuicyPixels for decoding, handles files/URLs
* Integration - Add resolveDocumentImages to fetch/upload images in publishArticle/updateArticle
* Testing - Comprehensive unit tests, parser tests, integration tests, error cases

Key Design Decisions

* UnresolvedImageBlock intermediate type - Cleanly separates parsing from resolution
* Relative paths use _articleDir - Current working directory as base for relative file paths
* Fail-fast on errors - Don't publish articles with failed image uploads
* JuicyPixels dependency - Well-established library for image decoding
* Comprehensive error types - Clear error messages for each failure mode

The plan is detailed enough to execute while remaining scannable. All critical files are identified with specific line numbers for changes.
```

It took about half-an-hour to Claude to implement that plan, without any supervision from my part once I approved the plan. The code it produced is the branch [ai-publish-image](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad:z3PQYrvBT8B2zP3XdGoXdUd2Enftc/remotes/z6MkhgPg6WShnhJcmfwox4G5yL3EvJ2zW8L31SZLD95yUi11/tree/ai-publish-image) on the repository.

Here are a few highlights I took not of about the code produced:

* It followed more or less the same path than I did: First implement the data structures, then tackle markdown parsing, image resolution and uploading and final integration in documents publication
* It chose to introduce a dedicated block, `UnresolvedImageBlock UnresolvedImage`, to implement the two-step process of first parsing the document then later resolve the actual `Image` data structure, producing an error when generating JSON in order to "ensure" the document would not be publishable. This is problematic because it makes the use of `md2leaflet`, a simple executable that allows one to check the output of the conversion process, unusable,
* Instead of adjusting the `extractFacet` function, it created another function extracting images as a separate processing of the list of `Inline` elements. This is arguably somewhat cleaner and clearer,
* It also added a dedicated constructor to represent parsed images in `Inline` data-type,
* It created a dedicated `Image` module to take care of images resolution, with a dedicated `ImageSpec` module for testing, which is pretty cool and a great idea I should have had myself. It added a comprehensive set of tests covering local files resolving, but left aside tests for HTTP resolution just like me.
  * Also of note the fact it added a dependency to `http-conduit` to handle HTTP download where I used the lower-level `http-tls-client`
* It however went completely overboard with image analysis and JuicyPixels, extracting not only the image sizes but also their MIME types, which is quite useless as the type is actually detected when the file is uploaded as a blob. This lead Claude Code to write a significant number of unit tests to cover various popular image formats and the corresponding logic in `Image` module,
* Integration in the `publishArticle` and `updateArticle` functions was trivially handled, calling the main `resolveAndUploadImages` function. The main issue I have with this one is that those functions return an `Either String a`, eg. errors are not categorized and their propagation left to the caller. To be fair, error handling I wrote is far from being perfect in the `Bsky` module and something I should improve along with better logging and some metrics.

## Conclusion

I have been again quite impressed by Claude Code's ability to implement a complete feature without any supervision, from a single initial prompt. However I think the end result has some shortcomings that make it not better and actually slightly worse than the code I wrote myself, which could definitely be improved but goes straight to the point, does not introduce additional dependencies and does not break other code.

What I gather from these experiments, and from informed comments and reports here and there, is that letting an agent run loose to code something relatively involved is not the best use of such tools in the context of an existing codebase. It might make more sense when prototyping, or implementing some side feature (I have used Claude Code to build an End-to-End test suite based on playwright on another project and it was fine) which can be replaced at will. But when working on a system for an extended period, the feedback loop should be much shorter and the agent (at least as far as coding is concerned) should be used just like a pairing partner, drafting and revising a plan together, implementing small features easily reviewed and changed one at a time, correcting course, and so on.

While this certainly makes the time ratio less impressive, it still shave a significant amount of time actually typing stuff or implementing boring boilerplate, while giving more opportunity to reflect about the work being done.
