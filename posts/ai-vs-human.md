---
title: Claude Code vs. Hand-written Code
author: Arnaud Bailly
date: 2025-12-29
---

In a [previous blog post](./leaflet.html) I wrote about my experience extending [Sensei](https://abailly.github.io/sensei) to handle publishing posts like this one to [Leaflet](https://pankzsoft.leaflet.pub). This was an opportunity to dive deeper into AI-assisted development with [Claude Code](https://claude.com/product/claude-code) which I also wrote about. In this post I want to recount a more focused experiment I did to understand how Claude code fares on some more complicated issue, and to compare its output with my own efforts to solve the same problem.

## The problem

The issue I wanted to solve lies in the Markdown-to-Leaflet format conversion, and more precisely in how text decorations, called _facets_ in Leaflet, are handled by the conversion process.

### Leaflet document structure

In Leaflet, a document is represented as a list of _pages_ which can either be _canvas_ (which I do not implement) or _linear documents_. The latter is a sequence of _blocks_ serving different purposes like _image blocks_, _blockquotes_, _Code blocks_ or _rich text blocks_. And a _rich text block_ which can be equated to a _paragraph_ has two main elements: The text of the paragraph, in a field called `plaintext`, and a sequence of `facets`.

A `facet` is in turn made of a _type_ (`italic`, `bold`, `underline`, `code`...) and an `index` denoting the span of text the facet must be applied to, so for example the following fragment

```
This is some _italic_ text along with a [link](https://foo.bar.com) and a `code` word
```

would translate to :

```json
{
  "$type": "pub.leaflet.pages.linearDocument",
  "blocks": [
    {
      "$type": "pub.leaflet.pages.linearDocument#block",
      "block": {
        "$type": "pub.leaflet.blocks.text",
        "facets": [
          {
            "features": [
              {
                "$type": "pub.leaflet.richtext.facet#italic"
              }
            ],
            "index": {
              "byteEnd": 19,
              "byteStart": 13
            }
          },
          {
            "features": [
              {
                "$type": "pub.leaflet.richtext.facet#link",
                "uri": "https://foo.bar.com"
              }
            ],
            "index": {
              "byteEnd": 42,
              "byteStart": 38
            }
          },
          {
            "features": [
              {
                "$type": "pub.leaflet.richtext.facet#code"
              }
            ],
            "index": {
              "byteEnd": 53,
              "byteStart": 49
            }
          }
        ],
        "plaintext": "This is some italic text along with a link and a code word"
      }
    }
  ],
  "id": "3mb4eihgbj3qp"
}
```

So to sum things up, Leaflet's document have a 2-layers structure: Blocks and facets.

### UTF-8 and facets

The `index`'s field bounds are expressed in _bytes_ over the `plaintext` content which is, itself, represented in UTF-8. This seems a bit weird to me and I don't really understand the rationale behind this choice. Practically speaking this means that the following text with some non-ASCII characters

```
    Cette phrase à des caractères _accentués_
```

will yield the following Leaflet document:

```json
{
    "$type": "pub.leaflet.pages.linearDocument",
    "blocks": [
        {
            "$type": "pub.leaflet.pages.linearDocument#block",
            "block": {
                "$type": "pub.leaflet.blocks.text",
                "facets": [
                    {
                        "features": [
                            {
                                "$type": "pub.leaflet.richtext.facet#italic"
                            }
                        ],
                        "index": {
                            "byteEnd": 42,
                            "byteStart": 32
                        }
                    }
                ],
                "plaintext": "Cette phrase à des caractères accentués"
            }
        }
    ],
    "id": "3mb4gfidghsxp"
}
```

Note the `index` offsets are now 32 and 42 instead of 30 and 39, to account for the supplementary bytes needed in UTF-8 encoding for the accented characters `à`, `è` and `é`.

### Markdown parsing

To convert from Markdown to Leaflet format, I am using the parser from [commonmark-hs](https://github.com/jgm/commonmark-hs/) library which is maintained by John McFarlane of [pandoc](https://pandoc.org) fame, so I trust him to do "The Right Thing" when it comes to markdown conversions.

This parser comes with a "hook" mechanism based on two typeclasses, `IsInline` and `IsBlock` which allow the user to plug any representation they wish. What's slightly annoying is that both typeclasses require a `Monoid` constraint which means I cannot directly use the `Block` and `Facet` types from the `Leaflet` module, but I need to wrap them. Because of the structure of the `IsInline` typeclass I ended up using a small wrapper around facets and text:


```haskell
data Inline = Plain Text | Decorated Feature (Maybe SourceRange)
```

and implementing the typeclasses using lists, eg. `[Inline]` and `[Block]` which automatically fills in the `Monoid` constraint.

Another typeclass, `Rangeable` must be implemented to allow the parser to assign a range to each element, which explains the `Maybe SourceRange` component in the `Decorated` constructor.

Recall that Markdown only breaks paragraphs, ie. "text blocks", on two consecutive line breaks, which allows editors to break long lines intos smaller, more readable, lines while still retaining the logic of the document's sectioning. This has the important consequence that `SourceRange` is expressed as a couple of `(line, column)` pairs. Moreover, it's 1-based _both_ for lines and columns.

Conceptually the parser is more or less assuming the concrete representation is tree-shape, e.g. is congruent with HTML or similar representation, which is not the case of Leaflet but all in all it works. There are a few warts and infelicities, for example in the way Leaflet does not allow `Blockquote` blocks in lists, or does not support ordered lists, etc. but it's mostly fine with the content I have.

### The bug

The main issue I wanted to solve is the fact that text decorations appearing in multiline paragraphs ended up being _off_ which manifests itself visually by text attributes like emphasis, bold, or links to be shifted by a few characters. This was particularly annoying in the posts written in French where obviously the chances of using multibyte-encoded characters are higher.

The problem can therefore be summarized as: Translate 1-based, line/column based, character offsets into 0-based, linear, byte offsets.

## The solutions

### Manual coding

It took me a 2-3 hours of combined work to solve the problem myself. I already had in place a test which showed the offset computation as bytes was correct for single line blocks:

```
    it "computes facets offsets as bytes not characters" $ do
      let markdown = "Un entier est ici construit à l'aide de la méthode `succ` et de la constante `Zero`:"
      ...
```

I therefore first started to add a bunch of tests related to the computation of facets offsets for multiline and multibyte texts, covering different cases:

The case where the decorated text itself contains multibyte characters:

```
    it "computes facets offsets as bytes not characters (2)" $ do
      let markdown = "une critique dite _démocratique_"
      ...
```

and the multiline blocks case:

```
    it "computes facets offsets as bytes not characters (3)" $ do
      let markdown =
            Text.unlines
              [ "une critique dite démocratique",
                "et une autre ligne ajoutée avec une emphase _ici_."
              ]
      ...
```

I also added a test case with more than two lines which is somewhat redundant with the previous one, but came from the suspicion my computations were off because of the _number_ of lines involved.

```
    it "computes facets offsets as bytes not characters (4)" $ do
      let markdown =
            Text.unlines
              [ "une première ligne avec du **bold**",
                "*une* critique `dite` démocratique",
                "et une autre ligne ajoutée avec une emphase _ici_."
              ]
      ...
```

The code I started with was the following:

```haskell
extractFacets :: [Inline] -> ([Facet], Text)
extractFacets inlines =
  let Converter {facets, plaintext} = foldl (flip extractFacet) initialConverter inlines
   in (facets, plaintext)

extractFacet :: Inline -> Converter -> Converter
extractFacet = \case
  Decorated f rge -> makeFacet f rge
  Plain "\n" -> \Converter {facets, plaintext} ->
    Converter {markup = 0, lastLine = BS.length (encodeUtf8 plaintext) + 1, facets, plaintext = plaintext <> " "}
  Plain t -> \Converter {plaintext, ..} -> Converter {plaintext = plaintext <> t, ..}
  where
    makeFacet f rge Converter {markup, lastLine, facets, plaintext} =
      Converter {markup = markup', lastLine, facets = facets <> [Facet {features = [f], index}], plaintext}
      where
        markup' =
          markup + case f of
            Code -> 2 -- `...`
            Italic -> 2 -- `*...*` or `_..._`
            Bold -> 4 -- `**...**`
            Link uri -> 4 + Text.length uri -- [...](uri)
            _ -> 0
        index = maybe (ByteSlice 0 0) toByteSlice rge
        toByteSlice (SourceRange ((beg, end) : _)) =
          ByteSlice (lastLine + sourceColumn beg - 1 - markup) (lastLine + sourceColumn end - 1 - markup')
        toByteSlice (SourceRange []) = ByteSlice 0 0
```

The `Converter` structure looked like the following:

```haskell
data Converter = Converter
  { -- | Accumulated markup characters in markdown source for current line
    markup :: Int,
    -- | Text length of last line seen (in bytes)
    lastLine :: Int,
    -- | Accumulated list of facets
    facets :: [Facet],
    -- | Accumulated plain text
    plaintext :: Text
  }
```

Note the comment for `lastLine` is misleading as it actually accounts not _only_ for the last line but also for all the previously seen lines.

Running those tests expectedly fails:

```
Running 1 test suites...
Test suite sensei-test: RUNNING...

Sensei.Bsky.Leaflet
  Markdown to Leaflet conversion
    computes facets offsets as bytes not characters (1) [✘]
    computes facets offsets as bytes not characters (2) [✘]
    computes facets offsets as bytes not characters (3) [✘]
    computes facets offsets as bytes not characters (4) [✘]

Failures:
  ...
  test/Sensei/Bsky/LeafletSpec.hs:140:22:
  2) Sensei.Bsky.Leaflet, Markdown to Leaflet conversion, computes facets offsets as bytes not characters (2)
       expected: [Facet {
                   index = ByteSlice {
                     byteStart = 18,
                     byteEnd = 31
                   },
                   features = [Italic]
                 }]
        but got: [Facet {
                   index = ByteSlice {
                     byteStart = 18,
                     byteEnd = 30
                   },
                   features = [Italic]
                 }]
```

After some time fiddling, thinking, and figuring out how the conversion should work on paper, I finally figured I had two problems:

* The accumulated lines length was computed in bytes, but the line/column ranges needed to be adjusted for the _difference_ between bytes and characters,
* The decorated fragment's difference between bytes and characters was not taken into account when computing the _end_ offset.

I therefore modified the `Converter` structure thus:

```haskell
data Converter = Converter
  { markup :: Int,
    -- | Accumulated text length of lines seen in (bytes, characters)
    accumulatedLinesLength :: (Int, Int),
    facets :: [Facet],
    plaintext :: Text
  }
```

and the `extractFacet` function to take into account the bytes/character difference:

```haskell
extractFacet :: Inline -> Converter -> Converter
extractFacet = \case
  Decorated f inner rge -> makeFacet f inner rge
```

I added an explicit `Newline` constructore to `Inline`, which accumulates bytes and characters seen so far:

```haskell
  Newline -> \Converter {facets, plaintext} ->
    Converter
      { markup = 0,
        accumulatedLinesLength = (BS.length (encodeUtf8 plaintext) + 1, Text.length plaintext + 1),
        facets,
        plaintext = plaintext <> " "
      }
  Plain t -> \Converter {plaintext, ..} -> Converter {plaintext = plaintext <> t, ..}
```

Some intermediate computations were left untouched:

```haskell
  where
    makeFacet f inner rge c@Converter {accumulatedLinesLength, plaintext = plaintextBefore} =
      Converter {markup = markup', accumulatedLinesLength, facets = facets <> [Facet {features = [f], index}], plaintext}
      where
        Converter {markup, facets, plaintext} = foldl (flip extractFacet) c inner
        markup' = ... -- left unchanged
        index = maybe (ByteSlice 0 0) toByteSlice rge
```

but to take into account the decorated `plaintext`, I computed two different bytes/characters pairs, one _before_ the decorated fragment and one _after_:

```haskell
        curlineLengthsBefore =
          bimap
            (BS.length (encodeUtf8 plaintextBefore) -)
            (Text.length plaintextBefore -)
            accumulatedLinesLength
        curlineLengths =
          bimap
            (BS.length (encodeUtf8 plaintext) -)
            (Text.length plaintext -)
            accumulatedLinesLength
```

These differences are then applied to the column offset provided by the parser (1-based, in characters) to compute the start and end linear offsets within the accumulated block of text

```haskell
        toByteSlice (SourceRange ((beg, end) : _)) =
          ByteSlice
            ( offsetSoFar
                + sourceColumn beg
                - markup
                + uncurry (-) curlineLengthsBefore
            )
            ( offsetSoFar
                + sourceColumn end
                - markup'
                + uncurry (-) curlineLengths
            )
```

### AI Coding

I then asked _Claude Code_ to solve the exact same issue, starting from the same point in a different branch. My prompt was ver ystraightforward:

> There are bugs in the way the index of each facet is computed: the computed ByteSlice in the Markdown module is only correct for single lines, but it's
> off when the facet appears in a multiline block of text. You should fix this in 2 steps:
>  1. write test cases exposing the faulty behaviour
>  2. fix the extraFacet function in Markdown module to compute correctly the offset in the case of multiple lines
>  You should stop editing after step 1. and let me review the tests written before going to step 2.

In the first step, it added a whole bunch of tests covering various cases for facets on multiple lines:

```
    it "correctly computes facet offset when emphasis is only on second line" $ do
      let markdown = "First line without markup\nSecond line with *emphasis* here"
    ...

    it "correctly computes facet offset when emphasis is at start of second line" $ do
      let markdown = "First line\n*emphasis* at start"
    ...
    it "correctly computes facet offsets for multiple facets on different lines" $ do
      let markdown = "Line one has *italic* text\nLine two has **bold** text\nLine three has `code` text"
    ...
    it "correctly computes facet offset for link on second line" $ do
      let markdown = "First line of text\nSecond line has a [link](https://example.com) here"
    ...
    it "correctly computes facet offsets when first line has markup and second has markup" $ do
      let markdown = "First line with *italic* word\nSecond line with **bold** word"
    ...
    it "correctly computes facet offsets with multiple markups on same line after newline" $ do
      let markdown = "Plain first line\nSecond with *italic* and **bold** text"
    ...
```

It also added a test to check `code` facet is properly applied on multiple words, which is indeed untested for `code` facet but is otherwise tested for other facets, so it not really needed.

```
    it "correctly computes facet offsets with code spanning across conceptual word boundaries" $ do
      let markdown = "Start\nAnother line with `code snippet` here"
    ...
```

And it finally added two tests for multibyte characters, one for French accented characters and another for longer Unicode characters like emojis:

```
    it "correctly computes facet offsets with UTF-8 multibyte characters" $ do
      let markdown = "Première ligne\nDeuxième avec *français* ici"
    ...
    it "correctly computes facet offsets with emoji characters across lines" $ do
      let markdown = "Hello 👋 world\nNext line has *emphasis* 🎉"
```

Those tests were failing as expected, so it proceeded to the second step, ie. fixing the code, leading to the following solution.
It first added a field in the `Converter` state to accumulate the text on the current line:

```haskell
data Converter = Converter
  { ...
    -- | Plaintext added on current line (for UTF-8 character-to-byte conversion)
    currentLinePlaintext :: Text
  }
  deriving (Show)
```

Then used the `currentLinePlaintext` field to convert offsets from characters to bytes only taking into account the difference for the current line as the previous lines are always accounted for in bytes:

```haskell
        toByteSlice (SourceRange ((beg, end) : _)) =
          let -- Character position in plaintext on current line
              charPosBeg = sourceColumn beg - 1 - markup
              charPosEnd = sourceColumn end - 1 - markup'
              -- Convert character positions to byte positions using current line's plaintext
              -- Take the prefix up to the character position and measure its byte length
              prefixBeg = Text.take charPosBeg currentLinePlaintext
              prefixEnd = Text.take charPosEnd currentLinePlaintext
              bytePosInCurrentLineBeg = BS.length (encodeUtf8 prefixBeg)
              bytePosInCurrentLineEnd = BS.length (encodeUtf8 prefixEnd)
           in ByteSlice (lastLine + bytePosInCurrentLineBeg) (lastLine + bytePosInCurrentLineEnd)
        toByteSlice (SourceRange []) = ByteSlice 0 0
```

Interestingly, it also uncovered a similar issue with the way facets are computed in `Blockquote` blocks, but it wasn't able to fix it, leaving some `TODO`s in the code instead. This issue actually [surfaces](https://pankzsoft.leaflet.pub/3mastatp7la74/l-quote/23_0-23_220#23_0) in my blog posts and is most manifest when some multibyte characters are cut by a `span`.

```haskell
    -- TODO: Blockquote facet starts are off by 2 bytes
    -- Should be: emphasis at 26-34, bold at 39-43
    -- But getting: emphasis at 28-34, bold at 41-43
    facets
      `shouldBe` [ Facet {index = ByteSlice 28 34, features = [Italic]},
                   Facet {index = ByteSlice 41 43, features = [Bold]}
                 ]
```

Even more interestingly, this problem stems from the fact the blockquote uses a different facet adjustment logic than the rest of the code, and it was introduced by Claude Code in an earlier development cycle.

## Conclusion

It would be silly to draw any kind of general conclusion from this small experiment, and all I can say is that I found the experiment demonstrated how effective LLM-based coding agents can be when given specific tasks. Comparing the code produced with mine, I find Claude's code better and easier to read, and its tests are more expressive and explicitly cover more cases. Obviously, the major benefit lies how fast Claude was able to produce its solution: 10-15 minutes vs. 2-3 hours is an impressive ratio.
