This blog uses [shake](https://www.shakebuild.com/manual) and [pandoc](https://pandoc.org) to generate static HTML files from markdown documents.

For historical reasons, it uses Git branches and submodules to separate source tree from actually published files in the same repository:
* The `hakyll` branch contains the source files
* The `master` branch contains the actual site's content.

**TODO**: Simplify this...

## Requirements

* Install [stack](https://www.haskellstack.org)
* Install Pandoc

## Build

* run `./build.hs` in the directory, this will put all files into `_site` directory

## Publish

* commit and push `_site` directory content to `master`
