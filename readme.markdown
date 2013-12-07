This is the source for my [personal site](http://blaenkdenum.com).

The Hakyll site's source (the Haskell code in **src/**) is BSD licensed as stated in the cabal file.

Feel free to use everything else in **provider/**, but if you can help it please don't use the site's style (in **provider/scss**) and instead use your own, or at the _very least_ differentiate the style ***as much as you can***. I created this style for this _personal_ site, so I'd prefer that it remains unique!

Features:

* nice urls, e.g. .com/posts/some-post and .com/some-page, no trailing 'index.html'
* easy page and post creation (place in **posts/** or **pages/**)
* post excerpts in index pages
* notes system, pretty much same as regular posts but with **/notes/** path
* custom deploy bash script for github pages setup, `./site deploy`
    * `src/deploy.sh setup` for initial configuration
* drafts system for posts, pages, notes --- stored in **drafts/**
    * `./site watch` and `./site preview` will include **drafts/** items in generated site
    * everything else, such as `./site deploy`, assumes non-drafts
    * `./site clean preview` cleans generated "preview site," including drafts
* tags support with pretty and "slugified" versions of tags
    * i.e. in metadata specified as "Linux", shows up this way in posts, but tag page is **/tags/linux/**
* scss consolidation, e.g. edit any one scss file, all of them get re-compiled and merged
* abbreviation support. abbreviations are defined as `*[GHC]: Glasgow Haskell Compiler`
* table of contents generation with pretty section numbers
    * table of contents section renaming: `# Some Header {toc="show this way in toc"}`
    * table of contents numbering with pure CSS
    * header section numbering with pure CSS
* pygments highlighting of codeblocks with caching
* correctly ignores the file "4913" that vim creates on Windows to test if the file is writeable
* blockquotes with pretty borders
* atom feed generation

Todo:

* possible optimization for pygments: combine all similar language code blocks into one "file"
