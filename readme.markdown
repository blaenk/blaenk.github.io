This is the source for my [personal site](http://www.blaenkdenum.com).

The Hakyll site's source (the Haskell code in **src/**) is BSD licensed as stated in the cabal file.

Feel free to use everything else in **provider/**, but if you can help it please don't use the site's style (in **provider/scss**) and instead use your own, or at the _very least_ differentiate the style ***as much as you can***. I created this style for this _personal_ site, so I'd prefer that it remains unique!

If you use vim, I highly recommend the [vim-pandoc-syntax](https://github.com/vim-pandoc/vim-pandoc-syntax) plugin for better Pandoc-flavored markdown editing, complete with codeblock highlighting and concealments.

Features:

* ["live" editing](http://blaenkdenum.com/posts/live-editing-with-hakyll/); see changes as you save them, in-place (Haskell STM Channels -> HTML5 WebSockets)
* fully integrated with MathJax using `$` inline and `$$` block delimiters
* nice urls, e.g. .com/posts/some-post and .com/some-page, no trailing 'index.html'
* easy page and post creation (place in **posts/** or **pages/**)
* default index page groups posts chronologically by year, where first group (for current year) doesn't show year heading
  * post excerpts in index pages with metadata `excerpt: some excerpt`
* notes system, pretty much same as regular posts but with **/notes/** path and no tags
* custom deploy script for github pages setup, `./site deploy`
    * `src/deploy.sh setup` for initial configuration
* drafts system for posts, pages, notes --- stored in **drafts/**
    * `./site watch` and `./site preview` will include **drafts/** items in generated site
    * everything else, such as `./site deploy`, assumes non-drafts
    * `./site clean preview` cleans generated "preview site," including drafts
* tags support with pretty and "slugified" versions of tags
    * i.e. in metadata specified as "Linux", shows up this way in posts, but tag page is **/tags/linux/**
* scss consolidation, e.g. edit any one scss file, all of them get re-merged into single scss file
* abbreviation support, defined as `*[GHC]: Glasgow Haskell Compiler`
* table of contents generation with pretty section numbers
    * can left-align with metadata `toc: left` (right is default)
    * can turn off toc completely with `toc: off`
    * section renaming: `# Some Header {toc="show this way in toc"}`
    * section ignoring (don't include in toc and don't number): `# Some Header {.notoc}`
    * table of contents section numbering with pure CSS
    * header section numbering with pure CSS
* pygments highlighting of codeblocks
    * UTF-8
    * very fast, server-backed implementation
    * produces simple, clean, and [semantically correct](http://www.w3.org/TR/html5/grouping-content.html#the-pre-element) markup

            <pre><code class="highlight language-haskell">the code</code></pre>

    * uses first class for language specification, `{.haskell}`, or optional `{lang=haskell}` keyvar, or alternative backtick codeblock syntax:

            ``` haskell
            putStrLn "Hey"
            ```

    * optional caption with `text="some caption"` keyvar
* correctly ignores the file "4913" that vim creates on Windows to test if the directory is writeable
* blockquotes with pretty borders
* atom feed generation
* disqus comments on by default, can turn off with metadata `comments: off`
    * only loads the JS when comments are on
* github history integration
    * show the commit that most recently altered a given post in its footer, as well as a link to its overall history on github
