---
title: Post Feed in Hakyll
published: June 19, 2013
excerpt: Implementing an atom feed
tags: Hakyll, Haskell
toc: off
---

When I made my site, specifically when I [switched to Hakyll](/posts/the-switch-to-hakyll), I didn't bother to include a [syndication feed](http://en.wikipedia.org/wiki/Web_feed) because I didn't expect that anyone would care to want to subscribe to my site. However, someone [filed an issue](https://github.com/blaenk/blaenk.github.io/issues/1) concerning this on github. I knew Hakyll exposed a module specifically for this: [Hakyll.Web.Feed](http://hackage.haskell.org/packages/archive/hakyll/latest/doc/html/Hakyll-Web-Feed.html). It was more a matter of implementing it in a straightforward manner with the least duplication of work.

> I'd like to subscribe to your blog, but I can't seem to find an RSS feed (nor the Hakyll code to generate one). Would you consider adding one?
>
> <cite>**Nathan** on [Issue #1](https://github.com/blaenk/blaenk.github.io/issues/1)</cite>

## Considerations

If I used my custom post compiler, it would include the table of contents and Pygments highlighted code. This was a problem because the table of contents didn't work correctly in [the feed reader](https://yoleoreader.com/) I tested with and so just served to waste space. Worse, code blocks were completely absent from the feed reader. Finally, posts containing math type---which is rendered with [MathJax](http://www.mathjax.org/) on this site---did not render at all in the feed reader.

So it was obvious to me that I had to compile the posts meant for the syndication feed with a more vanilla Pandoc compiler. However, I did want to keep the abbreviation substitution filter as that seemed to work perfectly fine.

Because I needed to compile the posts with an entirely different Pandoc compiler, I knew that already I was duplicating some effort. Knowing this, I wanted to make sure to save as much work as possible to avoid further duplicate effort.

## Drying Up

Since I wanted to use the abbreviation substitution filter in both the feed and regular post compiler, I knew that it was a potential location of duplicate effort. Both compilers would start something like this:

~~~ {lang="haskell"}
compile $ getResourceBody
  >>= withItemBody (abbreviationFilter)
  >>= pandocCompiler -- or pandocFeedCompiler
~~~

So it would have been preferable if I could save the state of the `Item` (i.e. post) as it was right after abbreviation substitution. Fortunately, Hakyll has support for this in the form of _snapshots_.

The solution was to save a snapshot of the post from the regular compiler for posts after it had been through the abbreviation substitution [^1]:

~~~ {lang="haskell"}
match postsPattern $ do
  route $ niceRoute "posts/"
  compile $ getResourceBody
    >>= withItemBody (abbreviationFilter)
    >>= saveSnapshot "abbreviated"
    >>= pandocCompiler
    -- ...
~~~

## Implementation

This meant that I could now refer to the "abbreviated" snapshot of any post. All I had to do now was to define a `Rule` to compile posts specifically for the syndication feed. Hakyll also has support for this in the form of _versions_, in which one can compile different versions of the same thing and refer to them later on.

So what I do in the "feed" version of the post compiler was to get the underlying `Identifier` for the given post and load the "abbreviated" snapshot of the version of that post that has no name, i.e. the version of the post compiled by the regular post compiler.

I then pass that snapshot to `pandocFeedCompiler` which is simply a more vanilla Pandoc compiler that removes the table of contents sentinel value I use, doesn't generate the table of contents, doesn't highlight code with Pygments, and uses regular superscripts etc. instead of MathJax:

~~~ {lang="haskell"}
match postsPattern $ version "feed" $
  compile $ do
    ident <- getUnderlying
    loadSnapshot (setVersion Nothing ident) "abbreviated"
      >>= makeItem . itemBody
      >>= pandocFeedCompiler
~~~

All that was left to do was to create the `atom.xml` file. An ephemeral `Context` is created to denote that the `$description$` tag should be filled with the body of the post, as the syndication feed rendering functions in Hakyll expect. All "feed" versions of posts are loaded, sorted in reverse chronological order, and the first ten are taken. Finally the function `renderAtom` actually generates the XML from all of this information:

~~~ {lang="haskell"}
create ["atom.xml"] $ do
  route idRoute
  compile $ do
    let feedCtx = postCtx <> bodyField "description"
    posts <- fmap (take 10) . recentFirst
      =<< loadAll (postsPattern .&&. hasVersion "feed")
    renderAtom feedConf feedCtx posts
~~~

## Caveat

Notice that we are using the "feed" versions of posts to render the syndication feed. This poses a problem, because the [atom feed template](https://github.com/jaspervdj/hakyll/blob/master/data/templates/atom-item.xml) requires access to the `$url$` field, but notice that the "feed" version is _not_ routed.

This means that a `Route` is not created for "feed" versions, and as a result the `$url$` will be an empty string, so the link to individual stories in the feed will just link to the site root!

This becomes apparent when you look at the implementation of `urlField`, which is defined in [Hakyll.Web.Template.Context](http://hackage.haskell.org/packages/archive/hakyll/latest/doc/html/Hakyll-Web-Template-Context.html):

~~~ {lang="haskell"}
urlField :: String -> Context a
urlField key = field key $
    fmap (maybe empty toUrl) . getRoute . itemIdentifier
~~~

First it gets the `Item`'s `Identifier`, and then it gets that `Identifier`'s `Route`. The problem is that since we're using a different version of the post, the `Identifier` will be different, and there won't be a `Route` associated with that `Identifier`.

I actually use a slightly different `urlField`-type function, which I called [`niceUrlField`](https://github.com/blaenk/blaenk.github.io/blob/1379be96c66de626b2623d0b09ce32e065da4f49/src/Site/Fields.hs#L80), it simply returns the URL without the `index.html` at the end. However, the solution to this problem is the same in both functions.

The solution is to get the no-name version of the `Identifier` that is retrieved, that is, the version of the post without an explicit version---the version that was compiled normally. This is done using the [`setVersion`](http://hackage.haskell.org/packages/archive/hakyll/latest/doc/html/Hakyll-Core-Identifier.html#v:setVersion) function. The function can be changed to this:

~~~ {lang="haskell"}
urlField' :: String -> Context a
urlField' key = field key $
    fmap (maybe empty toUrl) . getRoute . setVersion Nothing . itemIdentifier
~~~

This successfully retrieves the correct URL of the post, just make sure you `mappend` this alternate function in your feed's `Context`.

## Conclusion

It's a shame that some duplicate work seems necessary when it comes to compiling the post. That is, I have to compile every post using my special Pandoc compiler, and then again using the more vanilla feed compiler I made. I tried to balance this by saving effort at the very least with the abbreviation substitution filter, so that it only runs once on every post.

[^1]: If you're wondering what `postsPattern` is, refer to my [Drafts in Hakyll](/posts/drafts-in-hakyll) post, in which this value is used to determine from where to pull posts in, in order to facilitate a draft preview system.
