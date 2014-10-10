---
title: Extra Dependencies in Hakyll
published: June 28, 2013
excerpt: Designating extra dependencies for compilers
tags: Hakyll, Haskell
toc: off
---

I use [scss](http://sass-lang.com/) for my site's stylesheets. scss is a language very similar to [CSS](https://en.wikipedia.org/wiki/Cascading_Style_Sheets) that adds support for variables, nesting, mixins, selector inheritance, and more---while retaining a syntax very similar to CSS itself.

## Split Stylesheets

A common practice I've noticed with the use of scss is to avoid having one monolithic stylesheet and instead opt to split it out into separate semantic files. For example, `post.scss`{.path} would concern styling for posts, `syntax.scss`{.path} would concern styling for Pygments syntax highlighting, etc. These files are then imported into one stylesheet, e.g., `screen.scss`{.path}, using the `@import` directive. It is this stylesheet that gets compiled by the scss compiler into the monolithic CSS.

## Problem

In Hakyll, rules are generally designated by a pattern that matches a resource coupled with a route and a compiler. So this was the rule I originally had for `scss/screen.scss`{.path}:

``` haskell
match "scss/screen.scss" $ do
  route $ constRoute "css/screen.css"
  compile $ sassCompiler
```

The rule simply states that Hakyll should:

1. find the file `scss/screen.scss`{.path}
2. route it to `css/screen.css`{.path}
3. compile it using my custom `sassCompiler`.

This worked fine, but it meant that when I built or previewed the site, if I modified one of the split stylesheets, such as `post.scss`{.path}, it wouldn't regenerate the monolithic stylesheet. It would only do so if `scss/screen.scss`{.path} itself was modified.

## Solution

With the help of Hakyll's creator, Jasper, I learned that the solution involves the use of [`makePatternDependency`](http://hackage.haskell.org/packages/archive/hakyll/latest/doc/html/Hakyll-Core-Metadata.html#v:makePatternDependency) to create a `Dependency` from a given `Pattern`, and [`rulesExtraDependencies`](http://hackage.haskell.org/packages/archive/hakyll/4.3.1.0/doc/html/Hakyll-Core-Rules.html#v:rulesExtraDependencies) to associate the dependencies with a specific `Compiler`.

> Advanced usage: add extra dependencies to compilers. Basically this is needed when you're doing unsafe tricky stuff in the rules monad, but you still want correct builds.
>
> A useful utility for this purpose is `makePatternDependency`.
>
> <cite><strong>Jasper</strong> on <a href="http://hackage.haskell.org/packages/archive/hakyll/4.3.1.0/doc/html/Hakyll-Core-Rules.html#v:rulesExtraDependencies">Hackage</a></cite>

Now when I'm previewing my site---or build the site in general---and I modify any scss file, it correctly regenerates the monolithic `css/screen.css`{.path} file. Here's my new scss compiler rule:

``` haskell
match "scss/**.scss" $ do
  compile getResourceBody

scssDependencies <- makePatternDependency "scss/**.scss"
rulesExtraDependencies [scssDependencies] $ do
  create ["css/screen.css"] $ do
    route $ idRoute
    compile $ sassCompiler
```
