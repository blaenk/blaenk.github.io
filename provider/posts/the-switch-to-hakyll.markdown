---
layout: post
title: The Switch to Hakyll
published: 2013-05-14
comments: true
excerpt: Migrating from Jekyll to Hakyll
tags: Hakyll, Haskell, Pandoc, Jekyll
---

* toc

This site was originally built with [Jekyll](http://jekyllrb.com/). Technically I began with the pre-packaged distribution known as [Octopress](http://octopress.org/) which offered a Rakefile for common tasks as well as an out-of-the-box directory structure. I didn't use many of these features, however, so I had been wanting to shed traces of Octopress -- partly motivated for pursuit of increased speed in site generation. I found the opportunity to do this when Jekyll 1.0 was released recently.

To cut away the unnecessary components of Octopress, I decided to go through every file and keep only what I absolutely needed. This is evident in commits after [`712168ec`](https://github.com/blaenk/blaenk.github.com.jekyll/commit/712168ec33004b693cc8cfb553a6a861da6a8708). I managed to cut down quite a bit, mainly in the `_plugins/` directory. I was well on my way to making my site's source a lot leaner when I remembered that I had been wanting to try [Hakyll](http://jaspervdj.be/hakyll/), a static site generator written in Haskell that I had heard about on Hacker News. Given that I was more or less starting from scratch with my site, I figured it was the perfect opportunity to try it.

Ultimately, this site is now compiled with Hakyll. It took me about a week to implement every feature I wanted in Hakyll and Pandoc. The net effect is that the difference in speed and flexibility is highly appreciable.

## Directory Structure

One of the more telling attributes of my new site's source is the directory structure I have adopted.

## Hakyll

### Style

### Abbreviation Substitution {#abbrev}

### SCSS Compilation

### Git Tag

## Pandoc: LLVM of Text {#pandoc}

### Pygments Syntax Highlighting {#pygments}

### Table of Contents Generation {#toc-generation}

## Deploying

## Conclusion
