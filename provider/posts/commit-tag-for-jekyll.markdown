---
title: Commit Tag for Jekyll
published: March 22, 2013
excerpt: Liquid tag that links to the latest commit for your Jekyll site
tags: Git, Jekyll, Ruby
icon: github-alt
---

I've been touching up my site and I got to the footer, where I used to display the last time the site was generated, such as "generated March 22nd, 2013." I decided this was pretty tacky and removed it, but I did want to have something in the footer. Eventually I got the idea to instead put a short SHA1 hash of the latest commit for the site. It would link to the github page for said commit, and on hover of the link it would show the commit message.

After having seen previous plugin implementations, it seemed pretty simple enough. I quickly brought up the page for [rugged](https://github.com/libgit2/rugged), [libgit2's](http://libgit2.github.com/) ruby bindings.

> libgit2 is a portable, pure C implementation of the Git core methods provided as a re-entrant linkable library with a solid API, allowing you to write native speed custom Git applications in any language which supports C bindings.

The end result is the commit tag plugin which quite simply lets you insert a link to the latest commit of your site with the following properties:

* the link text is the short-form, 8-character truncated SHA1 hash to the commit pointed to by HEAD on the default branch, `source` in my case
* the link title text is the commit message
* the link target is the github page for the commit

The plugin is available in my [site's repository](https://github.com/blaenk/blaenk.github.com.jekyll/blob/source/plugins/commit.rb).

The commit tag is simple to use, taking only one parameter which denotes the repository to link to, in my case:

~~~ {lang="text"}
{% raw %}{% commit blaenk/blaenk.github.com %}{% endraw %}
~~~

This format is similar to [vundle's](https://github.com/gmarik/vundle), a plugin management plugin for vim. Alternatively, if no parameter is given -- i.e. just `{% raw %}{% commit %}{% endraw %}` -- then instead of a link it produces a `span` tag with the short-form hash and commit message as title text.

Of course, this is a very simple plugin and makes many assumptions (i.e. that you're using github), but it's simple enough that you should be able to modify it to your purpose easily.

One thing to be mindful of is that the tag is evaluated at the point in which your site is generated. That is, if you generate your site, _then_ make a commit and deploy, the tag will be evaluated such that it displays the previous commit instead. This is simply the nature of static site generation. All that's required is to generate the site after having made the commits you make. To automate this, I made my `Rakefile`'s `deploy` task execute the `generate` task. This way is more natural in my opinion, as I'm used to deploying only after commiting my changes.
