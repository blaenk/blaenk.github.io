---
title: Drafts in Hakyll
published: June 5, 2013
excerpt: Implementing a simple drafts system
tags: Hakyll, Haskell
toc: off
---

In the post about my [switch to Hakyll](/posts/the-switch-to-hakyll) I talked about the various features I implemented in my Hakyll blog. One feature that was sorely missing was support for drafts: posts which aren't supposed to be published when the site is deployed.

I usually take my time writing posts. Sometimes it can take me days, during which I might want to deploy other minor changes to the site, or perhaps even a shorter, quicker post. Without a draft system, I'm forced to manually move the draft post out of the provider directory so that it doesn't get generated and subsequently deployed.

A draft system is able to clearly distinguish draft posts from regular posts. This way, when it comes to deploying the site, draft posts aren't deployed along with it.

## Considerations

I believe that the fundamental problem with draft systems in static site generators is that drafts, like regular posts, come to permeate the entire site. They accomplish this by showing up on index pages, tag pages, and any other place you might expect regular posts to show up in. This is something to keep in mind when creating a draft system because it means that simply deleting the compiled page won't suffice, as there will still be traces in other pages.

One approach to this problem is to quarantine the draft posts such that they don't show up on any of these things and instead only show them when you visit them directly. This is not an option for me because when I preview drafts I want to see how they will affect the entire site. I don't preview drafts simply to check how my post is formatted.

## Examples

The following two draft system implementations exemplify the two approaches I can think of for a draft system. These are approaches taken by static site generators. There are other ad hoc solutions, such as creating a separate `drafts` branch in git.

[Octopress](http://octopress.org/) had support for drafts hacked onto Jekyll by way of a plugin that allowed a metadata field `published` to be set that, if set to **false**, would establish an environment variable that would be detected on site generation in order to regenerate the site without the draft posts. This consequently meant that draft posts were stored in the same directory as regular posts.

[Jekyll](http://jekyllrb.com) implemented support for this in its 1.0 version by allowing a new directory, `_drafts/`{.path}, to store draft posts which could be previewed by specifying the `--drafts` flag to most operations. However, it was right after Jekyll 1.0 was released that I decided to switch to Hakyll.

Octopress' draft system was pretty straightforward in my opinion, despite being a pretty hack-ish implementation. I would create drafts in the same directory as all of the other posts, and would simply set metadata `published: false`. This would allow the draft to show up when I previewed the site, but not when it was ultimately deployed. This was accomplished by regenerating the site on deploy, this time without the preview posts.

The other solution I could think of consisted of detecting when the site was being previewed, and if that were the case, establish a different output directory and a different posts pattern which would include the posts in a separate `drafts/`{.path} directory. When the site _wasn't_ being previewed, the regular output directory would be used.

## Implementation

Both approaches amount to hacks on top of Hakyll, but after some consideration, it seems to me that the second option is a lot less messy.

My solution consists of some code that runs before the Hakyll driver. The code extracts the first argument from the program arguments, which by convention is the action to perform, e.g. build, clean, preview, and checks to see if it's the **preview** action.

``` haskell
main = do
  (action:_) <- getArgs
```

If the **preview** action is being run, the Hakyll configuration data structure's `destinationDirectory` field, i.e. the output directory, is changed to a separate one for previewing purposes. This implies that the field is set to the deployable output directory by default. This is important because it means that all actions other than **preview** will _ignore_ drafts.

Furthermore, if we are previewing, the pattern used to fetch posts is changed to also include the posts in the `drafts/`{.path} directory. This is achieved by using the [`.||.`](http://hackage.haskell.org/packages/archive/hakyll/4.2.2.0/doc/html/Hakyll-Core-Identifier-Pattern.html#v:.-38--38-.) function to compose two `Pattern` types.

~~~ {lang="haskell"}
  let previewMode  = action == "preview"
      hakyllConf   = if previewMode
                     then myHakyllConf { destinationDirectory = "generated/preview" }
                     else myHakyllConf
      postsPattern = if previewMode
                     then "posts/*" .||. "drafts/*"
                     else "posts/*"
~~~

Finally, we need to make one modification to an existing action. The **clean** action removes the provider, cache, and destination (output) directories. However, we now have two separate destination directories and by default every other action only knows of the deployable destination directory, i.e. the one without drafts. For this reason, we have to detect if the action being run is **clean**, and if so, remove the preview output directory.

Note that this depends on [`System.Directory`](http://hackage.haskell.org/packages/archive/directory/latest/doc/html/System-Directory.html).

~~~ {lang="haskell"}
  when (action == "clean") $ do
    putStrLn "Removing generated/preview..."
    removeDirectoryRecursive "generated/preview"
~~~

Now that this is done, you simply have to be sure to use the `hakyllConf` that was created above, as well as `postsPattern` wherever you would have simply put `"posts/*"` before. Two places that come to mind are tag generation and posts compilation:

~~~ {lang="haskell"}
  hakyllWith hakyllConf $ do
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    match postsPattern $ do
      -- etc.
~~~

## Usage

This drafts system is pretty straightforward. When you run `./site preview` it'll serve the site with drafts as well. Deployment carries on as usual, i.e. you shouldn't have to modify your deployment routine. With this system, you'll never accidentally deploy drafts because they won't ever show up in that output directory to begin with.
