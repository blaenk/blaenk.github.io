---
title: Work
published: 2012-12-29
comments: off
toc: off
---

This is an aggregation of the different work I've done in terms of open source contributions and projects.

## Contributions

##### Hoedown: Extension feature combination fixes {#libhoedown .collapse}

<div class="collapsible">

Over the course of writing [bindings](#hoedown) to Hoedown for Rust, I discovered a couple of bugs in the C library which only manifested given certain combinations of different extension features.

[GDB]: /notes/gdb

After debugging with [GDB] I [noticed first] that footnotes and footnote references would not be processed if images and links were not being processed. Similarly, I noticed that [underlines would not be processed] if emphasis, double_emphasis, or triple_emphasis were not being processed.

[noticed first]: https://github.com/hoedown/hoedown/pull/150
[underlines would not be processed]: https://github.com/hoedown/hoedown/pull/149

</div>

##### Rust: Add missing trait implementation {#rust-hash .collapse}

<div class="collapsible">

I submitted a _very_ [minor fix] to Rust which added a `Hash` implementation to the `Path` type, the absence of which incurred both a performance and ergonomic cost when working with data structures that contained `PathBuf` values. The implementation was accidentally missing as a result of the fallout resulting from a complete rewrite of the `std::path` module.

[minor fix]: https://github.com/rust-lang/rust/pull/22351

</div>

##### Glob: Recursive wildcards and better error reporting {#glob .collapse}

<div class="collapsible">

The official [glob] package, used in Cargo as well as many other libraries, is used for globbing operations: enumerating a set of files based on a given pattern.

[glob]: https://github.com/rust-lang/glob

I implemented support for recursive wildcards, e.g. `posts/**/*.md` would match any file ending in an `.md` extension in any directory under `posts/`. I also implemented syntax error handling and reporting for patterns to help the user determine exactly how a pattern may be malformed.

Going further, I enabled I/O error propagation during the globbing operation, for exampl in case a directory cannot be read. Finally I made a modification to allow returning relative file paths if a relative pattern was used.

See the [pull requests](https://github.com/rust-lang/glob/pulls?q=is%3Apr+is%3Aclosed+author%3Ablaenk).

</div>

##### Aura: Allow Number Parameter for Truncation {#aura-truncate-number .collapse}

<div class="collapsible">

[Aura] is an [arch user repository] front-end written in Haskell. It allows for the seamless, automated installation of packages from the AUR. Aura has a flag `-As` which allows for searching the AUR, which can also accept the flags `--head` and `--tail` to show only the first or last 10 results.

The reason these flags exist instead of just using the `head` or `tail` programs is that the results that Aura outputs consist of two lines each: one for the package name and the other for the package description. For this reason, the Aura developers included these as a convenience. The problem was that these flags didn't accept a parameter, instead always defaulting to 10 items.

Usually the package I'm looking for is either the first result or within the first five, so the default of 10 results was too high.

I [contributed] a feature that allows these flags to accept an optional parameter specifying the number of items to return, for example, `--head=3` would show the first three results. The nice thing is that the parameter is optional, so that `--head` continues to default to 10 results, making the change backwards-compatible.

This feature made it into [version 1.2.3.3].

[Aura]: https://github.com/fosskers/aura
[arch user repository]: https://wiki.archlinux.org/index.php/Arch_User_Repository
[contributed]: https://github.com/fosskers/aura/pull/233
[version 1.2.3.3]: https://github.com/fosskers/aura/commit/e6069dd3571b1ee338da2b658847f2e08bb15b22#diff-4

</div>

##### Hakyll: Sub-Second Granularity {#hakyll-subsecond .collapse}

<div class="collapsible">

[Hakyll] has a watch mode where it watches files and re-compiles them when they're modified. It does this by checking the file's modified time and storing it for comparison on the next iteration. When the new major-release version 7.8 of [GHC] was released, a [bug was observed] in Hakyll where whenever any file was modified, the whole site was recompiled instead of just the file that was modified.

[Hakyll]: http://jaspervdj.be/hakyll/
[GHC]: http://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[bug was observed]: https://github.com/jaspervdj/hakyll/issues/250

The perplexing thing was that the Hakyll source hadn't changed at all, so clearly the bug was being caused by a direct or indirect package dependency or, _gulp_, the compiler itself. All in all, it seemed pretty uncharacteristic of my impression of Haskell.

Of course, it wasn't a compiler bug. Instead, it seemed to have been caused by the culmination of different factors. First, the [directory] package, which houses the `getModificationTime` function used to retrieve a file's modification time, began supporting sub-second precision in version 1.2 and above _if_ it's linked against the [unix] package version 2.6 and above.

> Note: When linked against unix-2.6.0.0 or later the reported time supports sub-second precision if provided by the underlying system call.
>
> <cite>[documentation][getModificationTime] for `getModificationTime`</cite>

[directory]: http://hackage.haskell.org/package/directory
[unix]: http://hackage.haskell.org/package/unix
[getModificationTime]: http://hackage.haskell.org/package/directory-1.2.1.0/docs/System-Directory.html#v:getModificationTime

The problem was essentially that Hakyll cached the modification time by first shaving off the sub-second precision with the `floor` function. However, when it then compared against this cached modification time, the other comparison operand's sub-second precision _wasn't_ shaved. What this meant was that the file was _almost always_ deemed modified, unless of course the modification time had a sub-second count of zero to begin with.

To illustrate the problem:

1. Read modification time as 3:45.325, shave sub-seconds and save as 3:45.000.
2. Read modification time as 3:45.325, compare against cached modification time, 3:45.000, to see if the file has changed.
3. 3:45.325 is more recent than 3:45.000, so the file is considered to have been modified.

The [patch][sub-second patch] simply _kept_ the sub-second precision when caching, allowing for graceful handling of systems that do and don't support sub-second precision.

This fix made it into [Hakyll 4.5.2.0].

[sub-second patch]: https://github.com/jaspervdj/hakyll/pull/252
[Hakyll 4.5.2.0]: https://github.com/jaspervdj/hakyll/commit/d89fadcdb97c2acd9aeaa58c830d30ad755f31d7

</div>

##### haxr: i8-Type Support {#haxr-i8-types .collapse}

<div class="collapsible">

[HaXR] is the main (only?) available package for performing XML-RPC in Haskell. It supports type conversions via typeclasses as with other serialization packages such as [Aeson], which can also be automated via [Template Haskell]. I [contributed][haxr pr] `i8`-type support to the package.

[HaXR]: http://hackage.haskell.org/package/haxr
[Aeson]: http://hackage.haskell.org/package/aeson
[Template Haskell]: http://www.haskell.org/haskellwiki/Template_Haskell
[haxr pr]: https://github.com/byorgey/haxr/pull/1

</div>

##### Go scgiclient: Unix Domain Socket Support {#go-scgiclient-uds .collapse}

<div class="collapsible">

I [contributed][goscgi pr] support for [unix domain sockets] to a Go package for creating [SCGI] clients.

[goscgi pr]: https://github.com/mpl/scgiclient/pull/1
[unix domain sockets]: http://en.wikipedia.org/wiki/Unix_domain_socket
[SCGI]: http://en.wikipedia.org/wiki/Simple_Common_Gateway_Interface

</div>

##### Go xmlrpc: i8-Type and Base64 Support {#go-xmlrpc-i8-base64 .collapse}

<div class="collapsible">

I contributed [i8-type] and [base64] support to an [XML-RPC package] for Go.

[i8-type]: https://github.com/kolo/xmlrpc/pull/12
[base64]: https://github.com/kolo/xmlrpc/pull/14
[XML-RPC package]: https://github.com/kolo/xmlrpc

</div>

##### archlinux: Syncplay Packages {#syncplay-packages .collapse}

<div class="collapsible">

One of my most used programs is a little program called [syncplay] which works with different media players---I use [MPC-HC] on Windows and [mpv] on Linux---and makes it easy to watch movies and shows with others, automatically synchronized, so that regardless of whether someone pauses, seeks, etc., everyone else's position will be synchronized. This allows me to watch movies and shows regularly with distant friends and family.

There exist user friendly installers for Windows, but Linux' side of things consist of a typical Makefile, so I created archlinux packages for the release and [git] versions of syncplay. This required some [modifications] to syncplay which were merged upstream. Furthermore, archlinux uses Python 3 by default, which syncplay can't use because of its dependency on twisted, which as far as I know is still not Python 3 compatible. So the package required some automated replacing of the shebang from `python` to `python2`.

[git]: https://aur.archlinux.org/packages/syncplay-git/
[MPC-HC]: http://mpc-hc.org
[mpv]: http://mpv.io
[syncplay]: http://syncplay.pl
[git package]: https://aur.archlinux.org/packages/syncplay-git/
[modifications]: https://github.com/Uriziel/syncplay/pull/30

</div>

##### vim-pandoc-syntax: Embedded Codeblock Highlighting, Various Features & Fixes {#vim-pandoc-syntax .collapse}

<div class="collapsible">

I contributed a [variety] of features to the [vim-pandoc-syntax] plugin, a plugin that provides [Pandoc-flavored markdown] syntax highlighting and concealment to vim. Concealments in vim are a relatively new feature which allow a different in-editor appearance for certain patterns. For example, the underscores that are used to italicize text can be concealed to decrease clutter.

I started out by fixing the italic pattern, which would previously get tripped up by intra-word underscores such as in ALL_BUILD. Then I added concealment of codeblock delimiters, so that the starting delimiter for codeblocks gets replaced with a &lambda; and the end delimiter gets concealed altogether. I also added abbreviation highlighting and concealment, then I added strong-emphasis highlighting and concealment. I fixed a bug in the definition block pattern that was causing cascading issues with other patterns.

The most substantial contribution was [embedded-language highlighting] for codeblocks. That is, if one writes a Haskell codeblock, the codeblock's contents would be highlighted using the Haskell syntax highlighter:

<img src="http://i.imgur.com/WpK6jNZ.png" class="center">

</div>

[vim-pandoc-syntax]: https://github.com/vim-pandoc/vim-pandoc-syntax
[variety]: https://github.com/vim-pandoc/vim-pandoc-syntax/pulls/blaenk?direction=desc&page=1&sort=created&state=closed
[Pandoc-flavored markdown]: http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown
[embedded-language highlighting]: https://github.com/vim-pandoc/vim-pandoc-syntax/issues/14

##### Hakyll: Update to Work With Pandoc 1.12 {#hakyll-pandoc-update .collapse}

<div class="collapsible">

A [patch](https://github.com/jaspervdj/hakyll/pull/183) that updates Hakyll to relfect the fact that Pandoc 1.12 decoupled the citations features. The citation features were provided by the [citeproc-hs](http://hackage.haskell.org/package/citeproc-hs) whose developer had been missing for some time now. The citeproc-hs package was embedded into the [pandoc-citeproc](http://hackage.haskell.org/package/pandoc-citeproc) package which contains the Pandoc-to-citeproc-hs interface. I simply modified Hakyll to use the new pandoc-citeproc package instead of citeproc-hs, as well as conform to the new citations API.

This change made it into [Hakyll 4.4.0.0](http://jaspervdj.be/hakyll/releases.html#hakyll-4.4.0.0).

</div>

##### Hakyll: Add Default Port Option {#hakyll-port .collapse}

<div class="collapsible">

Another [patch](https://github.com/jaspervdj/hakyll/pull/178) I created for [hakyll](http://jaspervdj.be/hakyll/), which was readily merged, adds a new field to Hakyll's `Configuration` [structure](http://hackage.haskell.org/packages/archive/hakyll/latest/doc/html/Hakyll-Core-Configuration.html) that allows one to specify the default port to use when running the preview server in Hakyll.

Before this patch, the default port was set to 8000---a port on which I already had a service listening on my system, and clients expected it there. It of course was possible to define a separate port as a command line argument, but this was necessary on every invocation of the preview server: `./site preview -p 4000`

With this patch users of Hakyll could override the port field in the `Configuration` structure so that an invocation of `./site preview` automatically listens on that defined port. To avoid breaking existing configurations, the default configuration still sets the default port to 8000, the only difference now is that it can be changed.

This change made it into [Hakyll 4.4.0.0](http://jaspervdj.be/hakyll/releases.html#hakyll-4.4.0.0).

</div>

##### libtorrent: Fix I/O Multiplexing Error on Solaris {#libtorrent .collapse}

<div class="collapsible">

[libtorrent](https://github.com/rakshasa/libtorrent) is the Bit Torrent library used in rtorrent, developed by the same person. After fixing the [signal disposition establishment bug](#rtorrent) in hopes of fixing [rtorrent issue #51](https://github.com/rakshasa/rtorrent/issues/51) which rendered rtorrent unusable on Solaris derivatives, three months later someone confirmed that it had fixed one of their problems. However, rtorrent was now crashing with the message "Listener port received error event." I tracked this message down to libtorrent. The problem was that on Solaris, libtorrent employs `select()` for I/O multiplexing instead of the platform-specific API such as `/dev/poll` and event ports, and the signature of `select()` was seemingly misinterpreted---which is a common occurrence given documentation discrepancies.

For example, in the Linux man pages, `select()` is [prototyped as](http://man7.org/linux/man-pages/man2/select.2.html):

~~~ {lang="cpp"}
int select(int nfds, fd_set *readfds, fd_set *writefds,
           fd_set *exceptfds, struct timeval *timeout);
~~~

However, in the Solaris man pages it's [prototyped as](docs.oracle.com/cd/E26502_01/html/E29034/select-3c.html):

~~~ {lang="cpp"}
int select(int nfds, fd_set *readfds, fd_set *writefds,
           fd_set *errorfds, struct timeval *timeout);
~~~

The key difference is that on Linux, the fourth argument is named `exceptfds` whereas on Solaris it's named `errorfds`. This innocent-looking difference mistakenly gives the impression that file descriptors present in that set indicate that an I/O error has occurred on that file descriptor. However, this is not necessarily the case, as is outlined in [`select_tut(2)`](http://man7.org/linux/man-pages/man2/select_tut.2.html):

> This set is watched for "exceptional conditions". In practice, only one such exceptional condition is common: the availability of out-of-band (OOB) data for reading from a TCP socket. See `recv(2)`, `send(2)`, and `tcp(7)` for more details about OOB data. (One other less common case where `select(2)` indicates an exceptional condition occurs with pseudoterminals in packet mode; see `tty_ioctl(4)`.) After `select()` has returned, `exceptfds` will be cleared of all file descriptors except for those for which an exceptional condition has occurred.

Furthermore, the Solaris man page says:

> If a socket has a pending error, it is considered to have an exceptional condition pending. Otherwise, what constitutes an exceptional condition is file type-specific. For a file descriptor for use with a socket, it is protocol-specific except as noted below. For other file types, if the operation is meaningless for a particular file type, `select()` or `pselect()` indicates that the descriptor is ready for read or write operations and indicates that the descriptor has no exceptional condition pending.
>
> ...
>
> A socket is considered to have an exceptional condition pending if a receive operation with `O_NONBLOCK` clear for the open file description and with the `MSG_OOB` flag set would return out-of-band data without blocking. (It is protocol-specific whether the `MSG_OOB` flag would be used to read out-of-band data.) A socket will also be considered to have an exceptional condition pending if an out-of-band data mark is present in the receive queue.

rtorrent didn't use out-of-band data or pseudoterminals as far as I was aware, and after searching the Solaris man pages for a while I couldn't find more information on what else it could've been. Considering that this was only observable on Solaris derivatives, I decided that it must have been something platform-specific, perhaps Solaris was more relaxed on its criteria for what it considered to be an "exceptional condition."

The [fix I came up with](https://github.com/rakshasa/libtorrent/pull/40) involved invoking [`getsockopt()`](http://man7.org/linux/man-pages/man2/getsockopt.2.html) to retrieve the socket error associated with that file descriptor, and if there was indeed an error, follow through with throwing the exception, albeit with more descriptive information as to what the error was. If, on the other hand, there was no error, then simply do nothing.

</div>

##### Hakyll: Fix Preview Functionality on Windows {#hakyll-preview .collapse}

<div class="collapsible">

[Hakyll](http://jaspervdj.be/hakyll/) is a static site generator, like [Jekyll](http://jekyllrb.com/), written in Haskell. At one point I decided to clean up my site source's directory structure by creating a separate provider directory. This triggered a bug in the latest stable release at the time (4.2.2.0) which caused the preview component to enter an infinite loop. The preview component simply watches files and recompiles them when you edit them for quick previewing on a locally-hosted web server. I found that this problem was indirectly solved in the unreleased master branch as a result of a significant change to the preview component that used specific operating systems' file notification APIs. That is, instead of the previous manual polling of the files, it would use [inotify](http://en.wikipedia.org/wiki/Inotify) on Linux for example.

All worked perfectly well on Linux, however, when I tried it on Windows I experienced very odd behavior in which the program seemed to freeze right after generating/compiling the site. Sometimes it would manage to output a single "L". I remembered that previously, it displayed a message such as "Listening on http://0.0.0.0:8000," so I concluded that somehow it was being interrupted. I found more evidence to back this hypothesis when I noticed that saving a file---thereby generating a file system event and triggering the callback established in the preview component of Hakyll (which simply recompiled the file that had been modified)---would cause the program to print a bit more of the message. "L" became "Lis" became "Listeni" and so on. Furthermore, attempting to load a page would time out unless a file was excessively saved to trigger the file system event callback---presumably affording the server's green thread enough time slices for it to respond to the request before the time out.

Upon analyzing the Hakyll source and noticing that it indirectly used the [foreign function interface](http://www.haskell.org/haskellwiki/FFI_Introduction) for interfacing with the host OS' file system events API, I found this relevant bit of information in the [GHC documentation](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html#g:5):

> Different Haskell implementations have different characteristics with regard to which operations block all threads.
> 
> Using GHC without the `-threaded` option, all foreign calls will block all other Haskell threads in the system, although I/O operations will not. With the `-threaded` option, only foreign calls with the unsafe attribute will block all other threads.

Compiling with the `-threaded` flag solved that problem. However, now the problem was that saving a file would yield a "permission denied" error in the Hakyll program. I eventually [came to realize](https://github.com/mdittmer/win32-notify/issues/3#issuecomment-18260415) that this was inherent behavior in the file system events API abstracted by the file system events Haskell package. The problem consisted of there being the possibility that the notification for a file having been modified, for example, would be sent and received/processed before the program (that caused that event to fire) had a chance to finish the actual writing that triggered the event to begin with. The workaround I [came up with](https://github.com/jaspervdj/hakyll/pull/155) consisted of simply attempting to open the file---success of which would indicate that the other process had already finished writing to the file---and if this was not possible, sleep for a bit before trying again.

The only other alternative we could think of was switching to a polling system for Windows. This was unfeasible because the file system events package didn't expose a way to force this, which would require us to implement it ourselves and would add significant overhead in performance, since every file would be polled periodically for changes, as opposed to this workaround which would poll a single file only if it wasn't able to open it on the first try.

This change made it into [Hakyll 4.3.0.0](http://jaspervdj.be/hakyll/releases.html#hakyll-4.3.0.0).

</div>

##### rtorrent: Fix Unportable Signal Disposition Establishment on Solaris {#rtorrent .collapse}

<div class="collapsible">

The [rtorrent](http://libtorrent.rakshasa.no/) project makes use of signals for certain kinds of inter-thread communication. Certain users on [Solaris](http://en.wikipedia.org/wiki/Solaris_(operating_system)) reported that [rtorrent crashed](https://github.com/rakshasa/rtorrent/issues/51) as soon as signal `SIGUSR1` was delivered. Being an avid reader of the POSIX standards I was curious and felt that I might know what was wrong, figuring that correctly identifying and solving the problem would be a huge testament to the POSIX standards considering I had never used Solaris.

Some people in the issue figured that it must be a non-POSIX compliant implementation of `pthread_kill()` which was preventing the application from sending signals to specific threads. I didn't think this was the case, as Solaris' [manual page](http://docs.oracle.com/cd/E26502_01/html/E29034/pthread-kill-3c.html) for `pthread_kill()` claims that it's implemented as is intended. If it was indeed a non-compliant implementation, I figured someone else would've already encountered the issue and there would be some sort of note in the man page. In fact, Solaris is [fully POSIX-compliant](http://en.wikipedia.org/wiki/POSIX#Fully_POSIX-compliant) which is more than can be said of Linux, and yet Linux didn't exhibit this behavior.

Instead my suspicion was something else entirely. As soon as I recognized that clearly something signal-related was causing crashes on certain platforms in particular, I thought of the one glaring, well-known to be unportable system call: `signal()`. In fact, the Linux [man page](http://man7.org/linux/man-pages/man2/signal.2.html) for `signal()` says:

> The only portable use of `signal()` is to set a signal's disposition to `SIG_DFL` or `SIG_IGN`. The semantics when using `signal()` to establish a signal handler vary across systems (and POSIX.1 explicitly permits this variation); **do not use it for this purpose.**

The emphasis is theirs and goes to show how unpredictable the use of `signal()` could be. To complicate matters, the first line in the man page's notes section says:

> The effects of `signal()` in a multithreaded process are unspecified.

However, I believed that the problem lay in the possibility that Solaris provided different semantics for `signal()` from the semantics that Linux provided:

> In the original UNIX systems, when a handler that was established using `signal()` was invoked by the delivery of a signal, the disposition of the signal would be reset to `SIG_DFL`, and the system did not block delivery of further instances of the signal. [...] This was bad because the signal might be delivered again before the handler had a chance to reestablish itself.  Furthermore, rapid deliveries of the same signal could result in recursive invocations of the handler.

This behavior is known as [System V](http://en.wikipedia.org/wiki/UNIX_System_V) semantics. In other words, when a signal handler is established and then subsequently triggered, the signal disposition is reset to its default disposition, whatever that may be for the signal in question. If the handler isn't re-established, then a subsequent triggering of that signal will be handled based on the default disposition for that signal.

There is another behavior which is referred to as [BSD](http://en.wikipedia.org/wiki/Berkeley_Software_Distribution) semantics in which:

> the signal disposition is not reset, and further instances of the signal are blocked from being delivered while the handler is executing.  Furthermore, certain blocking system calls are automatically restarted if interrupted by a signal handler.

The situation on Linux is such that the kernel's `signal()` system call provides System V semantics. However, glibc 2 and later expose a wrapper function for `signal()` which instead delegates its work to the preferred---for portability and flexiblity reasons---system call [`sigaction()`](http://man7.org/linux/man-pages/man2/sigaction.2.html), called in such a way as to provide BSD semantics. This wrapper function is exposed if the `_BSD_SOURCE` [feature test macro](http://man7.org/linux/man-pages/man7/feature_test_macros.7.html) is defined, which it is by default.

Solaris doesn't have such a wrapper for `signal()`, instead exposing its bare, System V semantics system call with [`signal()`](http://docs.oracle.com/cd/E26502_01/html/E29034/signal-3c.html):

> ``` c
> void (*signal(int sig, void (*disp)(int)))(int);
> ```
> 
> If `signal()` is used, `disp` is the address of a signal handler, and `sig` is not `SIGILL`, `SIGTRAP`, or `SIGPWR`, the system first sets the signal's disposition to `SIG_DFL` before executing the signal handler.

This clearly states that the signal disposition is reset to its default disposition before executing the signal handler. Taking a look at the [default signal disposition table](http://docs.oracle.com/cd/E26502_01/html/E29033/signal.h-3head.html) for Solaris, we can see that `SIGUSR1`'s default disposition is to exit the application. Presumably, Solaris users were crashing upon the second delivery of `SIGUSR1` or any other signal established with `signal()` who's default disposition was to exit or abort (core dump).

My [patch](https://github.com/rakshasa/rtorrent/pull/127) simply consisted of switching out calls to `signal()` for purposes of establishing signal handlers with calls to `sigaction()`.

</div>

##### MPC-HC: Fix Web UI Seeking {#mpc-hc .collapse}

<div class="collapsible">

I was interested in modifying [MPC-HC](http://mpc-hc.org/) to allow people to watch things in sync with each other, i.e. pause when someone pauses, seek when someone seeks, etc. I pulled the source from github and began looking for a good way to implement this functionality. I found the source for the web UI component of MPC-HC, which essentially provides an interface for which a web UI can be developed to control MPC-HC. I figured I could make use of this and began testing it when a friend noticed that the seeking in the existing web UI didn't work. After finding the relevant code in the MPC-HC source I found that it was a simple problem of failing to URL decode the seek parameter sent from the web UI. I submitted a [patch](https://github.com/mpc-hc/mpc-hc/pull/38) which was ultimately merged in and pushed out in [version 1.6.6](https://trac.mpc-hc.org/wiki/Changelog/1.6.6).

As for the original intent of implementing the functionality for synced playback, the MPC-HC developers told me about [Syncplay](http://syncplay.pl/) which I have used for months now to much success. The added benefit is that it isn't specific to any particular media player and is cross-platform.

</div>

##### node-xmlrpc: Buffer & i8 Datatypes, Chunked Responses, HTTP Basic Authentication {#node-xmlrpc .collapse}

<div class="collapsible">

I contributed a [series of features](https://github.com/baalexander/node-xmlrpc/pulls/blaenk?direction=desc&page=1&sort=created&state=closed) to [node-xmlrpc](https://github.com/baalexander/node-xmlrpc) because I needed them in one of my projects at the time. node-xmlrpc is a package for [node.js](http://nodejs.org/) which provides an interface for [XML-RPC](http://en.wikipedia.org/wiki/XML-RPC) communication.

I added `i8` datatype support (8-byte integers, i.e. 64-bit integers) because the application I was interfacing with always used that datatype when responding with integers, regardless of whether or not it was necessary. I added `buffer` datatype support (Base64 encoded data) just a volunteer contribution, though I had no need for it.

The original stream XML-RPC parser was unable to handle chunked responses correctly for element inner-text. This meant that if a chunk ended inside an element, that inner-text would become truncated. I fixed it by continuing to collect the inner-text until an end-of-element event was fired by the XML parser.

Finally, I added support for basic HTTP authentication.

</div>

## Projects

##### Hoedown: Idiomatic Rust bindings for Hoedown {#hoedown .collapse}

<div class="collapsible">

I needed Markdown processing functionality for a static site generator infrastructure library I'm developing, so I decided to write idiomatic [bindings] for [Hoedown], a C library for processing Markdown.

[bindings]: https://github.com/blaenk/hoedown
[Hoedown]: https://github.com/hoedown/hoedown

The bindings aim to be as idiomatic as possible, going so far as to bridge the gap so that Rust closures can be used where C callbacks are expected.

</div>

##### Levee: Web Interface for rtorrent {#levee .collapse}

<div class="collapsible">

[Levee] is a web interface for [rtorrent]. The back-end is written in Clojure using a combination of [http-kit] and [compojure], while the front-end is written in ClojureScript using [Om], which itself is built on top of Facebook's [React].

[Levee]: https://github.com/blaenk/levee
[rtorrent]: http://rakshasa.github.io/rtorrent/
[http-kit]: http://http-kit.org
[compojure]: https://github.com/weavejester/compojure
[Om]: https://github.com/swannodette/om
[React]: http://facebook.github.io/react/

It consists of a clean, responsive UI with support for drag-and-drop file uploads, WebSockets for up-to-date information, and a simple locking system to facilitate a multi-user environment.

Torrent metadata pertaining to Levee is stored in the torrent itself, thereby avoiding the need to maintain consistency between rtorrent and a separate database.

</div>

##### Hakyll website: Source Code for my Hakyll-Powered website {#hakyll-website .collapse}

<div class="collapsible">

I wouldn't include my website as a project if it weren't for the fact that I have heavily modified it. I've [written] about many of these modifications and customizations. Some of the features I've implemented as Pandoc abstract syntax tree transformers and others as Hakyll custom compilers.

Pandoc AST transformers I've written include blockquote beautification, automatic abbreviation substitution, [Pygments] codeblock highlighting, and table of contents generation (with pure CSS nested section numbering).

Custom Hakyll compilers I've written include a [preview-draft system] to separate drafts from other published posts and another to [automatically push changes] as they're written to the client through WebScokets, which is useful when previewing a post that's in the middle of being written.

A full list of customizations is available in the readme for the [repository].

[written]: http://localhost:4000/tags/hakyll/
[Pygments]: http://pygments.org/
[preview-draft system]: /posts/drafts-in-hakyll/
[automatically push changes]: /posts/live-editing-with-hakyll/
[repository]: https://github.com/blaenk/blaenk.github.io

</div>

##### Learning From Data: Assignments for Cal Tech CS 1156x {#learning-from-data .collapse}

<div class="collapsible">

The code for the [homework assignments] for the Cal Tech CS 1156x class offered on edX.org. The class was about Machine Learning, focusing on the theory. My class notes are [also available]. I decided to do the assignments in Python to gain some experience with [NumPy], which is quickly becoming the industry choice for scientific computing. I had experience with R, but I preferred to use a "real" language.

[homework assignments]: http://work.caltech.edu/homeworks.html
[also available]: /notes/machine-learning-theory/
[NumPy]: http://numpy.scipy.org/

I created a simple testing mechanism to automate testing of the experiments that were developed, allowing me to do something like:

``` python
# constructor is Question(question_str, choices, answer)
question8 = Question("8. in sample error",
                     [0, 0.1, 0.3, 0.5, 0.8], 'd')

in_sample_error = experiment()
question8.check(in_sample_error)
```

Which would output something like:

```
8. in sample error
  result:  0.506176
  nearest: d. 0.5
  answer:  d. 0.5
  + CORRECT
```

The homework assignments covered many topics such as the simple perceptron learning algorithm, support vector machines, logistic regression via stochastic gradient descent, regularization, and validation.

</div>

##### Pulse Visualizer: Visualizer for PulseAudio in Haskell {#pulse-visualizer .collapse}

<div class="collapsible">

This was my first Haskell application, aside from exercise solutions to Haskell books. During my final semester of college in 2012, I wanted to do some Independent Study to round out full-time student status. A [professor](http://kevinwortman.com/) agreed to mentor me in two different independent studies: [digital signal processing](http://en.wikipedia.org/wiki/Digital_signal_processing) and [Haskell](http://en.wikipedia.org/wiki/Haskell_(programming_language)). At first I had intended on treating them separately with the goal of writing a music visualizer for iTunes for the DSP study and perhaps a web application for the Haskell study. My professor suggested I try and merge them to make it easier on myself and that is exactly what I did.

I had already gotten a barebones iTunes visualizer up and running with C, so I figured I would write some hooks with the [foreign function interface](http://en.wikipedia.org/wiki/Foreign_function_interface) to delegate most of the work to Haskell. The way of going about this was pretty messy however, as it involved (at the time, and most likely even now) compiling the Haskell code into dynamic link libraries because the Haskell code had to be compiled with gcc, who's symbols differed from the ones Visual Studio produced, which I wanted to use to take advantage of DirectX 11 and DirectCompute.

I managed to get something working, but it felt very messy and was quite the abomination: Haskell to DLL with GCC on Windows linked with an iTunes Visualization Plugin DLL produced by MSVC which used DirectX 11. So I decided to instead look around for options to pursue on Linux, where Haskell development felt a lot more natural to me. After looking around for xmms, Banshee, or other bindings, and finding them lacking, I figured I might as well create a visualizer for a more fundamental thing: [PulseAudio](http://en.wikipedia.org/wiki/PulseAudio) itself.

PulseAudio has a concept of sources (e.g. processes) and sinks (e.g. sound cards). Every sink also has a corresponding source known as a monitor, meaning that the audio going to the associated sink can be intercepted and read. I found a [binding for Haskell](http://hackage.haskell.org/package/pulse-simple) that seemed sufficient enough which allowed me to monitor all of the audio on the system. I then paired this up with OpenGL to draw a pretty basic "frequency bar" visualization. The major benefit of having written it for PulseAudio itself instead of a particular music player or even as a standalone application is that I could then play the audio anywhere, such as YouTube or Pandora, and watch it visualized in my application.

Source is available [on github](https://github.com/blaenk/pulse-visualizer).

</div>

##### WP-reCAPTCHA: Official reCAPTCHA Plug-In for WordPress {#wp-recaptcha .collapse}

<div class="collapsible">

Back in 2007 I found out about [reCAPTCHA](http://www.google.com/recaptcha), a project started at [Carnegie Mellon University](http://en.wikipedia.org/wiki/Carnegie_Mellon_University) in part by the original creator of the [CAPTCHA](http://en.wikipedia.org/wiki/CAPTCHA). I'd bet by now most people have used and are familiar with reCAPTCHA. CAPTCHAs generally consist of random letters and numbers to test if someone was a human or an automated program (usually with intent to spam). reCAPTCHA's innovation was to use actual words, words that had failed automatic digitizing when scanning mass amounts of books and using [Optical Character Recognition](http://en.wikipedia.org/wiki/Optical_character_recognition) software.

The consequence of this was that programs couldn't be written, using computer vision techniques, to simply read the image of the word, since industry-grade Optical Character Recognition software---presumably employing the most bleeding edge and sophisticated computer vision techniques---had already failed to digitize the words. This way, two problems were solved with reCAPTCHA: it was foolproof against automated attacks and circumvention, and users came to realize that by solving reCAPTCHA challenges they were helping to digitize books and thus was not a waste of time as solving randomly generated CAPTCHAs was.

I also found out about a service they had called [MailHide](http://en.wikipedia.org/wiki/ReCAPTCHA#Derivative_projects) in which they would hide part of an email address away from spammers. The full email address could be retrieved by solving a reCAPTCHA challenge, effectively preventing automated programs from harvesting email addresses for the purposes of spamming them.

That same year (2007) I figured it would be neat to write a plugin for [WordPress](http://wordpress.org/) which automatically hid any email address in a WordPress site using the MailHide technique. It gained some attention, and the then lead engineer of reCAPTCHA, Ben Maurer, contacted me via email asking if I'd be interested in volunteering to work on the then official reCAPTCHA WordPress plugin, with MailHide integrated. I accepted and worked on the plugin for a few years over which it has gotten [over 400,000 downloads](http://wordpress.org/extend/plugins/wp-recaptcha/stats/).

reCAPTCHA was acquired by Google in 2009, and the original team seems to have largely left the project. I stopped using WordPress and as a result development of the plugin halted. The plugin is open source, however, and available [on github](http://github.com/blaenk/wp-recaptcha).

</div>

##### The Instagib Project: Standalone Instagib Game Based on Quake 3 Engine (id Tech 3) {#the-instagib-project .collapse}

<div class="collapsible">

For the longest time, my favorite competitive game was a particular kind of instagib mod for the [Jedi Outcast](http://en.wikipedia.org/wiki/Star_Wars_Jedi_Knight_II:_Jedi_Outcast) (JO) and [Jedi Academy](http://en.wikipedia.org/wiki/Star_Wars_Jedi_Knight:_Jedi_Academy) (JA, the sequel) games called [disruption instagib](http://archives.thejediacademy.net/index.php/Disruption). I mainly played this on Jedi Outcast which was the older one of the two, because the server I preferred to play on in Jedi Academy shut down but one still existed in Jedi Outcast (somehow). Given that this was a pretty niche mod in a pretty old game (considering Jedi Academy had already been released), I wished to somehow make it available to more people.

Both of these games used the Quake 3 engine ([id Tech 3](http://en.wikipedia.org/wiki/Id_Tech_3)), so when the Quake 3 source code was released under the GPL and the source was cleaned up and optimized by the [ioquake3](http://ioquake3.org) project, I decided to try to port the mod and the feel of JO/JA into a standalone mod. The reason for wanting to make it into a standalone game was because although instagib mods have been around for a very long time for pretty much any game, they tend to be relegated to just that: mods. As a result, you have a variety of different flavors of instagib, who's play-style is determined by the game for which it is a mod. This is fine, but it has the effect of fragmenting the instagib community. As a result, there are usually few servers available.

So in 2006-2007 I decided to develop a standalone Instagib game. The game used art assets from the OpenArena project with custom UI and other assets designed by two of my friends. The game had team-colored rail shots and rail jumping was implemented, aside from traditional instagib mechanics. I had written a custom [NSIS](http://nsis.sourceforge.net/Main_Page) installer script to generate an installer binary for Windows. I also had Linux tarballs and Mac OS X application bundles. Aside from this, I had developed a build and deployment system with Python, which allowed people to have the latest versions of binaries and art assets.

 I ultimately abandoned the project as I became distracted by other projects. The source used to be on a self-hosted subversion server back when it was actively developed. I intend to push the source to github in the near future.

Recently, however---as a result of Disney [shutting down](http://en.wikipedia.org/wiki/LucasArts#Acquisition_by_Disney_and_closure_of_the_development_arm) LucasArts---[Raven Software](http://en.wikipedia.org/wiki/Raven_Software), the creators of Jedi Outcast and Jedi Academy, decided to release the source code to both games under the GPL. I look forward to developing a canonical disruption instagib mod again.

</div>

##### Musicaster: Homemade last.fm {#musicaster .collapse}

<div class="collapsible">

This was an application I wrote back in 2006-2007 which was a combination of client-side C# and server-side PHP. The application was basically similar to [last.fm](http://last.fm), in which a client-side application scans popular media players for the currently playing song and then transmits the information to a server-side endpoint for aggregation. The client-side C# application was written with modularity in mind, so that one would simply implement an interface, generate a DLL, and place it in the same directory as the executable to add support for more players. I added such plugins for iTunes, Windows Media Player, and Winamp. Information about the currently playing song was then sent to an endpoint of the user's choosing. At the time I also wrote a WordPress plugin which displayed this information in a blog's description text (usually under the blog title/name).

After I had done all this, one of the friends I showed it to said, "Oh, so it's like last.fm?" I had never heard of last.fm before this and the whole time I had thought I had created something quite innovative.

</div>

##### MyPod: iPod Music Navigator and Retriever {#mypod .collapse}

<div class="collapsible">

This was my first C# application which I wrote back in 2006-2007. I created it in response to an instance in which I wanted to back-up some of the music I had transferred to my iPod. The application understood the file structure of the iPod as it was back then, which consisted of several nested directories each storing about four audio files with seemingly randomly generated names. MyPod simply walked this file structure and used [TagLib](http://taglib.github.io/) to expose the actual file information to the user in a data list. The user then specified which files they wanted to back up and they were then transferred to a location and naming template (i.e. artist - title) of their choosing.

</div>

