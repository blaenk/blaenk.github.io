---
title: Lately
published: 2012-12-28
comments: off
toc: off
---

This page lists some of the things that I've been up to lately. I try my best to keep it up-to-date.

## Reading

At any moment I have a handful of books that I'm currently reading. I find that I don't have trouble keeping track of each, thanks in part to keeping [notes](/notes/), and having a set of books allows me to switch to something else temporarily if a particular book falls into a lull. I also keep a page of reviews of [books that I've finished](/reads/).

<img src="/images/books/kingkiller2.jpg" class="right" width="128">

* [The Wise Man's Fear](http://amzn.com/0756407125) --- After finishing the [first book in the series](/reads/#kingkiller1) and liking it enough, I decided to continue on to the sequel. Where Sanderson is well known for his so-called "worldbuilding," Rothfuss is well known for his very beautiful prose. In a genre increasingly filled with books containing complex worlds and multiple POVs, _The Kingkiller Chronicle_ is a refreshing change of pace.
    
    In other books, the world-at-large is not really explored and is largely incidental; the aspects we glean about it are usually only those that are somehow related to the characters. This is only natural, but _Kingkiller Chronicle_ is more or less a story about the everyday life of the main character, Kvothe. This affords a level of familiarity with the world that I haven't seen in other books that I've read.

<img src="/images/books/bitcoin.jpg" class="right" width="128">

* [Mastering Bitcoin](http://amzn.com/1449374042) --- I just now began studying Bitcoin, but from the moment I learned of its existence I recognized that there was clearly something revolutionary about the fact that this was a decentralized monetary system which was used---and presumably by extension trusted---by people.

    I continue to be amazed at the foresight of Satoshi and sheer ingenuity of the solutions employed in Bitcoin. This book---and Bitcoin in general---triggered an epiphany in me: I realized that cryptography can be used for more than just maintaining user privacy, but also as a way to enforce conformance with a system.

<img src="/images/books/onchina.jpg" class="right" width="128">

* [On China](http://amzn.com/0143121316) --- China has always interested me as seeming like a world within a world. I had previously seen some [edX courses] about China that seemed to be _very_ good, but I tend to prefer reading rather than watching when it comes to learning.

    This book by [Henry Kissinger] goes over the historical perspective that informs China's current state of affairs. Henry Kissinger is the one that orchestrated the opening of relations with China when he served as Secretary of State during the Nixon Administration, so he's no stranger to the country. The book seems to go over parts of Chinese history chronologically leading up to modern day, and it is simply _engrossing_.

[Henry Kissinger]: http://en.wikipedia.org/wiki/Henry_Kissinger
[edX courses]: https://www.edx.org/course/harvardx/harvardx-sw12x-china-920

<img src="/images/books/haskellconpar.jpg" class="right" width="128">

* [Parallel and Concurrent Programming in Haskell](http://amzn.com/1449335942) [[notes](/notes/haskell/)] --- It's well known that Haskell makes it very easy to reason about concurrent and parallel programs, and this has proven true in my experience so far. The [software transactional memory] package provides transactional concurrency primitives that make writing safe, concurrent programs very simple.

    One of the more common difficulties in reasoning about Haskell programs is the property of being non-strict, which in GHC is handled via lazy evaluation. What this means is that one has to be careful about how expressions are evaluated, particularly with _when_ they will be evaluated. This is especially important in parallel evaluation contexts, where the parallel evaluation can end up only evaluating the expression to weak-head normal form, i.e. the outer-most constructor. For this reason, this book starts out with thorough coverage of so-called evaluation strategies, which decouple a given algorithm from the way in which it's to be evaluated (e.g. in parallel or not).

[software transactional memory]: http://en.wikipedia.org/wiki/Software_transactional_memory

<img src="/images/books/erlang.jpg" class="right" width="128">

* [Programming Erlang](http://amzn.com/193778553X) [[notes](/notes/erlang/)] --- Erlang was the first functional programming language I attempted to learn. The combination of foreign (to me) programming paradigm and actor concurrency model was enough to prevent me from seeing the bigger picture, and so I never finished learning it, instead opting for Haskell.

    I don't like to leave things like these unfinished, so now that I have a solid understanding of functional programming and the actor concurrency model, I'm attempting to give Erlang a second chance. I don't particularly see myself using Erlang; I think Haskell is a superior language and Scala has access to a more vibrant and robust package ecosystem. However, Erlang and its OTP library are known to be very robust, so my intention with learning it is to see what ideas I can pick up from them.

<img src="/images/books/java.jpg" class="right" width="128">

* [Java: The Complete Reference](http://amzn.com/0071808558) --- I've always "known" Java through my use of it in school where it was sometimes required, but I never formally learned it. With the release of Java 8 and the many new languages that are targeting the JVM, I felt it would be a good idea to sweep through a Java book to ensure that I understand the nook and crannies of Java.

    I was specifically looking for a book that served more as a reference instead of a beginner-oriented book that which I would have to sift through to find the nuggets of actual information, past the exercises and code examples. This book seems well regarded and has so far served that purpose.

## Studying

* Statistics: Three courses offered by UC Berkeley covering [descriptive statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594), [probabilities](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685), and [inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825). I'm taking them to freshen up on the subjects, to reinforce my foundation for understanding machine learning.

* [Parallel Programming with GPUs](https://www.udacity.com/course/cs344): A class that teaches GPGPU programming using [CUDA](http://en.wikipedia.org/wiki/CUDA).

*[GRRM]: George RR Martin
*[GPGPU]: General Purpose GPU

## Working On

* A game I've been wanting to work on for a while now. It's being developed with CMake, C++11, LuaJIT, and architected from the beginning to facilitate cross-platform development.

* An audio player, though it'll ultimately be used for something else other than general audio playback. I'm decoding audio frames using [ffmpeg](http://www.ffmpeg.org/)'s libavcodec, libavformat, libavutil, and libswresample. Playback is currently done using [PulseAudio](http://www.freedesktop.org/wiki/Software/PulseAudio) although I intend to switch to a cross-platform library like [PortAudio](http://www.portaudio.com/). It works with any format/container/codec supported by ffmpeg. Source will be up soon. **Update**: Ported to [PortAudio](http://www.portaudio.com/), now runs on Windows!

