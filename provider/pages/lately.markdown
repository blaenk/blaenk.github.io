---
title: Lately
published: 2012-12-28
comments: off
toc: off
---

This page lists some of the things that I've been up to lately.

## Reading

At any moment I have a handful of books that I'm currently reading. I find that I don't have trouble keeping track of each --- oftentimes due to keeping [notes](/notes/), and having a set of books allows me to switch to something else temporarily if a particular book falls into a lull. I also keep a page of reviews of [books that I've finished](/reads/).

<img src="/images/books/onchina.jpg" class="right" width="128">

* [On China](http://amzn.com/0143121316) --- China has always interested me as seeming like a world within a world. I had previously seen some [edX courses] about China that seemed to be _very_ good, but I tend to prefer reading rather than watching when it comes to learning.

    This book by [Henry Kissinger] goes over the historical perspective that informs China's current state of affairs. Henry Kissinger is the one that orchestrated the opening of relations with China when he served as Secretary of State during the Nixon Administration, so he's no stranger to the country. The book seems to go over parts of Chinese history in chronologically leading up to modern day, and it is simply _engrossing_.

[Henry Kissinger]: http://en.wikipedia.org/wiki/Henry_Kissinger
[edX courses]: https://www.edx.org/course/harvardx/harvardx-sw12x-china-920

<img src="/images/books/haskellconpar.jpg" class="right" width="128">

* [Parallel and Concurrent Programming in Haskell](http://amzn.com/1449335942) [[notes](/notes/haskell/)] --- It's well known that Haskell makes it very easy to reason about concurrent and parallel programs, and this has proven true in my experience so far. The [software transactional memory] package provides transactional concurrency primitives that make writing safe, concurrent programs very simple.

    One of the more common difficulties in reasoning about Haskell programs is the property of being non-strict, which in GHC is handled via lazy evaluation. What this means is that one has to be careful about how expressions are evaluated, particularly with _when_ they will be evaluated. This is especially important in parallel evaluation contexts, where the parallel evaluation can end up only evaluating the expression to weak-head normal form, i.e. the outer-most constructor. For this reason, this book starts out with thorough coverage of so-called evaluation strategies, which decouple a given algorithm from the way in which it's to be evaluated (e.g. in parallel or not).

[software transactional memory]: http://en.wikipedia.org/wiki/Software_transactional_memory

<img src="/images/books/clojure.jpg" class="right" width="128">

* [Clojure Programming](http://amzn.com/1449394701) [[notes](/notes/clojure/)] --- I've had a recent interest in JVM languages. I already looked into [Scala] and am unsure what I think of it so far. I like a lot of it but I feel like it's overly complex and context-dependent; it reminds me of C++. I decided to look into the other emerging JVM language, the lisp-like Clojure, and its level of simplicity and consistency feels refreshing in comparison.

    One of my computer science professors once told me that the reason he likes [Scheme] is because he likes a language where he could easily wrap his head around its computational model so that he may work as effectively as possible. At the time I found his choice of words interesting, "computational model." Now that I'm learning Clojure and the so-called _special forms_ which are [sufficient to describe any possible computation], I understand what he means. I have to agree that the simplicity in this model is empowering.

*[JVM]: Java Virtual Machine
[Scala]: /notes/scala/
[Scheme]: http://en.wikipedia.org/wiki/Scheme_(programming_language)
[sufficient to describe any possible computation]: http://www.paulgraham.com/rootsoflisp.html

<img src="/images/books/erlang.jpg" class="right" width="128">

* [Programming Erlang](http://amzn.com/193778553X) [[notes](/notes/erlang/)] --- Erlang was the first functional programming language I attempted to learn. The combination of foreign (to me) programming paradigm and actor concurrency model was enough to prevent me from seeing the bigger picture, and so I never finished learning it, instead opting for Haskell.

    I don't like to leave things like these unfinished, so now that I have a solid understanding of functional programming and the actor concurrency model, I'm attempting to give Erlang a second chance. I don't particularly see myself using Erlang; I think Haskell is a superior language and Scala has access to a more vibrant and robust package ecosystem. However, Erlang and its OTP library are known to be very robust, so my intention with learning it is to see what ideas I can pick up from them.

<img src="/images/books/angularjs.jpg" class="right" width="128">

* [Mastering Web Application Development with AngularJS](http://amzn.com/1782161821) [[notes](/notes/angular/)] --- I've found [Angular.js] to be an immensely powerful tool for front-end development. Gone are the days of manual, ad-hoc data-binding. Like other MVC-like front-end frameworks, Angular encourages structure and testability over jQuery spaghetti in a front-end application.

    Angular's templates are directly written in HTML using tag attributes, or _directives_, which are given meaning in code. This makes HTML itself highly extensible and gives the templates clearer meanings. Angular provides a leap in productivity that is similar to jQuery's in its day.

[Angular.js]: http://angularjs.org

<img src="/images/books/mistborn3.jpg" class="right" width="128">

* [Mistborn: The Hero of Ages](http://amzn.com/0765356147) --- This is the sequel to [The Well of Ascension] and the third and final book in the original _Mistborn_ trilogy. The end of the second book completely changed the stakes of the world and now the characters must deal with the repercussions.

    The world and characters are very different from how they were in the first book. By now the characters have been really developed from the events of the past two books and are efficiently and expertly attempting to deal with a problem that otherwise seems insurmountable to them. Meanwhile, those characters that seemed to have reached the extent of their development have experienced events that have catalyzed further development in ways that would seem uncharacteristic of how we knew them to be.

    This is a very beautiful series. It has been tragic, nerve-racking, deep, funny, and heartwarming. There is a recurrent theme of attempting the impossible, of perseverance and determination in the face of apparently unequivocal doom. Of survival.

[The Well of Ascension]: /reads/#mistborn2

## Studying

* Statistics: Three courses offered by UC Berkeley covering [descriptive statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594), [probabilities](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685), and [inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825). I'm taking them to freshen up on the subjects, to reinforce my foundation for understanding machine learning.

* [Parallel Programming with GPUs](https://www.udacity.com/course/cs344): A class that teaches GPGPU programming using [CUDA](http://en.wikipedia.org/wiki/CUDA).

*[GRRM]: George RR Martin
*[GPGPU]: General Purpose GPU

## Working On

* A game I've been wanting to work on for a while now. It's being developed with CMake, C++11, LuaJIT, and architected from the beginning to facilitate cross-platform development.

* An audio player, though it'll ultimately be used for something else other than general audio playback. I'm decoding audio frames using [ffmpeg](http://www.ffmpeg.org/)'s libavcodec, libavformat, libavutil, and libswresample. Playback is currently done using [PulseAudio](http://www.freedesktop.org/wiki/Software/PulseAudio) although I intend to switch to a cross-platform library like [PortAudio](http://www.portaudio.com/). It works with any format/container/codec supported by ffmpeg. Source will be up soon. **Update**: Ported to [PortAudio](http://www.portaudio.com/), now runs on Windows!

