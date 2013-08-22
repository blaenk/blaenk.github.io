---
title: Lately
published: 2012-12-28
comments: true
---

This page lists some of the things I've been up to lately.

## Working On

* An audio player, though it'll ultimately be used for something else other than general audio playback. I'm decoding audio frames using [ffmpeg](http://www.ffmpeg.org/)'s libavcodec, libavformat, libavutil, and libswresample. Playback is currently done using [PulseAudio](http://www.freedesktop.org/wiki/Software/PulseAudio) although I intend to switch to a cross-platform library like [PortAudio](http://www.portaudio.com/). It works with any format/container/codec supported by ffmpeg. Source will be up soon.
    * Ported to [PortAudio](http://www.portaudio.com/)!

## Reading

This is what I'm currently reading, but I also keep a list of [books I've read](/reads).

<img src="/images/books/algorithms.jpg" class="right" width="128">

* [Algorithms](http://amzn.com/032157351X): I've been refreshing my knowledge of algorithms using this book by Sedgewick. I chose this one for this purpose because it seems to strike a balance between the catalog-formatted coverage of Skiena's book and the in-depth analysis of the classic CLRS. So far it has worked perfectly fine, at times looking to CLRS for more information.

    Only one thing really bothered me about the book and that is that it teaches left-leaning red-black trees -- a creation of the author himself -- instead of classical red-black trees, _without_ warning the reader. The main claim to LLRBs that I'm aware of is that they're simpler to teach because they're more consistent, only allowing left-leaning red links, thereby reducing every imbalance to a matter of rotating into left-leaning red links.

    I discovered that the book was teaching LLRBs and not classical RB trees when it got around to "explaining" deletion from LLRBs, in quotes because it left it as an "exercise to the reader." Granted, it provided solution code to the exercise but I figured that I would find some more information online to reinforce my understanding; RB tree deletion is notoriously complex. Of course, every explanation and implementation I found online was entirely different from the red-black trees the book explained.

    I ended up reading through the wikipedia page on red-black trees, which is one of the most dense articles I've read, even just the deletion section was pretty dense. However, the actual implementation was pretty straightforward, so much so apparently that even the Linux kernel uses it.

    I'm currently on the final chapter which covers string algorithms such as sorting, trie structures, substring search, regular expressions, and compression.

<img src="/images/books/gof.jpg" class="right" width="128">

* [Design Patterns](http://amzn.com/0201633612): This is also known as the classical gang of four (GoF) book. I had already read [a book on design patterns](/reads/#headfirst-designpatterns), but while being readable enough, the examples that were used to explain the design patterns were so heavily contrived and ridiculous that I had trouble retaining any of it. I wasn't a huge fan of the book's style either -- which I think is common to all "Head First" books -- because it seemed to treat the reader as if they had a maximum attention span of three seconds: every page was littered with figures and pictures and comic-strip style dialog bubbles and so on.

    On the other hand, the GoF book is the canonical book on the subject and gets straight to the point. Despite being a relatively old book -- the latest edition, which I purchased, was made in 1994 -- it's still very relevant, in fact I consider it a testament to the book's lasting relevance that the authors haven't deemed it necessary to release a new version. Patterns are explained with C++ (and sometimes Smalltalk) compared to Head First's Java.

<img src="/images/books/got-pt.jpg" class="right" width="128">

* [A Guerra dos Tronos](http://pt.wikipedia.org/wiki/A_Game_of_Thrones): The Portuguese translation of Game of Thrones, the first book in the [A Song of Ice and Fire](http://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) series (pt: As Crônicas de Gelo e Fogo).
	
	Even during my first read-through of all five books that are currently out, I had intended to re-read the series in a different language. A friend is currently listening to the audiobook, so I decided now was as good a time as any to start. I have always wanted to learn a bit of Portuguese, so I looked around and found the whole set of books for my Kindle.

	The Portuguese text renders nicely on the Kindle and all, but it'd make the reading process a lot more efficient if I could substitute the English dictionary with a Portuguese-to-English dictionary for when I get stuck on a word that I can't resolve using context clues or Spanish and French.

<img src="/images/books/debugging.jpg" class="right" width="128">

* [The Art of Debugging with GDB, DDD, and Eclipse](http://amzn.com/1593271743): Another great book by No Starch Press, publishers of TLPI. I unfortunately never really spent the time to really understand how to use debuggers past basic functionality, in this case [GDB](http://www.gnu.org/software/gdb/). I knew enough to get by, but it always seemed a little archaic or very complicated, instead opting for so-called "printf debugging," in which one simply adds a bunch of print statements to find the source of various problems. This, I've come to realize, is an exercise in masochism. Really learning the various tools available to one through a debugger is enlightening.

## Studying

* [Digital Signal Processing](https://www.coursera.org/course/dsp): A DSP course taught by professors from [École Polytechnique Fédérale de Lausanne](https://en.wikipedia.org/wiki/%C3%89cole_Polytechnique_F%C3%A9d%C3%A9rale_de_Lausanne) in Switzerland.
* [Machine Learning](https://class.coursera.org/ml-2012-002/lecture/index): My goal is to apply Machine Learning concepts to Digital Signal Processing, such as for [onset detection](http://www.cs.usc.edu/research/08-895.pdf).
* [Parallel Programming with GPUs](https://www.udacity.com/course/cs344): A class that teaches GPGPU programming using [CUDA](http://en.wikipedia.org/wiki/CUDA).

*[GPGPU]: General Purpose GPU
