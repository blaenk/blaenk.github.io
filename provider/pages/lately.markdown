---
title: Lately
published: 2012-12-28
comments: off
toc: off
---

This page lists some of the things I've been up to lately.

## Working On

* A game I've been wanting to work on for a while now. It's being developed with CMake, C++11, LuaJIT, and architected from the beginning to facilitate cross-platform development.

* An audio player, though it'll ultimately be used for something else other than general audio playback. I'm decoding audio frames using [ffmpeg](http://www.ffmpeg.org/)'s libavcodec, libavformat, libavutil, and libswresample. Playback is currently done using [PulseAudio](http://www.freedesktop.org/wiki/Software/PulseAudio) although I intend to switch to a cross-platform library like [PortAudio](http://www.portaudio.com/). It works with any format/container/codec supported by ffmpeg. Source will be up soon. **Update**: Ported to [PortAudio](http://www.portaudio.com/), now runs on Windows!

## Reading

This is what I'm currently reading, but I also keep a list of [books I've read](/reads).

<img src="/images/books/wayofkings.jpg" class="right" width="128">

* [The Way of Kings](http://amzn.com/B003P2WO5E) --- My brother had a physical copy of this book lying around for months because he read on reddit that it was a very good book, but I paid no attention to it.

    However, after recently finishing the _A Song of Ice and Fire_ [prequels], I was left with a craving for that kind of world, so I looked online and saw that the general consensus was that it was indeed very good. It's written by Brandon Sanderson, who I wasn't familiar with, but he already had other popular series, and also masterfully finished the late Robert Jordan's _The Wheel of Time_ by writing books 12 through 14.

    The series is called _The Stormlight Archive_. The setting is the typical medieval fantasy world, somewhat like the one from _A Song of Ice and Fire_. It's a little heavier on the magic, but only slightly, and it's treated similar to how it's treated in ASOIAF: there are magical items in the world, but they are scarce, and people don't really understand how they work because they were created thousands of years ago. It has many _very_ interesting [concepts] which I've come to love.

    It's around the same length as the typical ASOIAF book, and has POV chapters as well, though not in as strict a fashion as ASOIAF. I am _really_ liking it, and supposedly the author intends for there to be 36-volumes written with this world as a setting :)

[prequels]: http://en.wikipedia.org/wiki/Tales_of_Dunk_and_Egg
[concepts]: http://en.wikipedia.org/wiki/The_Stormlight_Archive#Concepts

*[ASOIAF]: A Song of Ice and Fire
*[POV]: Point of View

<img src="/images/books/scala.jpg" class="right" width="128">

* [Programming in Scala](http://amzn.com/0981531644) --- I've been interested in Scala as a promising compromise between a purely functional statically typed language like Haskell and an imperative dynamic language like Ruby or Python. I'm particularly interested in the possible benefits of leveraging a VM as time-tested and robust as the JVM compared to other languages' perhaps more ad-hoc solutions. I'm also interested in it from a multicore utilization perspective with its actor concurrency model which resembles Erlang's, but with access to Scala/Java's perhaps more numerous packages.

<img src="/images/books/got-pt.jpg" class="right" width="128">

* [A Guerra dos Tronos](http://pt.wikipedia.org/wiki/A_Game_of_Thrones) --- The Portuguese translation of Game of Thrones, the first book in the [A Song of Ice and Fire](http://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) series (pt: As Crônicas de Gelo e Fogo).
	
	Even during my first read-through of all five books that are currently out, I had intended to re-read the series in a different language. A friend is currently listening to the audiobook, so I decided now was as good a time as any to start. I have always wanted to learn a bit of Portuguese, so I looked around and found the whole set of books for my Kindle.

	The Portuguese text renders nicely on the Kindle and all, but it'd make the reading process a lot more efficient if I could substitute the English dictionary with a Portuguese-to-English dictionary for when I get stuck on a word that I can't resolve using context clues or Spanish and French.

<img src="/images/books/debugging.jpg" class="right" width="128">

* [The Art of Debugging with GDB, DDD, and Eclipse](http://amzn.com/1593271743) --- Another great book by No Starch Press, publishers of TLPI. I unfortunately never really spent the time to really understand how to use debuggers past basic functionality, in this case [GDB](http://www.gnu.org/software/gdb/). I knew enough to get by, but it always seemed a little archaic or very complicated, instead opting for so-called "printf debugging," in which one simply adds a bunch of print statements to find the source of various problems. This, I've come to realize, is an exercise in masochism. Really learning the various tools available to one through a debugger is enlightening.

## Studying

* Statistics: Three courses offered by UC Berkeley covering [descriptive statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594), [probabilities](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685), and [inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825). I'm taking them to freshen up on the subjects, to reinforce my foundation for understanding machine learning.

* [Parallel Programming with GPUs](https://www.udacity.com/course/cs344): A class that teaches GPGPU programming using [CUDA](http://en.wikipedia.org/wiki/CUDA).

*[GRRM]: George RR Martin
*[GPGPU]: General Purpose GPU
