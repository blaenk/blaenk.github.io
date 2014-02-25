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

<img src="/images/books/got-pt.jpg" class="right" width="128">

* [A Guerra dos Tronos](http://pt.wikipedia.org/wiki/A_Game_of_Thrones) --- The Portuguese translation of Game of Thrones, the first book in the [A Song of Ice and Fire](http://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) series (pt: As Crônicas de Gelo e Fogo).
	
	Even during my first read-through of all five books that are currently out, I had intended to re-read the series in a different language. A friend is currently listening to the audiobook, so I decided now was as good a time as any to start. I have always wanted to learn a bit of Portuguese, so I looked around and found the whole set of books for my Kindle.

	The Portuguese text renders nicely on the Kindle and all, but it'd make the reading process a lot more efficient if I could substitute the English dictionary with a Portuguese-to-English dictionary for when I get stuck on a word that I can't resolve using context clues or Spanish and French.

## Studying

* Statistics: Three courses offered by UC Berkeley covering [descriptive statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594), [probabilities](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685), and [inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825). I'm taking them to freshen up on the subjects, to reinforce my foundation for understanding machine learning.

* [Parallel Programming with GPUs](https://www.udacity.com/course/cs344): A class that teaches GPGPU programming using [CUDA](http://en.wikipedia.org/wiki/CUDA).

*[GRRM]: George RR Martin
*[GPGPU]: General Purpose GPU
