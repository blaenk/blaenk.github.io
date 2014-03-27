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
