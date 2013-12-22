---
title: Reads
published: July 5, 2013
comments: off
---

This is a list of books I've finished reading recently, in reverse chronological order. You can also see a list of books 
I'm [currently reading](/lately/#reading).

----

<img src="/images/books/algorithms.jpg" class="right" width="128">

<a href="http://amzn.com/032157351X" name="algorithms">Algorithms</a> --- I've been refreshing my knowledge of algorithms using this book by Sedgewick. I chose this one for this purpose because it seems to strike a balance between the catalog-formatted coverage of Skiena's book and the in-depth analysis of the classic CLRS. So far it has worked perfectly fine, at times looking to CLRS for more information.

Only one thing really bothered me about the book and that is that it teaches left-leaning red-black trees --- a creation of the author himself --- instead of classical red-black trees, _without_ warning the reader. The main claim to LLRBs that I'm aware of is that they're simpler to teach because they're more consistent, only allowing left-leaning red links, thereby reducing every imbalance to a matter of rotating into left-leaning red links.

I discovered that the book was teaching LLRBs and not classical RB trees when it got around to "explaining" deletion from LLRBs, in quotes because it left it as an "exercise to the reader." Granted, it provided solution code to the exercise but I figured that I would find some more information online to reinforce my understanding; RB tree deletion is notoriously complex. Of course, every explanation and implementation I found online was entirely different from the red-black trees the book explained.

I ended up reading through the wikipedia page on red-black trees, which is one of the most dense articles I've read, even just the deletion section was pretty dense. However, the actual implementation was pretty straightforward, so much so apparently that even the Linux kernel uses it.

----

<img src="/images/books/tlpi.jpg" class="right" width="128">

<a href="http://amzn.com/1593272200" name="tlpi">The Linux Programming Interface</a> --- This book concerns the POSIX API as defined by various standards --- e.g., POSIX.1-2001/SUSv3, POSIX.1-2008/SUSv4 --- and platform-specific deviations. It has an emphasis on Linux, at times covering Linux-specific APIs such as epoll and inotify. The book comes in at a hefty 64 chapters and 1500+ pages, covering every possible topic including signals, processes, threads, interprocess-communication (IPC), sockets, terminals and pseudoterminals, evented I/O, and more.

At the time of writing, the book's reviews consist of 52 five-star reviews and one four-star review (simply because the reviewer didn't like the binding, which I think is excellent). It took me many months to finish, in part from becoming distracted by other books, but it was worth it because every chapter connected many dots, triggering epiphanies left and right. It is liberating to have this much of a deeper understanding of Linux and POSIX systems in general.

----

<img src="/images/books/cppprimer.jpg" class="right" width="128">

<a href="http://amzn.com/0321714113" name="cpp-primer">C++ Primer</a> ---  The 5th edition of the book was specifically updated for the new C++11 standard release. At one point I came to the realization that I didn't have enough experience with C++, and that my understanding of it was hazy at best. Recognizing this, I dedicated myself to bettering my understanding of C++ by rereading a book I already had on C++. However, I felt I was missing out on the new C++11 features that were recently introduced.

C++ Primer is what I look for in every programming book. It is direct --- to the point, thanks in part to the assumption that the reader is already familiar with programming, or even C++ itself. It diligently scours every nook and cranny of the C++11 standard, almost reading as a commentary of it, and in so doing, masterfully conveys the various intricacies of shared and unique pointers, move semantics, lambdas, variadic templates, and more.

----

<img src="/images/books/lyah.jpg" class="right" width="128">

<a href="http://amzn.com/1593272839" name="lyah">Learn You a Haskell</a> --- This book is [freely available](http://learnyouahaskell.com/) online, but I decided to buy it in print form to support the author. This was the first book I read on the subject of Haskell. However, after a few chapters I felt it was more concerned with the theory than practical usage of Haskell, so I decided to switch over to _Real World Haskell_.

After reading RWH, however, I had an insatiable interest in Haskell, and so I wanted to see if I had missed anything of note in this book, so I continued where I had left off. Indeed, after having seen practical uses of Haskell in RWH, I developed an appreciation for the theory that LYAH preoccupied itself with and came to appreciate it's more in-depth analysis of functors, applicative functors, monoids, and monads.

----

<img src="/images/books/rwh.jpg" class="right" width="128">

<a href="http://amzn.com/0596514980" name="rwh">Real World Haskell</a> --- This book is [freely available](http://book.realworldhaskell.org/read/) online, which is how I read it. I initially bought _Learn You A Haskell_, but I felt like it beat around the bush too much. Conversely, I found _Real World Haskell_ refreshingly direct and practical, living up to its name.

For the longest time I had wanted to learn Haskell, but I wanted to learn it in a practical context so that I may develop a realistic idea of what I can do with it. Specifically, I learned Haskell for the purpose of doing some digital signal processing in order to create a music visualizer. This book was very direct with regard to its teaching practical usage of Haskell. As a result, I was able to see how functors, applicative functors, monoids, monads, monad transformers --- and other seemingly-abstract concepts --- actually helped in developing real-world applications.

----

<img src="/images/books/dspguide.jpg" class="right" width="128">

<a href="http://amzn.com/0966017633" name="dspguide">The Scientist and Engineer's Guide to Digital Signal Processing</a> --- This book is [freely available](http://www.dspguide.com/pdfbook.htm) online. I initially began reading it in PDF format, but found it to be so good that I decided to buy the book to support the author.

For the longest time I had wanted to learn digital signal processing, but I had absolutely no idea where to begin. I had heard of some books that were deemed "classic" and thus oft-recommended, but these books were heavy-handed with the math and a bit too abstract for a beginner.

This book, on the other hand, was exactly what I needed, and best of all it was available for _free_ online. Right from the start it got right down to business and began teaching fundamental DSP concepts in ways that were very simple to visualize. For example, it explained the concept of convolution through an imaginary "[convolution machine](http://www.dspguide.com/ch6/4.htm)," which forms the basis of my [naive implementation](/posts/naive-convolution-in-haskell/) of convolution in Haskell.

Best of all, however, was that it provided example algorithm implementations of every concept it discussed. I feel this is a testament to the author's foresight, as the book was written during a transitional period in which DSP was beginning to shift from dedicated hardware to general purpose CPUs.

----

<img src="/images/books/direct3d11.jpg" class="right" width="128">


<a href="http://amzn.com/1936420228" name="d3d11">Introduction to 3D Game Programming with DirectX 11</a> --- After reading the 3D Math book mentioned below, I wanted to learn a graphics API to apply my newfound knowledge. I was previously familiar with basic, "immediate mode" OpenGL. However, modern graphics development favors the programmable pipeline, in which one uploads the vertex buffer to the GPU and writes vertex and pixel/fragment shaders to operate on that data.

I decided to choose Direct3D 11 as the API to learn because I felt it would be a bit more organized and consistent than OpenGL (cf. extensions), OpenGL's advantages notwithstanding. Indeed, I found the Direct3D 11 API to be pretty clean and straightforward. With it I covered traditional lighting, texturing, blending, stenciling, as well as the newer functionality like geometry, compute, and tessellation shaders. The book also covered some more advanced topics such as cube, normal, and displacement mapping, particle systems with stream-out, shadow mapping, and ambient occlusion.

----

<img src="/images/books/3dmath.jpg" class="right" width="128">

<a href="http://amzn.com/1568817231" name="3dmath">3D Math Primer for Graphics and Game Development</a> --- For the longest time I struggled with comprehending linear algebra and other 3D graphics-related math. I could understand the concepts on their own, but not how they related to 3D graphics. I had trouble visualizing the mathematical operations.

This book helped me to not only understand it, but fall in love with it. This is one of the highest quality books that I'm proud to have on my shelf. It is a nice hard-cover book with beautiful cover art and luminous, thick pages. It's also littered with very informative color illustrations. I say all this because the writing of the book is perfect, making for an overall masterpiece of a book. It masterfully explains the mathematical operations relevant to 3D graphics and helps visualize their operation.

This book helped me understand how matrices encode coordinate space transformations which can easily be concatenated, the utility of homogeneous coordinates in perspective projection, the pros & cons of various orientation representations (Polar Coordinates, Euler Angles, Axis-Angle, and Quaternions), Quaternion spherical linear interpolation ([slerp](http://en.wikipedia.org/wiki/Slerp#Quaternion_Slerp)), and many more concepts of the modern GPU pipeline within a mathematical context.

----

<img src="/images/books/iosnerdranch.jpg" class="right" width="128">

<a href="http://amzn.com/0321773772" name="ios">iOS Programming: The Big Nerd Ranch Guide</a> --- I had previously read a book on the iOS SDK but, while it wasn't bad, I found it lacking in depth. On the other hand, this book was packed with content covering topics including view controllers, various controls, notifications, multitasking, localization, Core Animation, Objective-C Blocks (i.e. lambdas), background execution, and push notifications.

Most of this information is easily available in the Apple documentation. Instead, I used this book more like a tour of various SDK features to get an idea of what kinds of things could be achieved.

----

<img src="/images/books/hfdesignpatterns.jpg" class="right" width="128">

<a href="http://amzn.com/0596007124" name="headfirst-designpatterns">Head First Design Patterns</a> --- The experience of many developers over decades has yielded a variety of common patterns which have become essential in solving a certain kinds of problems. This book is covers a few of these patterns in a straightforward manner.

Admittedly, in my opinion, while it was fun to read and achieved its purpose, ultimately the over-use of pictures and figures, and the very contrived example use cases made it feel very indirect. I feel I could've saved more time by simply getting the classic Gang of Four [Design Patterns](http://amzn.com/0201633612) book, which serves as more of a catalog of different design patterns with direct explanations and examples.

----

<img src="/images/books/metaruby.jpg" class="right" width="128">

<a href="http://amzn.com/1934356476" name="metaprogramming-ruby">Metaprogramming Ruby</a> --- Ruby has a tradition of employing metaprogramming techniques in various libraries to create DSLs that make programming a lot simpler or straightforward, as is evident in libraries like Rails, Sinatra, Cucumber, etc. --- pretty much any Ruby library defines a form of DSL.

This book covers the details on how the language really lends itself to metaprogramming through mechanisms such as `method_missing()`, `instance_eval()`, and `define_method()` that allow one to dynamically add methods and functionality to classes at run-time. Perhaps most important is its coverage of the Ruby object model (hierarchy) complete with Eigenclasses, and how method calls are resolved in this grand context.

*[TLPI]: The Linux Programming Interface
*[LYAH]: Learn You a Haskell
*[RWH]: Real World Haskell
*[DSP]: Digital Signal Processing
