---
title: Reads
published: July 5, 2013
---

This is a list of books I've finished reading recently, in reverse chronological order. You can also see a list of books 
I'm [currently reading](/lately/#reading).

---

<img src="/images/books/tlpi.jpg" class="right" width="128">

[The Linux Programming Interface](http://amzn.com/1593272200) -- This book concerns the POSIX API as defined by various standards -- e.g., POSIX.1-2001/SUSv3, POSIX.1-2008/SUSv4 -- and platform-specific deviations. It has an emphasis on Linux, at times covering Linux-specific APIs such as epoll and inotify. The book comes in at a hefty 64 chapters and 1500+ pages, covering every possible topic including signals, processes, threads, interprocess-communication (IPC), sockets, terminals and pseudoterminals, evented I/O, and more.

> _The Linux Programming Interface_ is the most comprehensive single-volume work on the Linux and UNIX programming interface, and a book that's destined to become a new classic.
> <footer><strong>Back Cover</strong> <cite>TLPI</cite></footer>

At the time of writing, the book's reviews consist of 52 five-star reviews and one four-star review (simply because the reviewer didn't like the binding, which I think is excellent). It took me many months to finish, in part from becoming distracted by other books, but it was worth it because every chapter connected many dots, triggering epiphanies left and right. It is liberating to have this much of a deeper understanding of Linux and POSIX systems in general.

My goal now is to dive even deeper by exploring the kernel space.

---

<img src="/images/books/cppprimer.jpg" class="right" width="128">

[C++ Primer](http://amzn.com/0321714113) -- The 5th edition of the book was specifically updated for the new C++11 standard release. At one point I came to the realization that I didn't have enough experience with C++, and that my understanding of it was hazy at best. Recognizing this, I dedicated myself to bettering my understanding of C++ by rereading a book I already had on C++. However, I felt I was missing out on the new C++11 features that were recently introduced.

C++ Primer is what I look for in every programming book. It is direct -- to the point, thanks in part to the assumption that the reader is already familiar with programming, or even C++ itself. It diligently scours every nook and cranny of the C++11 standard, almost reading as a commentary of it, and in so doing, masterfully conveys the various intricacies of shared and unique pointers, move semantics, lambdas, variadic templates, and more.

---

<img src="/images/books/lyah.jpg" class="right" width="128">

[Learn You a Haskell](http://amzn.com/1593272839) -- This book is [freely available](http://learnyouahaskell.com/) online, but I decided to buy it in print form to support the author. This was the first book I read on the subject of Haskell. However, after a few chapters I felt it was more concerned with the theory than practical usage of Haskell, so I decided to switch over to _Real World Haskell_.

After reading RWH, however, I had an insatiable interest in Haskell, and so I wanted to see if I had missed anything of note in this book, so I continued where I had left off. Indeed, after having seen practical uses of Haskell in RWH, I developed an appreciation for the theory that LYAH preoccupied itself with and came to appreciate it's more in-depth analysis of functors, applicative functors, monoids, and monads.

---

<img src="/images/books/rwh.jpg" class="right" width="128">

[Real World Haskell](http://amzn.com/0596514980) -- This book is [freely available](http://book.realworldhaskell.org/read/) online, which is how I read it. I initially bought _Learn You A Haskell_, but I felt like it beat around the bush too much. Conversely, I found _Real World Haskell_ refreshingly direct and practical, living up to its name.

For the longest time I had wanted to learn Haskell, but I wanted to learn it in a practical context so that I may develop a realistic idea of what I can do with it. Specifically, I learned Haskell for the purpose of doing some digital signal processing in order to create a music visualizer. This book was very direct with regard to its teaching practical usage of Haskell. As a result, I was able to see how functors, applicative functors, monoids, monads, monad transformers -- and other seemingly-abstract concepts -- actually helped in developing real-world applications.

---

<img src="/images/books/dspguide.jpg" class="right" width="128">

[The Scientist and Engineer's Guide to Digital Signal Processing](http://amzn.com/0966017633) -- This book is [freely available](http://www.dspguide.com/pdfbook.htm) online. I initially began reading it in PDF format, but found it to be so good that I decided to buy the book to support the author.

For the longest time I had wanted to learn digital signal processing, but I had absolutely no idea where to begin. I had heard of some books that were deemed "classic" and thus oft-recommended, but these books were heavy-handed with the math and a bit too abstract for a beginner.

This book, on the other hand, was exactly what I needed, and best of all it was available for _free_ online. Right from the start it got right down to business and began teaching fundamental DSP concepts in ways that were very simple to visualize. For example, it explained the concept of convolution through an imaginary "[convolution machine](http://www.dspguide.com/ch6/4.htm)," which forms the basis of my [naive implementation](/posts/naive-convolution-in-haskell/) of convolution in Haskell.

Best of all, however, was that it provided example algorithm implementations of every concept it discussed. I feel this is a testament to the author's foresight, as the book was written during a transitional period in which DSP was beginning to shift from dedicated hardware to general purpose CPUs.

---

<img src="/images/books/direct3d11.jpg" class="right" width="128">

[Introduction to 3D Game Programming with DirectX 11](http://amzn.com/1936420228) -- After reading the 3D Math book mentioned below, I wanted to learn a graphics API to apply my newfound knowledge. I was previously familiar with basic, "immediate mode" OpenGL. However, modern graphics development favors the programmable pipeline, in which one uploads the vertex buffer to the GPU and writes vertex and pixel/fragment shaders to operate on that data.

I decided to choose Direct3D 11 as the API to learn because I felt it would be a bit more organized and consistent than OpenGL (cf. extensions), OpenGL's advantages notwithstanding. Indeed, I found the Direct3D 11 API to be pretty clean and straightforward. With it I covered traditional lighting, texturing, blending, stenciling, as well as the newer functionality like geometry, compute, and tessellation shaders. The book also covered some more advanced topics such as cube, normal, and displacement mapping, particle systems with stream-out, shadow mapping, and ambient occlusion.

---

<img src="/images/books/3dmath.jpg" class="right" width="128">

[3D Math Primer for Graphics and Game Development](http://amzn.com/1568817231) -- For the longest time I struggled with comprehending linear algebra and other 3D graphics-related math. I could understand the concepts on their own, but not how they related to 3D graphics. I had trouble visualizing the mathematical operations.

This book helped me to not only understand it, but fall in love with it. This is one of the highest quality books that I'm proud to have on my shelf. It is a nice hard-cover book with beautiful cover art and luminous, thick pages. It's also littered with very informative color illustrations. I say all this because the writing of the book is perfect, making for an overall masterpiece of a book. It masterfully explains the mathematical operations relevant to 3D graphics and helps visualize their operation.

This book helped me understand how matrices encode coordinate space transformations which can easily be concatenated, the utility of homogeneous coordinates in perspective projection, the pros & cons of various orientation representations (Polar Coordinates, Euler Angles, Axis-Angle, and Quaternions), Quaternion spherical linear interpolation ([slerp](http://en.wikipedia.org/wiki/Slerp#Quaternion_Slerp)), and many more concepts of the modern GPU pipeline within a mathematical context.

---

<img src="/images/books/iosnerdranch.jpg" class="right" width="128">

[iOS Programming: The Big Nerd Ranch Guide](http://amzn.com/0321773772) -- I had previously read a book on the iOS SDK but, while it wasn't bad, I found it lacking in depth. On the other hand, this book was packed with content covering topics including view controllers, various controls, notifications, multitasking, localization, Core Animation, Objective-C Blocks (i.e. lambdas), background execution, and push notifications.

Most of this information is easily available in the Apple documentation. Instead, I used this book more like a tour of various SDK features to get an idea of what kinds of things could be achieved.

---

<img src="/images/books/hfdesignpatterns.jpg" class="right" width="128">

[Head First Design Patterns](http://amzn.com/0596007124) -- The experience of many developers over decades has yielded a variety of common patterns which have become essential in solving a certain kinds of problems. This book is covers a few of these patterns in a straightforward manner.

Admittedly, in my opinion, while it was fun to read and achieved its purpose, ultimately the over-use of pictures and figures, and the very contrived example use cases made it feel very indirect. I feel I could've saved more time by simply getting the classic Gang of Four [Design Patterns](http://amzn.com/0201633612) book, which serves as more of a catalog of different design patterns with direct explanations and examples.

---

<img src="/images/books/metaruby.jpg" class="right" width="128">

[Metaprogramming Ruby](http://amzn.com/1934356476) -- Ruby has a tradition of employing metaprogramming techniques in various libraries to create DSLs that make programming a lot simpler or straightforward, as is evident in libraries like Rails, Sinatra, Cucumber, etc. -- pretty much any Ruby library defines a form of DSL.

This book covers the details on how the language really lends itself to metaprogramming through mechanisms such as `method_missing()`, `instance_eval()`, and `define_method()` that allow one to dynamically add methods and functionality to classes at run-time. Perhaps most important is its coverage of the Ruby object model (hierarchy) complete with Eigenclasses, and how method calls are resolved in this grand context.

*[TLPI]: The Linux Programming Interface
*[LYAH]: Learn You a Haskell
*[RWH]: Real World Haskell
*[DSP]: Digital Signal Processing