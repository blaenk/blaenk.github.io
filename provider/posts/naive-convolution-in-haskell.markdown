---
title: Naive Convolution in Haskell
published: January 4, 2013
excerpt: Functional implementation of convolution in Haskell
tags: Haskell, Digital Signal Processing
---

* toc

[Convolution](http://en.wikipedia.org/wiki/Convolution) is a mathematical method of combining two signals to form a third signal. Passing the [Dirac delta function](http://en.wikipedia.org/wiki/Dirac_delta_function) (unit impulse) $\delta[n]$ through a linear system results in the impulse response $h[n]$. The impulse response is simply the signal resulting from passing the unit impulse (Dirac delta function) through a linear system.

## Principle

The properties of [homogeneity](http://www.cns.nyu.edu/~david/handouts/linear-systems/linear-systems.html) and [shift-invariance](http://en.wikipedia.org/wiki/Shift-invariant_system) in [Linear Time-Invariant System Theory](http://en.wikipedia.org/wiki/LTI_system_theory) hold that scaling and shifting an input signal in a linear system results in the same scaling and shifting in the output signal. Because of these properties, we can represent any impulse as a shifted and scaled delta function and consequently know what the impulse response, i.e. output signal in response to an impulse input signal, will be for that scaled and shifted impulse.

An impulse of $-3$ at the $8^{th}$ sample would be represented as a unit impulse $h$ by scaling the delta function $\delta$ by $-3$ and shifting it to the right by $8$ samples: $-3\delta[n-8]$, where $n-8$ means the $8^{th}$ sample is now the $0^{th}$. Due to homogeneity and shift invariance, we can determine the impulse response of this impulse by simply scaling and shifting the unit impulse response in the same manner. In other words:

$$-3\delta[n-8] \mapsto -3h[n-8]$$

What this means is that if we know the unit impulse response of a system, we consequently know how the system will react to _any_ impulse, not just a unit impulse. These impulse responses can then be synthesized to form the output signal that would result from running the input signal through the actual system. An example of the powerful implications of this property is [convolution reverb](http://en.wikipedia.org/wiki/Convolution_reverb), in which an impulse response of a physical or virtual space is generated and then convolved with any input signal to simulate the effect of reverberation in that space.

In short, the input signal _convolved_ with the unit impulse response results in the output signal. Convolution of input signal $x[n]$ with unit impulse $h[n]$ to generate output signal $y[n]$ is denoted as:

$$x[n] * h[n] = y[n]$$

Since convolution allows us to go from input signal $x[n]$ to output signal $y[n]$, we can conclude that convolution involves the generation of the impulse response for each impulse in the input signal as decomposed by [impulse decomposition](http://www.dspguide.com/ch5/7.htm), _as well as_ the subsequent synthesis of each impulse response, to generate the output signal.

## Definition

Convolution can be described by the so-called _convolution summation_. The convolution summation is pretty simple, and is defined as follows:

$$y[i] = \sum_{j=0}^{M-1} h[j]\ x[i-j]$$

Where the length of the output signal $y[n]$ is defined as $M + N - 1$ where $M$ is the length of the unit impulse response and $N$ is the length of the input signal.

All this says is that a given sample $y[i]$ in the output signal $y[n]$ is determined by the summation of every $i^{th}$ sample in every resultant impulse response. In effect, the summation above encodes how different samples in the resulting impulse responses contribute to a single output sample.

Natural imperative instinct might lead you to conclude that this can be easily implemented using nested iterations and arrays:

~~~ {lang="cpp" text="imperative convolution in C++ with arrays"}
const int outputLength = M + N - 1;
int *y = new int[outputLength]();

for (int i = 0; i < outputLength; ++i) {
  for (int j = 0; j < M; ++j) {
    if (i - j >= 0)
      y[i] += x[i - j] * h[j];
  }
}
~~~

But wait up! We are using Haskell, a functional programming language which typically does without both arrays and iteration. This means that to implement convolution in Haskell without the use of [Arrays](http://hackage.haskell.org/package/array) or imperative iteration loops, we need to really understand the operation occurring in the convolution summation.

### Convolution Machine

The book [The Scientist and Engineer's Guide to Digital Signal Processing](http://www.dspguide.com) uses a metaphor known as the [Convolution Machine](http://www.dspguide.com/ch6/4.htm) to help conceptualize the convolution operation at a granular level. The convolution machine is simply a theoretical machine in which the unit impulse response is:

1. wrapped onto a roller/cylinder
2. rolled over the input signal such that each sample lines up with one on the reversed impulse response
3. each lined-up pair of samples from input signal and impulse response is multiplied and each product is summed

If you're wondering why step **3** mentions a _reversed_ impulse response, imagine that you have a roller and that the impulse response is on a strip of tape. Now imagine that you apply the impulse response tape over and around the roller, such that the numbers are facing you and are in the correct order. Now, when you roll this roller over and across the input signal, from left to right, the numbers on the impulse response tape will make contact with the input signal in _reverse order_.

See [this page](http://www.dspguide.com/ch6/4.htm) for an illustration of the convolution machine in Figure 6-8.

## Implementation

Implementing the convolution machine is pretty straightforward once we are able to conceptualize what it is actually doing.

Let's start with the type signature. Since we're not using arrays, we'll represent the signals as lists of numbers. Convolution does something with two signals to produce a third signal, so the type signature is pretty straightforward:

~~~ {lang="haskell"}
convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs = undefined
~~~

In the signature, `xs` refers to the input signal and `hs` refers to the impulse response.

### Padding

Now for the implementation of `convolve`. First, consider this component of the convolution summation:

$$x[i-j]$$

When we are computing the first sample, such that $i = 0$, in the output signal $y[n]$, then at one point we need to refer to the $x[-(M-1)]$ sample where $M$ is length of impulse response. However, there are no samples to the left of the first sample.

So what we have to do is prepad the input signal with $M-1$ samples of value $0$. This padding has the added benefit of allowing us to simply map over the padded input signal to generate the output signal. This is because the convolution operation's output signal length is $M + N - 1$ where $M$ is the length of the impulse response and $N$ is the length of the input signal. The padding can be achieved with:

``` {lang="haskell"}
let pad = replicate ((length hs) - 1) 0
    ts  = pad ++ xs
```

Once we prepad the input signal with enough zero samples, we can pass the padded input signal and impulse response to a function which simulates the rolling of the convolution machine. This function will be nested within `convolve` and will simply be used as a recursive helper function.

### Let's Roll

``` {lang="haskell"}
roll :: (Num a) => [a] -> [a] -> [a]
roll _  [] = []
roll hs ts = undefined
```

The `roll` function is recursive and will simulate the actual rolling of the convolution machine over the input signal. As it rolls, it will consume the `head` of the input signal `ts`. Think of the consumption of the `head` as if the input signal is being wrapped around the roller as it rolls. The input signal `ts` will therefore eventually be empty, meaning the convolution machine has finished rolling over the entire input signal.

The `roll` function is run for every sample in the output signal. This is where the bulk of the implementation comes in. At any given sample in the input signal, we simulate the roll by zipping the input signal from that sample forward along with the impulse response. This generates a list of pairs each consisting of the input signal sample with its corresponding impulse response sample (which is being rolled over it).

If you have trouble conceptualizing this, imagine that the impulse response on the roller is tape, so that when you roll it over the input signal, the impulse response---which, remember, makes contact with the input signal in reverse---sticks to the input signal and is lined up such that each sample in the impulse response is directly over a sample of the input signal.

We then need to multiply the components of each pair with each other, i.e. the input sample multiplied by its corresponding impulse response sample. The act of zipping and multiplying the zipped up pairs can be done in one go with `zipWith (*)`. We then gather all of these products and `sum` them up. This sum is the latest computed sample in the output signal.

We construct the complete output signal by cons'ing the sample with a recursive call to `roll`, however this `roll` will concern only the next sample forward, thereby simulating rolling across the input signal.

With this information, we can finish the definition of `roll`:

``` {lang="haskell"}
roll :: (Num a) => [a] -> [a] -> [a]
roll _  [] = []
roll hs ts = let sample = sum $ zipWith (*) ts hs
             in sample : roll hs (tail ts)
```

Here is the whole convolution function `convolve` put together:

~~~ {lang="haskell" text="naive convolution in Haskell through the convolution machine"}
convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
  in roll ts (reverse hs)
  where
    roll :: (Num a) => [a] -> [a] -> [a]
    roll _  [] = []
    roll hs ts = let sample = sum $ zipWith (*) ts hs
                 in sample : roll hs (tail ts)
~~~

## Reduction

Now that we understand the concept behind convolution, we can reduce the above implementation a bit further.

The observation we should make is that the `roll` function acts like `map`, specifically over `ts`. The only detail is that on every element mapped over, the result of that element's mapping concerns the list `ts` from that element forward. If we are on the third element of `ts`, we only act on the third element forward. In other words, we are mapping over every `tail` of `ts`. Knowing this, we can change the `roll` function to a straight up `map` over `tails ts`.

However, `tails` considers `[]` to be a tail of any list---which is technically correct---so we'll always have a trailing `0` element if we do it this way. That's why we simply take the `init` of the result of `tails`, which returns every element in a list except the last one. We also still need to prepad the signal, so those lines remain:

~~~ {lang="haskell" text="a reduced form of the convolution machine implementation"}
convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
  in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)
~~~

## Parallelization

There's something to be said about how the various properties of the Haskell language come together to make certain algorithms trivially parallelizable. Green threads, single assignment, function purity and its consequent idempotence/referential transparency, among other things. I think it's interesting to note how easy it can be to parallelize this naive convolution algorithm.

### parMap

The [parallel](http://hackage.haskell.org/package/parallel) Haskell package contains various tools for parallelization. One of these is the [Control.Parallel.Strategies](http://hackage.haskell.org/packages/archive/parallel/latest/doc/html/Control-Parallel-Strategies.html) module, which defines the [`parMap`](http://hackage.haskell.org/packages/archive/parallel/latest/doc/html/Control-Parallel-Strategies.html#v:parMap) function, which maps over list elements in parallel, in essence, a parallel map:

~~~ {lang="haskell"}
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
~~~

`parMap` takes an [evaluation strategy](http://hackage.haskell.org/packages/archive/parallel/latest/doc/html/Control-Parallel-Strategies.html#t:Strategy) which is used to actually perform the evaluation in parallel. We use the [`rdeepseq`](http://hackage.haskell.org/packages/archive/parallel/latest/doc/html/Control-Parallel-Strategies.html#v:rdeepseq) evaluation strategy, which fully evaluates the argument to Normal Form (i.e. fully evaluated), as opposed to [`rseq`](http://hackage.haskell.org/packages/archive/parallel/latest/doc/html/Control-Parallel-Strategies.html#v:rseq) which merely evaluates the argument to [Weak Head Normal Form](http://en.wikibooks.org/wiki/Haskell/Graph_reduction#Weak_Head_Normal_Form) (WHNF). The `rdeepseq` strategy can only operate on arguments it knows it can fully evaluate, those that conform to the [`NFData`](http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html#t:NFData) typeclass from the [Control.Deepseq](http://hackage.haskell.org/package/deepseq) module. To conform to this, we add another type constraint to our convolution parameters:

~~~ {lang="haskell"}
parConvolve :: (NFData a, Num a) => [a] -> [a] -> [a]
~~~

Continuing forward, all we have to do now is make a drop-in replacement of `map` with `parMap`. Actually, it's not quite a drop-in replacement, because we need to supply `parMap` with the `rdeepseq` evaluation strategy:

~~~ {lang="haskell" text="a parallelized version of the reduced naive convolution algorithm"}
parConvolve :: (NFData a, Num a) => [a] -> [a] -> [a]
parConvolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
  in parMap rdeepseq (sum . zipWith (*) (reverse hs)) (init $ tails ts)
~~~

### Benchmark

The [`criterion`](http://hackage.haskell.org/packages/archive/criterion) Haskell package provides tools for benchmarking and analyzing code. The synthetic benchmark we will conduct will run each implementation with an impulse response of length 100 and an input signal of length 1,000.

In the following code, `conv` is the naive implementation, `conv'` is the reduced naive implementation, and `parConv` is the parallel implementation:

~~~ {lang="haskell" text="criterion benchmarking code"}
data ConvType = Naive | Reduced | Parallel deriving (Eq, Ord)
convTypes = Data.Map.fromList [(Naive, conv), (Reduced, conv'), (Parallel, parConv)]

main = defaultMain [
  bench "Naive Convolution" (runConv Naive),
  bench "Reduced Convolution" (runConv Reduced),
  bench "Parallelized Convolution" (runConv Parallel) ]
  where runConv ctype =
          let hs = [1..100 :: Int]
              ts = [1..1000 :: Int]
              convfn = fromJust $ Data.Map.lookup ctype convTypes
          in nf (convfn hs) ts
~~~

Compile the benchmark with:

~~~ {lang="bash"}
$ ghc --make -O2 -threaded -o conv conv.hs
~~~

Run it with:

~~~ {lang="bash"}
$ ./conv -o bench.html -r out.csv +RTS -N4
~~~

The `-o` parameter specifies an output file for generated [charts and graphs](../../../../static/html/convolution-criterion.html). The `-r` parameter specifies a comma separated value (CSV) file to output relative statistics which we use to measure performance relative to the reference, non-reduced naive implementation.

The `+RTS` parameter is a delimiter which begins parameters to the [runtime system](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html). The `-N#` parameter specifies how many cores to utilize. The machine I was using has 6 cores, but I found that using less than that lowered the amount of statistical variance. I imagine this was because the computer was able to continue its own tasks on the other two cores.

The above benchmark yielded the following results:

| Name              | % faster |
|:-----             |:--------:|
| Naive (Reference) | 0        |
| Reduced           | 0        |
| Parallel          | 54       |

The parallel version apparently really boosts performance. An important thing to realize is that when parallelizing things, it's considered best to only parallelize when the benefits outweigh the relative overhead of managing the green threads.

For example, in my tests, changing the impulse response length to 5 and the input signal length to 10 shows the parallel version to be 27% slower than the naive implementation. Also notice that the reduced version is a bit slower, for what I can only imagine to be a GHC optimization that applies to the naive implementation but not to the reduced version.

| Name              | % faster |
|:-----             |:--------:|
| Naive (Reference) | 0        |
| Reduced           | -16      |
| Parallel          | -27      |

On the other hand, increasing the impulse response length to 1,000 and the input signal length to 10,000 maintained a similar performance increase:

| Name              | % faster |
|:-----             |:--------:|
| Naive (Reference) | 0        |
| Reduced           | 0        |
| Parallel          | 48       |

## Fusion

As Christian pointed out in the comments, this naive implementation can really benefit from [Stream Fusion]. It's embarrassingly straightforward to enable stream fusion on this naive algorithm, considering that all we're doing is qualifying the list functions so that the ones from the [`stream-fusion`] package are used instead. Doing this yielded a 36% performance increase over the parallel algorithm showcased above, which was already fast. This is quite a bountiful optimization reward indeed.

[Stream Fusion]: http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/haskell-beats-C.pdf
[`stream-fusion`]: http://hackage.haskell.org/package/stream-fusion

~~~ {.haskell text="stream fusion implementation"}
import qualified Data.List.Stream as S

parConvolveSF hs xs =
  let pad = S.replicate ((S.length hs) - 1) 0
      ts  = pad S.++ xs
  in parMap rdeepseq (S.sum . S.zipWith (*) (S.reverse hs)) (S.init $ S.tails ts)
~~~

## Conclusion

I'm new to Digital Signal Processing, so if you notice any glaring errors please feel free to correct me; I would appreciate it. If you are interested in this subject and would like to read a book to learn more, I wholeheartedly recommend [The Scientist and Engineer's Guide to Digital Signal Processing](http://www.dspguide.com). If you would like to learn more about Convolution, you can check the relevant chapters in that freely available book.

### Imperative Approach

You can also check out [this page](http://www.songho.ca/dsp/convolution/convolution.html) as well, which also covers multidimensional convolution with a concrete example of a [Gaussian filter](http://en.wikipedia.org/wiki/Gaussian_filter) applied to an image for the purposes of blurring it. This specific application of the Gaussian filter is known as the [Gaussian Blur](http://en.wikipedia.org/wiki/Gaussian_blur). The Gaussian Blur is pretty popular in realtime image rendering, such as in video games, because of a property it has which allows it be applied in two dimensions, e.g. in an image, as two independent one-dimensional operations. This makes it dramatically faster and more efficient, and is trivial to implement in modern GPU [Compute shaders](http://www.opengl.org/wiki/Compute_Shader) [^compute_shader]. Such shaders can then be used to implement effects such as motion blur in games [^motion_blur]. The page also provides imperative implementations of convolution in C++.

### Optimizations

Haskell is known for having many ways of doing any one thing, so if you come up with a better solution feel free to [gist it](https://gist.github.com) and post it in the comments.

Of course, this post concerns a _naive_ implementation of convolution. There are other more optimized implementations of convolution, such as FFT convolution which exploits the Fast Fourier Transform and the principle of duality---convolution in the time domain is equivalent to multiplication in the frequency domain---to perform convolution a lot faster in some cases.

*[GHC]: Glasgow Haskell Compiler
*[CSV]: Comma Separated Value
*[WHNF]: Weak Head Normal Form
*[GPU]: Graphics Processing Unit
*[FFT]: Fast Fourier Transform
*[AR]: Aspect Ratio

[^compute_shader]: As described in [3D Game Programming with DirectX 11](http://www.d3dcoder.net/d3d11.htm) by Frank D. Luna in Chapter 12, page 450, § 12.7
[^motion_blur]: Despite this optimization of Gaussian Blurring, many implementations optimize further. Blurring typically involves rendering the scene to a separate buffer (e.g. Render-to-Texture) at a scaled-down resolution. This speeds up the blurring operation as there are less pixels to operate on. Then the result is rendered to the actual screen. Since the point is to blur, the upscaling is usually hardly noticeable.

    Recently I purchased an old game on Steam which I had played circa 2003. This game was developed back when 1280x1024 was a popular resolution, that is 4:3 aspect ratio. I got to a part where the game displayed some sort of blur effect and noticed that the entire screen was completely blurred to the point where I couldn't make anything out. I presume this was not the intended effect. If I had to guess, I imagine they hard-coded a scaled down resolution---and thus aspect ratio as well---at which to render the scene for blurring, such that upscaling it to my current 1920x1080 resolution 16:9 AR looked horrible. I imagine newer games take into account aspect ratio and some other factor to scale down the current resolution from.
