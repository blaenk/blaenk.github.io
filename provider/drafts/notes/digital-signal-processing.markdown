---
title: Digital Signal Processing
published: October 7, 2013
excerpt: DSP
comments: off
---

# Terminology

Discrete signals have discrete time and discrete amplitude. A discrete-time signal is a one dimensional (for now) sequence of **complex** numbers. A signal is represented as:

$$ x[n] $$

The sequences run on both ends of the zero such that:

$$ x \colon \mathbb {Z \to C} $$

The $n$ is a dimension-less "time" with an integer index.

**Analysis** is a periodic measurement of a signal, whereas **synthesis** is a stream of generated samples.

The **unit step** is zero from $-\infty$ until zero, where it becomes one from zero to $+\infty$:

$$ x[n] = u[n] $$

The **exponential decay** sequence starts at zero with a value of one and exponentially decreases in the direction of $+\infty$:

$$ x[n] = |a|^n u[n], \quad \text {where}\ |a| < 1 $$

The **sinusoid** is:

$$ x[n] = sin(\omega_0 n + \theta) $$

There are four classes of signals:

1. **finite-length**
    * sequence notation: $x[n], \quad n = 0, 1, \dots, N - 1$
    * vector notation: $x = [x_0, x_1, \dots, x_{n - 1}]^T$
    * They are practical entities and are good for numerical packages.

<!-- -->

2. **infinite-length**
    * sequence notation: $x[n], \quad n \in \mathbb {Z}$
    * abstraction, good for theorems

<!-- -->

3. **periodic**
    * $N$-periodic sequence: $\tilde {x} [n] = \tilde {x} [n + kN]$
        * the tilde means there is periodicity
        * $N$ is the period
        * $k$ is how many periods are shifted
    * they convey the same information as finite-length signals of length $N$
    * they are the natural bridge between finite and infinite signals

<!-- -->

4. **finite-support**
    * notation:

        $$
        \bar {x} [n] = \begin{cases}
                          x[n] & \text {if } 0 \leq n \lt N - 1 \\
                          0    & \text {otherwise}
                        \end{cases} \quad n \in \mathbb {Z}
        $$

    * its a finite signal padded with zeros on either side to form an infinite signal
    * same information as a finite-length signal of length $N$
    * another bridge between finite and infinite length signals

Elementary operators:

* **scaling**
    * $y[n] = \alpha x[n]$

<!-- -->

* **sum**
    * $y[n] = x[n] + z[n]$

<!-- -->

* **product**
    * $y[n] = x[n] \cdot z[n]$

<!-- -->

* **shift** by $k$ (delay)
    * $y[n] = x[n - k]$
    * this can be done on a finite signal by creating a finite support signal or by periodic extension

Energy:

$$ E_x = \sum_{n = -\infty}^\infty |x[n]|^2 $$

Power (instantaneous energy):

$$ P_x = \lim_{N \to \infty} \frac {1} {2N + 1} \sum_{n = -N}^N |x[n]|^2 $$

For periodic signals:

$$ E_{\tilde x} = \infty $$

$$ P_{\tilde x} = \frac 1 N \sum_{n = 0}^{N - 1} |\tilde x [n]|^2 $$

# Complex Exponential

Oscillations consist of frequency, initial phase, and amplitude:

$$
\begin{align}
  \text {frequency:}     \quad & \omega\ \text {(units: radians)} \\
  \text {initial phase:} \quad & \phi\   \text {(units: radians)} \\
  \text {amplitude:}     \quad & A\      \text {(units: underlying measurement)}
\end{align}
$$

The trigonometric function of choice in DSP is the complex exponential:

$$
\begin{align}
x[n] &= Ae^{j(\omega n + \phi)} \\
     &= A[\underbrace {cos(\omega n + \phi)}_\text {real} + \underbrace {jsin(\omega n + \phi)}_\text {imaginary}]
\end{align}
$$

The complex exponential is used because it makes sense, since sines and cosines always go together. It also makes the math easier, since the trigonometry becomes algebra.

To illustrate the simplicity that complex exponentials afford, consider changing the phase of a pure cosine such as:

$$ cos(\omega n + \phi) = a\ cos(\omega n) + b\ sin(\omega n), \quad a = cos\ \phi,\ b = sin\ \phi $$

Each sinusoid is always a sum of sine and cosine, so to do this we have to remember complex trigonometric formulas and carry more terms in our equations.

On the other hand, with the equivalent complex exponential:

$$ Re\ \{ e^{j(\omega n + \phi)} \} = Re\ \{ e^{j \omega n}\ e^{j \phi} \} $$

This way, the sine and cosine "live" together, and a phase shift is a simple multiplication.

A complex exponential is:

$$ e^{j \alpha} = cos\ \alpha + j\ sin\ \alpha $$

A point $\mathbf z$ on a complex plane can be rotated by angle $\alpha$:

$$ \mathbf {z}' = \mathbf {z}\ e^{j \alpha} $$

The complex exponential generating machine:

$$ x[n] = e^{j \omega n}; \quad x[n + 1] = e^{j \omega}\ x[n] $$

If there is an initial phase $\theta$ then it is defined as:

$$ x[n] = e^{j \omega n + \phi}; \quad x[n + 1] = e^{j \omega}\ x[n], \quad x[0] = e^{j \phi} $$

A complex exponential is periodic if:

$$ e^{j \omega n} \Longleftrightarrow \omega = \frac M N 2 \pi, \quad \text {where}\ M, N \in \mathbb N $$

$$ e^{j \omega} = e^{j (\omega + 2 k \pi)} \quad \forall k \in \mathbb N $$

In discrete time, the $n$ has no physical dimension; it's just a counter. The periodicity how many samples before the pattern repeats.

In continuous signals, periodicity is how many seconds before a pattern repeats. The frequency is measured in Hz ($s^{-1}$).

Sometimes it's necessary to create a mapping from discrete time signals to continuous time signals. One example of when this is needed is a sound card on a computer. For this purpose, the time $T_s$ is the time in seconds between samples. A discrete periodicity of $M$ samples corresponds to a continuous periodicity of $MT_s$ seconds. With this information, it follows that the continuous frequency is:

$$ f = \frac 1 {MT_s} $$

# Resources

* École Polytechnique Fédérale de Lausanne [Digital Signal Processing](https://www.coursera.org/course/dsp)
* Rice [ELEC 301: Discrete Time Signals and Systems](https://www.edx.org/course/rice-university/elec301x/discrete-time-signals-and/1032)
