---
title: Machine Learning
published: September 30, 2013
excerpt: Machine Learning concepts
comments: off
toc: left
---

I've been wanting to learn about the subject of machine learning for a while now. I'm familiar with some basic concepts, as well as reinforcement learning. What follows are notes on my attempt to comprehend the subject. The primary learning resource I'm using is Cal Tech's CS 1156 on edX, with supplementary material from Stanford's CS 229 on Coursera.

* toc

# Learning Problem

The essence of machine learning:

1. pattern exists
2. cannot pin it down mathematically
3. have data on it to learn from

A movie recommender system might be modeled such that there are a set of factors of varying degrees of likability for the viewer, and varying degrees of presence in a given movie. These two sets of factors combine to produce a projected viewer rating for that given movie.

The machine learning aspect would take an actual user rating, and given two contributing-factor vectors for the movie and viewer full of random factors, it would modify each until a rating similar to the actual user rating is produced.

## Learning Components

* **Input**: $x = (x_1, x_2, \dots, x_n)$
    * feature vector
* **Output**: $y$
    * result given the input
* **Target Function**: $f\colon \cal {X} \to \cal {Y}$
    * The ideal function for determining the result, unknown, reason for learning
* **Data**: $(x_1, y_1), (x_2, y_2), \dots, (x_n, y_n)$
    * Historical records of actual inputs and their results
* **Hypothesis**: $g\colon \cal {X} \to \cal {Y}$
    * Result of learning, $g \approx f$
* **Learning Algorithm**: $\cal A$
    * The machine learning algorithm
* **Hypothesis Set**: $\cal H$
    * The set of candidate hypotheses where $g \in \cal H$

Together, $\cal A$ and $\cal H$ are known as the **learning model**.

## Perceptrons

### Model {#perceptron-model}

Given input $x = (x_1, x_2, \dots, x_n)$:

$$
\begin{align}
approve &\colon \sum_{i=1}^{d} w_i x_i > threshold \\
deny    &\colon \sum_{i=1}^{d} w_i x_i < threshold
\end{align}
$$

This can be expressed as a linear formula $h \in \cal H$:

$$ h(x) = sign \left( \left( \sum_{i=1}^{d} w_i x_i \right) - threshold \right) $$

The factors that are varied to determine the hypothesis function are the weights and threshold.

The threshold can be represented as a weight $w_0$ if an artificial constant $x_0$ is added to the feature vector where $x_0 = 1$. This simplifies the formula to:

$$ h(x) = sign \left( \sum_{i=0}^{d} w_i x_i \right) $$

This operation is the same as the dot product of the two vectors:

$$ h(x) = sign(w \bullet x) $$

### Learning Algorithm {#perceptron-learning-algorithm}

**Given the perceptron**:

$$ h(x) = sign(w \bullet x) $$

**Given the training set**:

$$x = (x_1, x_2, \dots, x_n)$$

**Pick a misclassified point**. A point is misclassified if the perceptron's output doesn't match the recorded data's output given an input vector:

$$ sign(w \bullet x_n) \not= y_n $$

**Update the weight vector**: The algorithm must correct for this by updating the weight vector.

$$ w' \gets w + y_n x_n $$

Visualizing $\vec w$ and $\vec x$, it's apparent that the perceptron is equivalent to the dot product, which is equivalent to $\cos \theta$ where $\theta$ is the angle between $\vec w$ and $\vec x$. Given this, $\vec w$ is updated depending on what the intended result $y_n$ is.

For example, if the perceptron modeled the result to be $-1$, then $\theta > 90^\circ$. However, if the intended result $y_n = +1$, then there is a mismatch, so the weight vector is "nudged" in the correct direction so that $\theta < 90^\circ$ by adding it to $x_n$.

## Types of Learning

* **Supervised learning**: when the input/output data is provided
* **Unsupervised learning**: only the input is provided; "unlabeled data"
* **Reinforcement learning**: input and _some_ output is provided

# Feasibility

It seems as if it isn't feasible to learn an unknown function because the function can assume any value outside of the data available to us.

To understand why it is indeed possible, consider a probabilistic example. Given a bin full of marbles that are either **red** or **green**:

$$
\begin{align}
&P \left( \text {picking a red marble} \right) = \mu \\
\\
&P \left( \text {picking a green marble} \right) = 1 - \mu \\
\end{align}
$$

If the value of $\mu$ is unknown, and we pick $N$ marbles independently, then:

$$ \text {frequency of red marbles in sample} = \nu $$

The question is: does $\nu$ say anything about $\mu$? It might appear that it **doesn't**. For example, if there are 100 marbles in the bin and only 10 of them are red, just because we happen to take out all 10 ($\nu  = 1$) doesn't mean that that sample is representative of the distribution in the bin ($\mu = 1$).

However, in a **big sample**, it's more probable that $\nu$ is close to $\mu$, that is, they are within $\epsilon$ of each other. This can be formally expressed as **Hoeffding's Inequality**:

$$
P \left( \left| \nu - \mu \right| \gt \epsilon \right) \leq 2e^{- 2\epsilon^2 N}
$$

$\nu$ is generally varied, and $\mu$ is a constant.

As is apparent from the inequality, if we choose a very small $\epsilon$ value, it has the effect of setting the right-hand side to near 1, thus rendering the effort pointless since we already knew that the probability would be $\leq 1$. **Therefore**, if we want a smaller $\epsilon$, we will have to increase the input size $N$ to compensate.

The appeal of Hoeffding's Inequality is that it is valid for any $N \in \mathbb {Z}^+$ and any $\epsilon > 0$. What we're trying to say with the inequality is that $\nu \approx \mu$ which means that $\mu \approx \nu$.

This relates to learning in the following way. In the bin, $\mu$ is unknown, but in learning the unknown is the target function $f \colon \mathcal {X} \to \mathcal {Y}$. Now think of the bin as the input space, where every marble is a point $x \in \mathcal {X}$ such as a credit application. As a result, the bin is really $\mathcal {X}$, where the marbles are of different colors such as green and gray, where:

* **green** represents that the hypothesis got it right, that is $h(x) = f(x)$
* **red** represents that the hypothesis got it wrong, that is $h(x) \not= f(x)$

However, in creating this analogy from the bin example to the learning model, the bin example has a probability component in picking a marble from the bin. This must be mapped to the learning model as well, in the form of introducing a probability distribution $P$, where $P$ is not restricted over $\mathcal {X}$, and $P$ doesn't have to be known. It is then assumed that the probability is used to generate the input data points.

The problem so far is that the hypothesis $h$ is fixed, and for a given $h$, $\nu$ generalizes to $\mu$, which ends up being a **verification** of $h$, not learning.

Instead, to make it a learning process, then there needs to be no guarantee that $\nu$ will be small, and we need to choose from multiple $h$'s. To generalize the bin model to more than one hypothesis, we can use multiple bins. Out of the many bins that were created, the hypothesis responsible for the bin with the smallest $\mu$ --- the fraction of red marbles in the bin --- is chosen.

## Notation {#notation-for-learning}

Marker: Lecture 2, 40:38

Both $\mu$ and $\nu$ depend on which hypothesis $h$:

$\nu$ is the error "**in sample**", which is denoted by $\def \insample {E_{\text {in}}} \insample(h)$

$\mu$ is the error "**out of sample**", which is denoted by $\def \outsample {E_{\text {out}}} \outsample(h)$

For clarification, if something performs well "out of sample" then it's likely that learning actually took place. This notation can be used to modify Hoeffding's Inequality:

$$
P \left( \left|\insample(h) - \outsample(h)\right| \gt \epsilon \right) \leq 2e^{- 2\epsilon^2 N}
$$

The problem now is that Hoeffding's Inequality doesn't apply to multiple bins. To account for multiple bins, the inequality can be modified to be:

$$
\begin{align*}
P \left( \left|\insample(g) - \outsample(g)\right| \gt \epsilon \right) &\leq
P \begin{aligned}[t]
              \left( \vphantom {\epsilon} \right. &\hphantom {\text {or}}\
                \left|\insample(h_1) - \outsample(h_1)\right| \gt \epsilon \\
              &\text {or}\ \left|\insample(h_2) - \outsample(h_2)\right| \gt \epsilon \\
              &\dots \\
              &\left. \vphantom{\insample(h_1)} \text {or}\ \left|\insample(h_M) - \outsample(h_M)\right| \gt \epsilon \right)
            \end{aligned} \\
&\leq \sum_{m=1}^M P \left( \left| \insample(h_m) - \outsample(h_m) \right| > \epsilon \right) \\
&\leq \sum_{m=1}^M 2e^{- 2\epsilon^2 N} \\
P \left( \left|\insample(g) - \outsample(g)\right| \gt \epsilon \right) &\leq 2Me^{- 2\epsilon^2 N}
\end{align*}
$$

# Linear Model

## Input Representation

If we want to develop a system that can detect hand-written numbers, the input can be different examples of hand-written digits. However, if an example digit is a 16x16 bitmap, then that corresponds to an array of 256 real numbers, which becomes 257 when $x_0$ is added for the threshold. This means the problem is operating in 257 dimensions.

Instead, the input can be represented in just three dimensions: $x_0$, intensity, and symmetry. The intensity corresponds to how many black pixels exist in the example, and the symmetry is a measure of how symmetric the digit is along the x and y axes. See slide 5 to see a plot of the data using these features.

## Pocket Algorithm

The PLA would never converge on non-linearly separable data, so it's common practice to forcefully terminate it after a certain number of iterations. However, this has the consequence that the hypothesis function ends up being whatever the result of the last iteration was. This is a problem because it could be that a better hypothesis function with lower in-sample error $\insample$ was discovered in a previous iteration.

The Pocket algorithm is a simple modification to the PLA which simply keeps track of the hypothesis function with the least in-sample error $\insample$. When this is combined with forceful termination after a certain number of iterations, PLA becomes usable with non-linearly separable data.

## Linear Regression

The word **regression** simply means real-valued output. For example, in the scenario of credit approval, a classification problem would consist of determining whether or not to grant a credit line to an applicant. However, a regression problem would be determining the dollar amount for a particular credit line.

The linear regression output is defined as:

$$ h(x)  = \sum_{i = 0}^d w_i x_i = \mathbf {w^{\mathrm {T}}x} = \mathbf {w \bullet x} $$

For example, the input may look something like this:

$$ (x_1, y_1), (x_2, y_2), \dots, (x_n, y_n) $$

Here $y_n \in \mathbb {R}$ is the credit line for customer $x_n$.

The measure of how well $h$ approximates $f$ is referred to as the error. With linear regression, the standard error function used is the **squared error**, defined as:

$$ \text {squared error} = (h(x) - f(x))^2 $$

This means that the in-sample error $\insample$ is defined as:

$$ \insample(h) = \frac {1} {N} \sum_{n = 1}^N (h(\mathbf {x}_n) - y_n)^2 $$

This can be written in terms of $\vec {w}$:

$$
\begin{align}
\insample(\mathbf w) &= \frac {1} {N} \sum_{n = 1}^N (\mathbf {w}^{\mathrm {T}} \mathbf {x}_n - y_n)^2 \\
                  &= \frac {1} {N} \sum_{n = 1}^N (\mathbf {w} \bullet \mathbf {x}_n - y_n)^2
\end{align}
$$

This can be written in vector form as:

$$
\begin{align}
&\insample(w) = \frac {1} {N} \| \mathrm {X} \mathbf {w} - \mathbf {y} \|^2 \\[10pt]
&\text {where}\ \mathrm {X} = \begin{bmatrix}
                              —\ x_1^{\mathrm {T}}\ — \\
                              —\ x_2^{\mathrm {T}}\ — \\
                              \vdots \\
                              —\ x_n^{\mathrm {T}}\ —
                            \end{bmatrix},\ 
              \mathrm {y} = \begin{bmatrix}
                              y_1 \\
                              y_2 \\
                              \vdots \\
                              y_n
                            \end{bmatrix}
\end{align}
$$

Since the goal is to minimize the in-sample error $\insample$, and $\mathrm {X}$ and $\mathrm {y}$ are constant since they were provided as input data, $\insample$ can be minimized by varying $\vec {w}$:

$$
\begin{align}
&\phantom {\nabla} \insample(w) = \frac {1} {N} \| \mathrm {X} \mathbf {w} - \mathbf {y} \|^2 \\
&\nabla \insample(w) = \frac {2} {N} \mathrm {X}^{\mathrm {T}} \left( \mathrm {X} \mathbf {w} - \mathbf {y} \right)^2 = 0 \\
\end{align}
$$

Knowing this, an equation for the weight vector can be found by distributing the $\mathrm {X}^{\mathrm {T}}$ factor. The $\mathrm {w}$ factor can then be isolated by multiplying both sides by the inverse of $\mathrm {X}^{\mathrm {T}} \mathrm {X}$. The resulting factor $X^\dagger$ on the right side is known as the **pseudo-inverse** of $\mathrm {X}$.

$$
\begin{align}
\mathrm {X}^{\mathrm {T}} \mathrm {Xw} &= \mathrm {X}^{\mathrm {T}} \mathrm {y} \\
\text {if}\ \mathrm {X}^\dagger &= \left( \mathrm {X}^{\mathrm {T}} \mathrm {X} \right)^{-1} \mathrm {X}^{\mathrm {T}} \\
\text {then}\ \mathrm {w} &= \mathrm {X}^\dagger \mathrm {y}
\end{align}
$$

The dimension of $\mathrm {X}^\mathrm {T}$ is $(d + 1) \times N$, so the dimension of $\mathrm X$ is $N \times (d + 1)$. This means that even if $N$ is some large number, their product results in a small square matrix of dimensions $(d + 1) \times (d + 1)$. This means that the dimensions of $\mathrm {X}^\dagger$ will be $(d + 1) \times N$.

### Algorithm

The algorithm for linear regression is therefore:

1. construct input data matrix $\mathrm {X}$ and target vector $\mathrm {y}$ from the data set $(x_1, y_1), (x_2, y_2), \dots, (x_n, y_n)$

$$
\mathrm {X} = \begin{bmatrix}
                —\ x_1^{\mathrm {T}}\ — \\
                —\ x_2^{\mathrm {T}}\ — \\
                \vdots \\
                —\ x_n^{\mathrm {T}}\ —
              \end{bmatrix},\ 
\mathrm {y} = \begin{bmatrix}
                y_1 \\
                y_2 \\
                \vdots \\
                y_n
              \end{bmatrix}
$$

2. compute the pseudo inverse $\mathrm {X}^\dagger = \left( \mathrm {X}^{\mathrm {T}} \mathrm {X} \right)^{-1} \mathrm {X}^{\mathrm {T}}$
3. return the weight vector $\mathrm {w} = \mathrm {X}^\dagger \mathrm {y}$

### Classification {#lienar-regression-for-classification}

Linear regression learns a real-valued function $y = f(x) \in \mathbb {R}$. However, binary-valued functions are also real-valued: $\pm 1 \in \mathbb {R}$. Therefore, we can use linear regression to find $\mathrm w$ where:

$$ \mathrm {w}^{\mathrm {T}} \mathrm {x}_n \approx y_n = \pm 1 $$

This way, $\text {sign} (\mathrm {w}^{\mathrm {T}} \mathrm {x}_n)$ is likely to agree with $y_n = \pm 1$. This provides good initial weights for classification.

## Non-Linear Transformations

Not all data is linearly separable. In fact, certain data features aren't linear. For example, a credit line is affected by "years in residence," but it doesn't affect it in a linear way where someone with 10 years in residence will get much more benefit than someone in 5. Instead, the feature can be defined as affecting it in a non-linear manner given the following conditions:

$$ [\![ x_i < 1 ]\!] \text { and } [\![ x_i > 5 ]\!] $$

Linear regression and classification work because they are linear in the weights. For this reason, data can be transformed non-linearly.

For example, if a given data set has positive data points around the center of a region, a transformation could be applied to each point which simply measures the distance from the center of the region to a given point:

$$ (x_1, x_2) \xrightarrow{\Phi} (x_1^2, x_2^2) $$

This newly transformed data set --- which is now linearly separable --- is used as the new data set.

To recap, non-linear transformations can be used to transform data such that it becomes linearly separable:

1. given original data
    * $\mathrm {x}_n \in \mathcal X$
2. transform the data
    * $\mathrm {z}_n = \Phi (\mathrm {x}_n) \in \mathcal Z$
3. separate data in the $\mathcal Z$-space
    * $\tilde {g} (\mathrm {z}) = \text {sign} (\tilde {w}^{\mathrm {T}} \mathrm {z})$
4. classify in $\mathcal X$-space
    * $g(\mathrm {x}) = \tilde {g} (\Phi (\mathrm {x})) = \text {sign} (\tilde {w}^{\mathrm {T}} \Phi (\mathrm {x}))$

A $\Phi$-transform transforms input data into the $\mathcal Z$-coordinate space:

$$
\begin{align}
\mathrm {x} = (x_0, x_1, \dots, x_d)\ &\xrightarrow{\Phi}\ \mathrm {z} = (z_0, z_1, \dots \dots, z_{\tilde d}) \\
\mathrm { x_1, x_2, \dots, x_n}\      &\xrightarrow{\Phi}\ \mathrm {z_1, z_2, \dots, z_n } \\
\mathrm { y_1, y_2, \dots, y_n}\      &\xrightarrow{\Phi}\ \mathrm {y_1, y_2, \dots, y_n } \\
\text {no weights in}\ \mathcal {X}\  &\phantom {\xrightarrow{\Phi}\ } \mathrm {\tilde {w}} = (w_0, w_1, \dots \dots, w_{\tilde {d}}) \\
g(\mathrm {x}) &= \text {sign} (\mathrm {\tilde {w}^T z}) = \text {sign} (\mathrm {\tilde {w}^T \Phi (x)})
\end{align}
$$

# Error and Noise

Error measures explain what it means for $h \approx f$. It is defined as $E(h, f)$. It almost always has a pointwise definition $e(h(\mathbf x), f(\mathbf x))$ which takes as input the value of the same point for both the hypothesis and target functions. An example of this is the squared error and binary error:

$$
\def \xpoint {\mathbf x}
\text {squared error}\colon \quad e(h(\xpoint), f(\xpoint)) = (h(\xpoint) - f(\xpoint))^2
$$

$$ \text {binary error}\colon \quad e(h(\xpoint), f(\xpoint)) = [\![ h(\xpoint) \not= f(\xpoint)]\!] $$

To go from a pointwise error measure to overall, we take the average of pointwise errors.

$$
\begin{align}
\insample(h)  &= \frac 1 N \sum_{n = 1}^N e(h(\xpoint_n), f(\xpoint_n)) \\ \\
\outsample(h) &= \mathbb {E}_{\xpoint} \left[ e(h(\xpoint), f(\xpoint)) \right] \\
              &\phantom {=}\ \text {where } \mathbb E \text { is the expected value}
\end{align}
$$

It can therefore be said that $g \approx f$ if it is tested with the values from the same distribution that it was trained on and yields an acceptable pointwise error measure.

## Choosing the Error Measure

An example in which the choice of error measure is important is fingerprint verification, which returns $+1$ if it thinks it's you, and $-1$ if it thinks it's an intruder.

There are two types of error:

* **false accept**: something that shouldn't have been accepted was accepted (_false positive_)
* **false reject**: something that shouldn't have been rejected was rejected (_false negative_)

The question is, how much should each type of error be penalized?

<table style="background-color: transparent !important; border-collapse: collapse; border: none; empty-cells: hide;">
  <tr>
    <td colspan="2" style="background-color: transparent;"></td>
    <td colspan="2" style="text-align: center;">$f$</td>
  </tr>
  <tr style="background-color: transparent;">
    <td colspan="2"></td>
    <td style="text-align: center; border-bottom: 2px solid black;">$+1$</td>
    <td style="text-align: center; border-bottom: 2px solid black;">$-1$</td>
  </tr>
  <tr>
    <td rowspan="2">$h$</td>
    <td style="border-right: 2px solid black;">$+1$</td>
    <td style="text-align: center">no error</td>
    <td style="text-align: center">**false accept**</td>
  </tr>
  <tr style="background-color: transparent;">
    <td style="border-right: 2px solid black;">$-1$</td>
    <td style="text-align: center">**false reject**</td>
    <td style="text-align: center">no error</td>
  </tr>
</table>

The choice of error measure is application specific. For example, in the scenario that fingerprint recognition is used to determine which customers in a particular supermarket can get discounts, the cost of false rejections or accepts are:

* **false reject**: costly; customer gets annoyed, possibly switches to competitor
* **false accept**: minor; give away discount to someone that didn't qualify for it, but the intruder has left their fingerprint behind (to later train the system to fix that mistake?)

Given this particular application domain, we may want to penalize a candidate hypothesis function if it has a high rate of false rejects/negatives, in this case by a factor of $10$. False accepts/positives on the other hand are only penalized by a factor of $1$.

<table style="background-color: transparent !important; border-collapse: collapse; border: none; empty-cells: hide;">
  <tr>
    <td colspan="2" style="background-color: transparent;"></td>
    <td colspan="2" style="text-align: center;">$f$</td>
  </tr>
  <tr style="background-color: transparent;">
    <td colspan="2"></td>
    <td style="text-align: center; border-bottom: 2px solid black;">$+1$</td>
    <td style="text-align: center; border-bottom: 2px solid black;">$-1$</td>
  </tr>
  <tr>
    <td rowspan="2">$h$</td>
    <td style="border-right: 2px solid black;">$+1$</td>
    <td style="text-align: center">$0$</td>
    <td style="text-align: center">$1$</td>
  </tr>
  <tr style="background-color: transparent;">
    <td style="border-right: 2px solid black;">$-1$</td>
    <td style="text-align: center">$10$</td>
    <td style="text-align: center">$0$</td>
  </tr>
</table>

In an alternative scenario, the fingerprint identification system is used by the CIA for security.

* **false accept**: disaster
* **false reject**: can be tolerated, you're an employee

<table style="background-color: transparent !important; border-collapse: collapse; border: none; empty-cells: hide;">
  <tr>
    <td colspan="2" style="background-color: transparent;"></td>
    <td colspan="2" style="text-align: center;">$f$</td>
  </tr>
  <tr style="background-color: transparent;">
    <td colspan="2"></td>
    <td style="text-align: center; border-bottom: 2px solid black;">$+1$</td>
    <td style="text-align: center; border-bottom: 2px solid black;">$-1$</td>
  </tr>
  <tr>
    <td rowspan="2">$h$</td>
    <td style="border-right: 2px solid black;">$+1$</td>
    <td style="text-align: center">$0$</td>
    <td style="text-align: center">$1000$</td>
  </tr>
  <tr style="background-color: transparent;">
    <td style="border-right: 2px solid black;">$-1$</td>
    <td style="text-align: center">$1$</td>
    <td style="text-align: center">$0$</td>
  </tr>
</table>

This reinforces the fact that the error measure is application specific, and thus specified by the user. A question that should be asked to determine the error measure is how much does it cost the application to have a false positive or negative.

However, it isn't always possible to determine a correct error measure. There are alternatives to this:

* **plausible measures**: approximate e.g. squared error $\equiv$ Gaussian noise
* **friendly measures**: closed-form solution e.g. linear regression, convex optimization

In the learning diagram, the error measure goes into the learning algorithm so that it knows what value to minimize among the hypothesis set, and to determine the final hypothesis, to know if the hypothesis candidate approximates the target function.

## Noise

The "target function" is not always a _function_. In the mathematical sense, a function must return a unique value for any given input. However, going back to the credit-card approval system, it is entirely possible that two given applicants had the exact same feature set (e.g. age, annual salary, years in residence) but ended up having different outputs: one ended up having good credit and the other bad. This is possible because the feature set doesn't capture every possible factor.

This is mitigated by having a target distribution. Instead of having a function $f$ that yields an exact answer $y$ for any given $\xpoint$:

$$ y = f(\xpoint) $$

We will use a target _distribution_, which still encodes $y$'s dependence on $\xpoint$, but in a probabilistic manner:

$$ y \sim P(y \mid \xpoint) $$

Now $(\xpoint, y)$ will be generated by the joint distribution:

$$ P(\xpoint) P(y \mid \xpoint) $$

This means that we will get a noisy target, which is equivalent to a deterministic target $f(\xpoint) = \mathbb {E} (y \mid \xpoint)$ **plus** noise $y - f(\xpoint)$.

In fact, a deterministic target is a special case of a noisy target where:

$$
P(y \mid \xpoint) = \begin{cases}
                      1 & y = f(\xpoint) \\ \\
                      0 & y \not= f(\xpoint)
                    \end{cases}
$$

What this means is that in the learning diagram, we replace the "unknown target function $f\colon \mathcal {X \to Y}$" box with "unknown target distribution $P(y \mid \xpoint)$ target function: $f\colon \mathcal {X \to Y}$ plus noise".

It's important to notice the distinction between $P(y \mid \xpoint)$ and $P(\xpoint)$. $P(\xpoint)$ is the probability that was introduced to satisfy Hoeffding's Inequality, derived from the unknown input distribution. $P(y \mid \xpoint)$ is the probability that was introduced to reflect the fact that real-world target functions are noisy. Both probabilities are derived from _unknown_ probability distributions, they're unknown because they don't need to be known.

Both probabilities convey the probabilistic aspects of $\xpoint$ and $y$. The target distribution $P(y \mid \xpoint)$ is what we are trying to learn. The input distribution $P(\xpoint)$ is simply quantifying the relative example of $\xpoint$; we're not trying to learn it.

The input distribution is the distribution of the feature set in the general population. For example, if the feature set simply consists of salary, then the input distribution says how many people make $70k, $100k, etc.

For this reason, the probabilities can be merged as:

$$ P(\xpoint) P(y \mid \xpoint) \equiv P(\xpoint \cap y) $$ 

This merging mixes two concepts. $P(\xpoint \cap y)$ is not a target distribution for supervised learning. The target distribution that we are actually trying to learn is $P(y \mid \xpoint)$.

# Training vs. Testing {#training-vs-testing}

Learning is feasible because it is likely that:

$$ \outsample(g) \approx \insample(g) $$

However, is this _really_ learning?

We've defined learning as being $g \approx f$, which effectively means that the out-of-sample error for $g$ should be close to zero:

$$ \outsample(g) \approx 0 $$

The above means that "we learned well," and it's achieved through:

$$ \outsample(g) \approx \insample(g) \quad \text {and} \quad \insample(g) \approx 0 $$

Learning thus reduces to two questions:

1. Can we make sure that $\outsample(g)$ is close enough to $\insample(g)$?
2. Can we make $\insample(g)$ small enough?

## Difference {#difference-between-testing-and-training}

There is a difference between training and testing. It's analogous to when one takes a practice exam compared to when one takes a final exam.

In the testing scenario, $\insample$ is how well one did in the final exam, and $\outsample$ is how well one understands the material in general. In this case, the probability that $\insample$ tracks $\outsample$ --- that is, that doing well on the final exam means that they understood the material --- increases as the number of questions in the final exam $N$ increases:

$$ P(|\insample - \outsample| \gt \epsilon) \leq 2\phantom {M} e^{-2 \epsilon^2 N} $$

In the training scenario, $\insample$ is how well one did on the practice problems. The more practice problems $M$ that were done, the more of a penalty that is incurred because the practice problems are pretty much memorized, and so the topics aren't learned in a more general manner:

$$ P(|\insample - \outsample| \gt \epsilon) \leq 2Me^{-2 \epsilon^2 N} $$

## Overlapping Events

The goal is to replace the $M$ term with something more manageable since otherwise the above "guarantee" isn't much of a guarantee, since something as simple as the perceptron learning algorithm leads to $M = \infty$.

The bad event $\mathcal B_m$ is:

$$ |\insample - \outsample| \gt \epsilon $$

Bad events can be visualized as a Venn Diagram with varying degrees of overlap. However, the union bound is made regardless of the correlations between the bad events in order to be more general:

$$ P(\mathcal B_1\ \mathbf {or}\ \mathcal B_2\ \mathbf {or}\ \dots\ \mathbf {or}\ \mathcal B_M) $$

As a result, it treated as if the bad events were disjoint, in which case the total area of the sum of all bad events is considered:

$$ \leq \underbrace {P(\mathcal B_1) + P(\mathcal B_2) + \dots + P(\mathcal B_M)}_{\text {no overlaps: M terms}} $$

This is a poor bound because in reality the bad events could have significant amounts of overlapping. This is why $M = \infty$ in many learning algorithms when using this very loose bound.

Given the following perceptron:

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/training-versus-testing/e-in.png">
  <img src="/images/machine-learning/training-versus-testing/e-out.png">
</div>

When the perceptron (the hypothesis) is changed, there is a huge amount of overlap:

<img src="/images/machine-learning/training-versus-testing/perceptron-overlap.png" class="center">

Only the area in yellow is what has changed between the two perceptrons; everything else overlaps.

$$
\begin{align}
&\Delta \outsample : \text {change in +1 and -1 areas} \\
&\Delta \rlap {\insample} \phantom {\outsample} : \text {change in labels of data points}
\end{align}
$$

Given this observation --- that different hypotheses have significant amounts of overlapping with others --- we would like to make the statement that one hypothesis exceeds epsilon often when another hypothesis exceeds epsilon.

$$ |\insample(h_1) - \outsample(h_1)| \approx |\insample(h_2) - \outsample(h_2)| $$

## Dichotomies

When counting the number of hypotheses, the entire input space is taken into consideration. In the case of a perceptron, each perceptron differs from another if they differ in at least one input point, and since the input is continuous, there are an infinite number of different perceptrons.

Instead of counting the number of hypotheses in the entire input space, we are going to restrict the count only to the sample: a finite set of input points. Then, simply count the number of the possible **dichotomies**. A dichotomy is like a mini-hypothesis, it's a configuration of labels on the sample's input points.

A hypothesis is a function that maps an input from the entire _input space_ to a result:

$$ h\colon \mathcal X \to \{-1, +1\} $$

The number of hypotheses $|\mathcal H|$ can be infinite.

A dichotomy is a hypothesis that maps from an input from the _sample size_ to a result:

$$ h\colon \{\mathbf {x_1, x_2, \dots, x_N}\} \to \{-1, +1\} $$

The number of dichotomies $|\mathcal H(\mathbf {x_1, x_2, \dots, x_N})|$ is at most $2^N$, where $N$ is the sample size. This makes it a candidate for replacing $M$.

## Growth Function

The **growth function** counts the _most_ dichotomies on any $N$ points.

$$
\def \growthfunc {m_{\mathcal H}}
\growthfunc(N) = \max_{\mathbf {x_1, \dots, x_N} \in \mathcal X} |\mathcal H(\mathbf {x_1, \dots, x_N})|
$$

This translates to choosing any $N$ points and laying them out in any fashion in the input space. Determining $m$ is equivalent to looking for such a layout of the $N$ points that yields the _most_ dichotomies.

The growth function satisfies:

$$ \growthfunc(N) \leq 2^N $$

This can be applied to the perceptron. For example, when $N = 4$, we can lay out the points so that they are easily separated. However, given a layout, we must then consider all possible configurations of labels on the points, one of which is the following:

<img src="/images/machine-learning/training-versus-testing/breaking-point.png" class="center">

This is where the perceptron breaks down because it _cannot_ separate that configuration, and so $\growthfunc(4) = 14$ because two configurations --- this one and the one in which the left/right points are blue and top/bottom are red --- cannot be represented.

For this reason, we have to expect that that for perceptrons, $m$ can't be the maximum possible because it would imply that perceptrons are as strong as can possibly be.

We will try to come up with a hypothesis function that can easily determine the value of $m$.

### Positive Rays

Positive rays are defined on $\mathbb R$ and define a point $a$ such that:

$$
h(x) = \begin{cases}
         +1 & \text {if } x > a \\ \\
         -1 & \text {otherwise}
       \end{cases}
$$

<img src="/images/machine-learning/training-versus-testing/positive-rays.png" class="center">

$\mathcal H$ is the set from the reals to a label $-1$ or $+1$, so $h\colon \mathbb R \to \{-1, +1\}$. Put more simply:

$$ h(x) = \mathrm {sign} (x - a) $$

The dichotomy is determined by noticing between which two points in the input set $a$ falls into, these are the line segments between the input points on the line, of which there are $N + 1$, so:

$$ \growthfunc(N) = N + 1 $$

### Positive Intervals

Instead of a ray, we define an interval $[l, r]$ where any point that falls within it is defined as $+1$ and everything outside it is $-1$:

$$
h(x) = \begin{cases}
         +1 & \text {if } l \leq x \leq r \\ \\
         -1 & \text {otherwise}
       \end{cases}
$$

<img src="/images/machine-learning/training-versus-testing/positive-intervals.png" class="center">

The way to vary the different dichotomies is by choosing two line segments --- of which there are again $N + 1$ --- at which to place the interval ends. For this reason, the growth function is:

$$ \growthfunc(N) = {N + 1 \choose 2} $$

However, this doesn't count the configuration in which the interval ends lie on the same line segment, making all points $-1$, so we add that:

$$ \growthfunc(N) = {N + 1 \choose 2} + 1 = \frac 1 2 N^2 + \frac 1 2 N + 1 $$

### Convex Sets

In this case, we take a plane and not a line so that $\mathcal H$ is the set of $h\colon \mathbb R^2 \to \{-1, +1\}$. In this model, if a point is found within a convex region then it results in $+1$, and $-1$ otherwise.

$$
h(x) = \begin{cases}
         +1 & \text {if inside region} \\ \\
         -1 & \text {otherwise}
       \end{cases}
$$

A convex region is a region where for any two points picked within a region, the entirety of the line segment connecting them lies within the region. For example, the left image is a convex region, but the one on the right isn't because the line segment connecting the chosen two points doesn't fall entirely within the region:

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/training-versus-testing/convex-region.png">
  <img src="/images/machine-learning/training-versus-testing/non-convex-region.png">
</div>

Convex regions can therefore be used to model dichotomies. The best layout for the points in the input set is to place them on the perimeter of a circle, in which case any configuration of the labels can be satisfied with a convex region as shown below:

<img src="/images/machine-learning/training-versus-testing/convex-region-dichotomy.png" class="center">

When a hypothesis set is able to reach every possible dichotomy, it is said that the hypothesis set shattered the points. This means that the hypothesis set is very good at fitting the data, but this is a trade off with generalization.

## Break Point

What happens when $\growthfunc(N)$ replaces $M$? If the growth function is polynomial then everything will be fine. So we need to prove that $\growthfunc(N)$ is polynomial.

If no data set of size $k$ can be shattered by $\mathcal H$, then $k$ is a **break point** for $\mathcal H$. By extension, this means that a bigger data set cannot be shattered either. In other words, given a hypothesis set, a break point is the point at which we fail to achieve all possible dichotomies.

We [already saw](#growth-function) that for perceptrons, $k = 4$.

For positive rays where $\growthfunc(N) = N + 1$, the break point is $k = 2$ because $2 + 1 = 3$ and this is not the same as $2^2 = 4$. This is evidenced by the fact that the below configuration cannot be represented by a positive ray:

<img src="/images/machine-learning/training-versus-testing/positive-ray-break-point.png" class="center">

Similarly, for positive intervals where $\growthfunc(N) = \frac 1 2 N^2 + \frac 1 2 N + 1$, the break point is $k = 3$ because $\growthfunc(3) = 7 \not= 2^3 = 8$. The following configuration is an example configuration that cannot be represented by a positive interval:

<img src="/images/machine-learning/training-versus-testing/positive-interval-break-point.png" class="center">

Convex sets don't have a break point, so $k = \infty$. Having no break point means that $\growthfunc(N) = 2^N$.

On the other hand, if there _is_ a break point, then $\growthfunc(n)$ is guaranteed to be polynomial (growth) on $N$. **This means that it is possible to learn with the given hypothesis set.**

For example, consider a puzzle where there are three binary points $\mathbf x_1$, $\mathbf x_2$, and $\mathbf x_3$. The constraint is that the breaking point is $k = 2$, this effectively means that no combination of 2 points can be shattered, which in this context means that no two points can hold every possible combination: 00, 01, 10, and 11. A possible solution to the puzzle is:

 $\mathbf x_1$   $\mathbf x_2$   $\mathbf x_3$ 
--------------- --------------- ---------------
&#9675;         &#9675;         &#9675;
&#9675;         &#9675;         &#9679;
&#9675;         &#9679;         &#9675;
&#9679;         &#9675;         &#9675;

# Theory of Generalization

We want to prove that $\growthfunc(N)$ is indeed polynomial, and also that it is therefore possible to use it to replace $M$.

First, to show that $\growthfunc(N)$ is polynomial, we will show that it is bound by a polynomial:

$$ \growthfunc(N) \leq \dots \leq \dots \leq \text {a polynomial} $$

We will use the quantity $B(N, k)$ which is the maximum number of dichotomies on $N$ points with break point $k$. In other words, the maximum number of rows we can get on $N$ points such that no $k$ columns have all possible patterns

## Conceptualization

To understand how $B(N, k)$ is defined, we should first determine each combination that is possible given a break point $k$ in a table with a column for every feature $\mathbf x_i$. We will then group these rows into two major groups.

$S_1$ is the group of $\alpha$ rows that are entirely unique on $\mathbf x_1$ to $\mathbf x_N$. If the last column $\mathbf x_N$ were removed, there would be no other row in the set with the same $\mathbf x_1$ to $\mathbf x_{N - 1}$.

Since there are $\alpha$ rows in $S_1$, we will say that $B(N, k)$ is defined in terms of it as:

$$ B(N, k) = \alpha\ +\ ? $$

$S_2$ is a group comprised of two subgroups: $S_2^+$ and $S_2^-$. The rows that belong here are those where, if the last row $\mathbf x_N$ were removed, there would be exactly one other row with the same $\mathbf x_1$ to $\mathbf x_{N - 1}$. If the last column is $-1$ then it goes into $S_2^-$ and vice versa. There are $\beta$ rows in $S_2^+$ and $\beta$ rows in $S_2^-$ for a total of $2\beta$ rows in $S_2$.

We can now add the extra term to $B(N, k)$:

$$ B(N, k) = \alpha + 2\beta $$

## Estimating $\alpha$ and $\beta$

We will now look at estimating $\alpha$ and $\beta$ by relating it to smaller numbers of $N$ and $k$ in order to achieve a recursive definition.

If we only focus on the columns $\mathbf x_1$ to $\mathbf x_{N - 1}$, then we can observe that the only unique rows are those in $S_1$ and those in _one_ of the subgroups of $S_2$. This is relevant to $B(N, k)$ because it only concerns those rows that are _different_ such that a condition occurs, the condition being:

$$ \alpha + \beta \leq B(N - 1, k) $$

We can make this relation because we are now considering $N - 1$ columns instead of all $N$, and we are still using a break point of $k$. We say "$\leq$" and not "$=$" as before because before, we made it equal by construction. However, we arrived at the mini-matrix of $\alpha + \beta$ rows in a manner where we can't be positive that it maximizes the number of rows. However, we can be sure that it's at _most_ $B(N, k)$ because that is the maximum number that is possible.

## Estimating $\beta$

We now want to estimate $\beta$ by itself. To do this, we'll focus only on the $S_2$ group, comprised of $S_2^+ \cap S_2^-$ rows. Specifically, we'll focus on a single mini-matrix of $\beta$ rows, such as $S_2^+$, _without_ the last column $\mathbf x_n$. We will make the assertion that the break point for this mini-matrix is $k - 1$.

We can say this is true because, if it weren't, then it would mean that it would indeed be _possible_ to shatter $k - 1$ points within that mini-matrix. This would mean that we could attain $2^{k - 1}$ different rows within the mini-matrix; one for each different combination.

However, we must remember that we're only focusing on one sub-matrix within the overall matrix. Further, we know that by definition there is another sub-matrix $S_2^-$ with the same amount of rows.

Therefore, if we then were to add back in the $\mathbf x_n$ column and also consider the rows from $S_2^-$, we would have $2^k$ rows with every possible combination (i.e. shattering $k$ points), which would **contradict** the fact that $k$ is the break point for the larger matrix. Therefore, we can be certain that the break point for the mini-matrix is at most $k - 1$. It could be lower, but since we are trying to achieve an upper bound, this is irrelevant.

With this in mind, we can define a relation for the maximum number of dichotomies within this mini-matrix which is composed of $N - 1$ points and for which we have proved that $k - 1$ is the break point:

$$ \beta \leq B(N - 1, k - 1) $$

## Bound Relation

We want to arrive at an upper bound without having to know $\alpha$ or $\beta$.

If we combine the two derived relations into a system of inequalities:

$$
\begin{align}
\alpha + \beta &\leq B(N - 1, k) \\
\beta &\leq B(N - 1, k - 1)
\end{align}
$$

Adding them together yields:

$$ \alpha + 2\beta \leq B(N - 1, k) + B(N - 1, k - 1) $$

And since we know that:

$$ B(N, k) = \alpha + 2\beta $$

We can say that:

$$ B(N, k) \leq B(N - 1, k) + B(N - 1, k - 1) $$

## Computing the Bound

To define this recursive bound, we will have to compute the values of the bound for smaller values of $N$ and $k$.

### Numerically {#numerical-bound-computation}

We can start doing this by filling out a table with the left hand side being $N$ and the top being $k$, where any element in the table specifies the maximum number of dichotomies for that combination $B(N, k)$. We begin doing this by filling out the bounds for when $k = 1$ and when $N = 1$.

When $k = 1$, we will only ever have one dichotomy because we're using a binary function, since having more than one row would mean that one of the columns is fully exhausted.

When $N = 1$ and $k \geq 2$, given that we only have one point, the maximum number of dichotomies we can represent with a binary function is two.

With these bounds, we can then define every other point in the table:

<img src="/images/machine-learning/theory-of-generalization/dichotomy-table.png" class="center">

Remembering that:

$$ B(N, k) \leq B(N - 1, k) + B(N - 1, k - 1) $$

We can take an example such as $B(3, 3)$. To get this value, we add $B(N - 1, k - 1) = B(2, 2) = 3$, with $B(N - 1, k) = B(2, 3) = 4$, resulting in $B(3, 3) = 7$.

### Analytically {#analytical-bound-computation}

A more analytic solution for computing the bound is given by the theorem:

$$
\def \upperbound {\sum_{i = 0}^{k - 1} {N \choose i}} 
B(N, k) \leq \upperbound
$$

We can easily verify that this is true for the boundary conditions that we derived in the table above.

For every other condition we must consider the induction step. Given the above theorem, it is implied that:

$$ \upperbound = \sum_{i = 0}^{k - 1} {N - 1 \choose i} + \sum_{i = 0}^{k - 2} {N - 1 \choose i} $$

So it's recursively implied that:

$$
\begin{align}
B(N - 1, k) &= \sum_{i = 0}^{k - 1} {N - 1 \choose i} \\
B(N - 1, k - 1) &= \sum_{i = 0}^{k - 2} {N - 1 \choose i}
\end{align}
$$

We can accomplish this by reducing the right hand side to resemble the left hand side:

$$
\begin{align}
\sum_{i = 0}^{k - 1} {N \choose i} &= \sum_{i = 0}^{k - 1} {N - 1 \choose i} + \sum_{i = 0}^{k - 2} {N - 1 \choose i} \\
&= 1 + \sum_{i = 1}^{k - 1} {N - 1 \choose i} + \sum_{i = 1}^{k - 1} {N - 1 \choose i - 1} \\
&= 1 + \sum_{i = 1}^{k - 1} \left[ {N - 1 \choose i} + {N - 1 \choose i - 1} \right]
\end{align}
$$

We can reduce the final expression further by considering the example. You are in a room full of $N$ people, and want to determine how to choose $10$ people in the room: $\smash {N \choose 10}$. However, this can also be represented as the number of ways we can count $10$ people _excluding_ you and _including_ you; both cases are disjoint and together cover all combinations.

The number ways to choose $10$ people _excluding_ you is $\smash {N - 1 \choose 10}$. It's $N - 1$ because you are being excluded from the total number of people. The number of ways to choose $10$ people _including_ you is $\smash {N - 1 \choose 9}$. It's $9$ because you're already being included, so you just need $9$ others. Adding these both together should therefore be the same as $\smash {N \choose 10}$.

$$
\begin{align}
\sum_{i = 0}^{k - 1} {N \choose i} &= 1 + \sum_{i = 1}^{k - 1} \left[ {N - 1 \choose i} + {N - 1 \choose i - 1} \right] \\
&= 1 + \sum_{i = 1}^{k - 1} {N \choose i} \\
&= \upperbound \\ \\
\therefore\ B(N, k) &\leq \upperbound \quad \text {q.e.d}
\end{align}
$$

So the above proves that the growth function $\growthfunc(N)$ is bounded by a polynomial $B(N, k)$. It's polynomial because for a given $\mathcal H$, the break point $k$ is fixed.

$$ \growthfunc(N) \leq \underbrace {\upperbound}_{\text {maximum power is } N^{k - 1}} $$

To show that this holds, we can use it to determine the growth function $\growthfunc$ for:

* $\mathcal H$ is **positive rays** (break point $k = 2$):
    * $\growthfunc(N) = N + 1 \leq N + 1$
* $\mathcal H$ is **positive intervals** (break point $k = 3$):
    * $\growthfunc(N) = \frac 1 2 N^2 + \frac 1 2 N + 1 \leq \frac 1 2 N^2 + \frac 1 2 N + 1$
* $\mathcal H$ is **2D perceptrons** (break point $k = 4$):
    * $\growthfunc(N) = \text {?} \leq \frac 1 6 N^3 + \frac 5 6 N + 1$

## Vapnik-Chervonenkis Inequality

We would like to replace the $M$ factor in Hoeffding's Inequality:

$$ P(|\insample(g) - \outsample(g)| \gt \epsilon) \leq 2Me^{-2 \epsilon^2 N} $$

The following straightforward substitution would not work:

$$ P(|\insample(g) - \outsample(g)| \gt \epsilon) \leq 2 \growthfunc(N) e^{-2 \epsilon^2 N} $$

This is because we need to consider the following questions:

How does $\growthfunc(N)$ relate to overlaps? If we use the VC bound, it takes into consideration the various overlaps of the error.

What do we do about $\outsample$? The problem is that to determine the overlap we need to determine what a "bad point" is: a bad point is one that deviates from $\outsample$, so we need to know $\outsample$. Going back to the sheet with the holes analogy where we could only see the color of the points but not the separating line: to determine the error overlap between two separating lines, we would have to remove the covering sheet (the one with the holes).

We could alleviate this by getting rid of $\outsample$. To do this, we pick **two** samples instead of just one. We already know that $\outsample$ and $\insample$ track each other because $\insample$ is generated from the same distribution as $\outsample$. We give the two samples different names: $\insample$ and $\insample'$. Does $\insample$ track $\insample'$? It is obvious that each of them track $\outsample$, since they were generated from the same distribution, so consequently they **do track each other**, albeit perhaps more loosely.

The advantage of this fact is that, when considering multiple bins, the same situation applies. That is, with multiple bins, the tie between $\outsample$ and $\insample$ became looser and looser. Likewise, when considering two samples, they do track each other but it becomes looser and looser. The advantage of using two samples is that we are in the realm of dichotomies. We are in the realm of dichotomies because we now only care about what happens in the sample, not the input space, even though the sample is larger now at $2N$ not $N$.

With all of this in mind, the **Vapnik-Chervonenkis Inequality** is defined as:

$$ P(|\insample(g) - \outsample(g)| \gt \epsilon) \leq 4 \growthfunc(2N) e^{-{\frac 1 8} \epsilon^2 N} $$

The growth function is parameterized with $2N$ because we are now considering two samples.

*[VC]: Vapnik-Chervonenkis
