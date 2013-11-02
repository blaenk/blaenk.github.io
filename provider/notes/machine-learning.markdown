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

Given input $\def \feature {\mathbf x} \feature = (x_1, x_2, \dots, x_n)$:

$$
\begin{align}
\text {approve} &\colon \sum_{i=1}^{d} w_i x_i > \text {threshold} \\
\text {deny}    &\colon \sum_{i=1}^{d} w_i x_i < \text {threshold}
\end{align}
$$

This can be expressed as a linear formula $h \in \cal H$:

$$
\newcommand{sign}{\operatorname{sign}}
h(x) = \sign \left( \left( \sum_{i=1}^{d} w_i x_i \right) - \text {threshold} \right)
$$

The factors that are varied to determine the hypothesis function are the weights and threshold.

The threshold can be represented as a weight $w_0$ if an artificial constant $x_0$ is added to the feature vector where $x_0 = 1$. This simplifies the formula to:

$$ h(x) = \sign \left( \sum_{i=0}^{d} w_i x_i \right) $$

This operation is the same as the dot product of the two vectors:

$$
\def \weight {\mathbf w}
\def \weightT {\weight^\intercal}
h(x) = \sign(\weight \bullet \feature)
$$

### Learning Algorithm {#perceptron-learning-algorithm}

**Given the perceptron**:

$$ h(x) = \sign(\weight \bullet \feature) $$

**Given the training set**:

$$\feature = (x_1, x_2, \dots, x_n)$$

**Pick a misclassified point**. A point is misclassified if the perceptron's output doesn't match the recorded data's output given an input vector:

$$ \sign(w \bullet x_n) \not= y_n $$

**Update the weight vector**: The algorithm must correct for this by updating the weight vector.

$$ \weight' \gets \weight + y_n x_n $$

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
\insample(\weight) &= \frac {1} {N} \sum_{n = 1}^N (\weightT \mathbf {x}_n - y_n)^2 \\
                  &= \frac {1} {N} \sum_{n = 1}^N (\weight \bullet \mathbf {x}_n - y_n)^2
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
&\nabla \insample(w) = \frac {2} {N} \mathrm {X}^{\mathrm {T}} \left( \mathrm {X} \mathbf {w} - \mathbf {y} \right) = 0 \\
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

$$
\begin{align}
\growthfunc(N) &= {N + 1 \choose 2} + 1 \\
&= \frac 1 2 N^2 + \frac 1 2 N + 1
\end{align}
$$

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
    * $N + 1 \leq N + 1$
* $\mathcal H$ is **positive intervals** (break point $k = 3$):
    * $\frac 1 2 N^2 + \frac 1 2 N + 1 \leq \frac 1 2 N^2 + \frac 1 2 N + 1$
* $\mathcal H$ is **2D perceptrons** (break point $k = 4$):
    * $\text {?} \leq \frac 1 6 N^3 + \frac 5 6 N + 1$

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

The growth function is parameterized with $2N$ because we are now considering two samples. This inequality more or less says that we are making a statement that is probably (RHS), approximately (LHS epsilon), correct.

# VC Dimension

The **VC Dimension** is a quantity defined for a hypothesis set $\mathcal H$ denoted by $\def \vc {d_{\text {VC}}} \vc(\mathcal H)$ and is defined as the most points that $\mathcal H$ can shatter; the largest value of $N$ for which $\growthfunc(N) = 2^N$.

$$
\begin{align}
N &\leq \vc(\mathcal H)\ \Longrightarrow\ \mathcal H \text { can shatter } N \text { points } \\
N &\gt \vc(\mathcal H)\ \Longrightarrow\ N \text { is a break point for } H
\end{align}
$$

Therefore, the growth function can be defined in terms of a break point $k$:

$$ \growthfunc(N) \leq \upperbound $$

It can also be defined in terms of the VC dimension $\vc$:

$$ \growthfunc(N) \leq \underbrace {\sum_{i = 0}^{\vc} {N \choose i}}_{\text {maximum power is } N^{\vc}} $$

With respect to learning, the effect of the VC dimension is that if the VC dimension is finite, then the hypothesis will generalize:

$$ \vc(\mathcal H)\ \Longrightarrow\ g \in \mathcal H \text { will generalize } $$

The key observation here is that this statement is **independent of**:

* the learning algorithm
* the input distribution
* the target function

The only things that factor into this are the training examples, the hypothesis set, and the final hypothesis.

## Perceptrons {#vc-dimension-of-perceptrons}

We already know that $\vc = 3$ for $d = 2$ (2D). However, we would like to generalize this for any dimension. We will argue that $\vc = d + 1$. We prove this by showing that $\vc \leq d + 1$ and $\vc \geq d + 1$.

To do this we will first construct a set of $N = d + 1$ points in $\mathbb R^d$ in such a way that they can be shattered by the perceptron. We begin this by first setting the first element of every row vector to $1$ since it corresponds to $x_0$: the threshold weight which we have already established is always set to $1$. The rest of the rows are set so that they form an identity matrix which is easily invertible, which is a property that we will show to mean that every point can be shattered:

$$
\mathrm {X} = \begin{bmatrix}
                —\ x_1^{\mathrm {T}}\ — \\
                —\ x_2^{\mathrm {T}}\ — \\
                \vdots \\
                —\ x_{d + 1}^{\mathrm {T}}\ —
              \end{bmatrix} = 
              \begin{bmatrix}
                1 & 0 & 0 & \cdots & 0 \\
                1 & 1 & 0 & \cdots & 0 \\
                1 & 0 & 1 & \cdots & 0 \\
                & \vdots & & \ddots & 0 \\
                1 & 0 & \cdots & 0 & 1
              \end{bmatrix}
$$

The key observation is that $\mathrm X$ is invertible.

$$
\text {For any } \mathrm y = \begin{bmatrix}
                               y_1 \\
                               y_2 \\
                               \vdots \\
                               y_n
                             \end{bmatrix} = 
                             \begin{bmatrix}
                               \pm 1 \\
                               \pm 1 \\
                               \vdots \\
                               \pm 1 \\
                             \end{bmatrix}
$$
 
The question is, can we find a vector $\mathbf w$ satisfying:

$$ \text {sign}(\mathbf {Xw}) = \mathbf y $$

We can do this by changing the requirement to be:

$$ \mathbf {Xw} = \mathbf y $$

This is valid because we're using a binary function.

From here it's a straightforward process to finding the vector $\mathbf w$ by isolating it, made possible by multiplying both sides by the inverse of the feature matrix $\mathrm X$ --- we know it's invertible because we specifically constructed it to be:

$$ \mathbf w = \mathbf {X^{-1}y} $$

Since we were able to come up with $\mathbf w$, it means that we were able to shatter $d + 1$ points. More specifically, it means that we know for a fact that we can shatter _at least_ $d + 1$ points, but it's entirely possible that we can shatter more than that:

$$ \vc \geq d + 1 $$

Therefore, to prove that $\vc = d + 1$, we must now show that $\vc \leq d + 1$, since the combination of both of these proofs would prove that $\vc = d + 1$. To prove that $\vc \leq d + 1$, we must show that we **cannot** shatter _any_ set of $d + 2$ points.

For any $d + 2$ points $\mathbf {x}_1, \dots, \mathbf {x}_{d + 1}, \mathbf {x}_{d + 2}$, it is obvious that there are more points than dimensions. This is because each feature vector $\mathbf x_i$ is a $d + 1$ vector. Since there are more vectors than dimensions, we know that the vectors are linearly dependent; one of the vectors $\mathbf x_j$ can be represented in terms of the other $\mathbf x_i$ vectors:

$$ \mathbf x_j = \sum_{i \neq j} a_i \mathbf x_i $$

It can be said that not all of the $a_i$ scalars are zeros, this follows from the fact that $x_0$ is always $1$.

Now consider the specific dichotomy where the vectors in $\mathbf x_i$ with a non-zero $a_i$ get the value $y_i = \text {sign}(a_i)$, the vectors with zero-valued $a_i$ get the value $\pm 1$, and the vector $\mathbf x_j$ gets the value $y_i = -1$:

$$
\begin{align}
y_i &= \text {sign}(\mathbf w^{\mathrm T} \mathbf x_i) =
       \begin{cases}
         \text {sign}(a_i) & \text {if } a_i \text { is non-zero} \\ \\
         \pm 1 & \text {otherwise}
       \end{cases} \\ \\
y_j &= \text {sign}(\mathbf w^{\mathrm T} \mathbf x_j) = -1
\end{align}
$$

We are going to argue that no perceptron can implement this dichotomy. Remember that $\mathbf x_j$ is the linear sum of the rest of the vectors $\mathbf x_i$, each scaled by a factor $a_i$. We now multiply them by a weight vector, in order to represent a perceptron:

$$
\mathbf x_j = \sum_{i \neq j} a_i \mathbf x_i\ \Longrightarrow\ \mathbf w^{\mathrm T} \mathbf x_j = \sum_{i \neq j} a_i \mathbf w^{\mathrm T} \mathbf x_i
$$

An observation to make is that if by definition of the dichotomy, $\text {sign}(\mathbf w^{\mathrm T} \mathbf x_i) = \text {sign}(a_i)$, then it means that the signs of both are the same. By extension, this means that their product will always be greater than zero:

$$
\begin{align}
\text {if } &y_i = \text {sign}(\mathbf w^{\mathrm T} \mathbf x_i) = \text {sign}(a_i) \\
\text {then } &a_i \mathbf w^{\mathrm T} \mathbf x_i \gt 0
\end{align}
$$

This by extension means that the sum of the $\mathbf x_i$'s will also greater than zero:

$$
\mathbf w^{\mathrm T} \mathbf x_j = \sum_{i \neq j} a_i \mathbf w^{\mathrm T} \mathbf x_i > 0
$$

Therefore, since we defined $y_j = \text {sign}(\mathbf w^{\mathrm T} \mathbf x_j)$ in our dichotomy, the value of $y_j$ will always be:

$$ y_j = \text {sign}(\mathbf w^{\mathrm T} \mathbf x_j) = +1 $$

However, in our dichotomy we defined $y_j$ as being equal to $-1$:

$$ y_j = \text {sign}(\mathbf w^{\mathrm T} \mathbf x_j) = -1 $$

Therefore we cannot appropriately represent our dichotomy.

Now we have proved that $\vc \leq d + 1$ and $\vc \geq d + 1$. Therefore:

$$ \vc = d = 1 $$

What is $d + 1$ in the perceptron? It is the number of parameters in the perceptron model: $w_0, w_1, \dots, w_d$. In essence, this means that when we have a higher number of parameters, we will have a higher $\vc$.

## Interpretation {#vc-dimension-interpretation}

The parameters in the weight vector correspond to degrees of freedom that allow us to create a specific hypothesis. The number of parameters correspond to **analog** degrees of freedom: varying any single parameter --- which is itself continuous --- yields an entirely new perceptron. The VC dimension translated these into **binary** degrees of freedom, since we're only trying to get a different dichotomy.

This is important because it allows us to ascertain how expressive a model may be; how many different outputs we can actually get.

There is a distinction between parameters and degrees of freedom. This is because parameters may not contribute degrees of freedom.

For example, consider a 1D perceptron. This consists of a weight parameter and a threshold parameter, resulting in two degrees of freedom. This results in $\vc = d + 1 = 2$.

Now consider the situation where the output of this model is fed as input into another perceptron, which is fed to another perceptron and so on, for a total of four linked perceptrons. This corresponds to $8$ parameters since each perceptron contains $2$. **However**, there are still only $2$ degrees of freedom because every perceptron after the first simply returns the input; they are redundant.

For this reason, we can think of $\vc$ as measuring the **effective** number of parameters.

## Minimum Sample Size

Knowing all this, we would like to find a way to determine the number of training points needed to achieve a certain level of performance. The first realization is that merely having a finite VC dimension means it's even possible to learn. We should also remember that the VC inequality has two quantities that we would like to minimize: $\epsilon$ and $\delta$:

$$ P(|\insample(g) - \outsample(g)| > \epsilon) \leq \underbrace {4 \growthfunc(2N) e^{- \frac 1 8 \epsilon^2 N}}_{\delta} $$

What we would like to do is say that we want a particular $\epsilon$ and $\delta$. For example, we would like to be at most 10% away from $\outsample$ ($\epsilon = 10\% = 0.1$) and we want that statement to be correct at least 95% of the time ($\delta = 5\% = 0.05$). How many examples do we need to satisfy these constraints?

The question is, how does $N$ depend on $\vc$? Consider a simplification of the RHS:

$$ N^d e^{-N} $$

An observation made from plotting the above for increasing values of $d$ with $N$ vs the logarithm of the probability shows that $N$ is proportional to $\vc$. A practical observation made by machine learning academics and practitioners is that the actual quantity we are trying to bound follows the same monotonicity as the actual bound, e.g. a bigger VC dimension yields bigger quantities, if not close to proportional. The higher the VC dimension, the more examples that are required.

The **rule of thumb** is that for a large range of reasonable $\epsilon$ and $\delta$, and for a large range of practical applications, you need at least 10 times the VC dimension:

$$ N \geq 10\ \vc $$

## Generalization Bound

We will now simplify the VC inequality. We'll begin by denoting the RHS as $\delta$:

$$ P(|\insample(g) - \outsample(g)| > \epsilon) \leq \underbrace {4 \growthfunc(2N) e^{- \frac 1 8 \epsilon^2 N}}_{\delta} $$

We want to get $\epsilon$ in terms of $\delta$, to allow us to state the reliability $\delta$ of the statement we would like to achieve, and have the equation output the tolerance $\epsilon$ that the statement can guarantee with that reliability constraint.

$$
\delta = 4 \growthfunc(2N) e^{- \frac 1 8 \epsilon^2 N}\ \Longrightarrow\ \epsilon = \underbrace {\sqrt {\frac 8 N \ln {\frac {4 \growthfunc(2N)} \delta}}}_{\Omega}
$$

This means that the probability that $\insample$ tracks $\outsample$ within $\Omega$ is at least $1 - \delta$:

$$ P(|\outsample - \insample| \leq \Omega(N, \mathcal H, \delta)) \geq 1 - \delta $$

The absolute value can be removed because $\insample$ is usually much smaller than $\outsample$, since $\insample$ is the value we minimize deliberately. The difference between $\outsample$ and $\insample$ is known as the **generalization error**:

$$ P(\outsample - \insample \leq \Omega) \geq 1 - \delta $$

This can then be rearranged and simplified further into the **generalization bound**:

$$ P(\outsample \leq \insample + \Omega) \geq 1 - \delta $$

The effect of the generalization bound is that it bounds the unknown value $\outsample$ by values we do know, namely $\insample$ and $\Omega$.

# Bias-Variance Tradeoff

We have noticed that there is a trade off between approximation and generalization. In other words, we want to minimize $\outsample$, such that we attain a good approximation of $f$ out of sample. The more complex the hypothesis set $\mathcal H$, the better chance we have of **approximating** $f$. However, the less complex of a hypothesis set $\mathcal H$, the better the chance of **generalizing** out of sample.

$$
\begin{align}
\text {more complex } \mathcal H\ &\Longrightarrow\ \text {better chance of } \textbf {approximating }  f \\
\text {less complex } \mathcal H\ &\Longrightarrow\ \text {better chance of } \textbf {generalizing } \text {out of sample}
\end{align}
$$

We have already learned that VC analysis is one approach of decomposing $\outsample$:

$$ \outsample \leq \insample + \Omega $$

Bias-Variance analysis is another approach to decomposing $\outsample$, which does so into two components:

* how well $\mathcal H$ can approximate $f$
* how well we can zoom in on a good $h \in \mathcal H$

Bias-Variance applies to **real-valued targets** and uses **squared error**.

$$ \outsample(g^{(\mathcal D)}) = \mathbb E_{\mathrm x} \left[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \right] $$

$g^{(\mathcal D)}$ refers to the fact that the hypothesis comes from the dataset $\mathcal D$.

Given a budget of $N$ training examples, we want to generalize the above for any data set of size $N$:

$$
\begin{align}
\mathbb E_{\mathcal D} \left[ \outsample(g^{(\mathcal D)}) \right] &= \mathbb E_{\mathcal D} \left[ \mathbb E_{\mathrm x} \left[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \right] \right] \\
&= \mathbb E_{\mathrm x} \left[ \mathbb E_{\mathcal D} \left[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \right] \right]
\end{align}
$$

We're only going to focus on the inner component of the RHS. We're going to consider the concept of an average hypothesis $\def \avghypo {\bar g} \avghypo$ so that:

$$ \avghypo(\mathbf x) = \mathbb E_{\mathcal D} \left[ g^{(\mathcal D)}(\mathbf x) \right] $$

The average hypothesis for a particular point $\mathbf x$, $\avghypo(\mathbf x)$, is equivalent to finding the hypothesis found from many different data sets, then averaging the result of each of those hypothesis on the point $\mathbf x$:

$$
\avghypo(\mathbf x) \approx \frac 1 K \sum_{k = 1}^K g^{(\mathcal D_k)}(\mathbf x)
$$

## Representation {#bias-variance-representation}

We will now decompose the expected error into two components, bias and variance:

$$
\begin{align}
\mathbb E_{\mathcal D} \Big[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \Big]
&= \mathbb E_{\mathcal D} \left[ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x) + \bar g(\mathbf x) - f(\mathbf x))^2 \right] \\
&= \mathbb E_{\mathcal D} \Big[ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x))^2 + (\bar g(\mathbf x) - f(\mathbf x))^2 \\
&\phantom {= \mathbb E_{\mathcal D}} + 2\ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x))\ (\bar g(\mathbf x) - f(\mathbf x)) \Big] \\
&= \mathbb E_{\mathcal D} \left[ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x))^2 \right] + (\bar g(\mathbf x) - f(\mathbf x))^2
\end{align}
$$

The result is an equation that says that the expected error $\mathbb E_{\mathcal D}$ for a particular data set $\mathcal D$ given the hypothesis $g^{(\mathcal D)}(\mathbf x)$ resulting from that given data set is measured against the actual target function $f(\mathbf x)$, and that error measure **is equivalent** to the **variance** --- the expected error of the hypothesis $g^{(\mathcal D)}(\mathbf x)$ measured against the average hypothesis $\bar g(\mathbf x)$ --- **plus** the **bias** --- the error measure of the average hypothesis $\bar g(\mathbf x)$ against the target function $f(\mathbf x)$:

$$
\def \bias {\textbf {bias} (\mathbf x)}
\def \var {\textbf {var} (\mathbf x)}
\mathbb E_{\mathcal D} \Big[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \Big] = \underbrace {\mathbb E_{\mathcal D} \left[ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x))^2 \right]}_{\var} + \underbrace {\vphantom {\Big[} (\bar g(\mathbf x) - f(\mathbf x))^2}_{\bias}
$$

Both the bias and the variance in the above equation are as measured from a particular point $\mathbf x$. The bias essentially represents the bias of the hypothesis set $\mathcal H$ away from the target function. The variance is essentially measuring the effect of the finite data set, where each finite data set will vary in its result. Therefore:

$$
\begin{align}
\mathbb E_{\mathcal D} \left[ \outsample(g^{(\mathcal D)}) \right] &= \mathbb E_{\mathrm x} \left[ \mathbb E_{\mathcal D} \left[ (g^{(\mathcal D)}(\mathbf x) - f(\mathbf x))^2 \right] \right] \\
&= \mathbb E_{\mathrm x} \Big[ \bias + \var \Big] \\
&= \textbf {bias} + \textbf {var}
\end{align}
$$

## Tradeoff

There is a trade off between the bias and the variance:

$$
\textbf {bias} = \mathbb E_{\mathrm x} \Big[ (\bar g(\mathbf x) - f(\mathbf x))^2 \Big] \qquad \textbf {var} = \mathbb E_{\mathrm x} \Big[ \mathbb E_{\mathcal D} \Big[ (g^{(\mathcal D)}(\mathbf x) - \bar g(\mathbf x))^2 \Big] \Big]
$$

If we go from a small hypothesis to a bigger one, the bias goes down but the variance goes up:

$$ \mathcal H \uparrow\colon \qquad \textbf {bias} \downarrow \qquad \textbf {variance} \uparrow $$

We can now formulate the relations between the hypothesis for a given dataset $\mathcal D$ known as $g^{(\mathcal D)}(\mathbf x)$, the average hypothesis $\bar g(\mathbf x)$, and the target function $f(\mathbf x)$:

$$
g^{(\mathcal D)}(\mathbf x)\ \xrightarrow[\text {variance}]{}\ \bar g(\mathbf x)\ \xrightarrow[\text {bias}]{}\ f(\mathbf x)
$$

We will now use an example to prove that this is true. Given a target function that is a sinusoid:

$$ f\colon [-1, 1] \to \mathbb R \qquad f(x) = \sin(\pi x) $$

We are only given two examples $N = 2$. There are two hypothesis sets, one is a constant model and the other one is linear, and we would like to see which one is better:

$$
\begin{align}
&\mathcal H_0 \colon \quad h(x) = b \\
&\mathcal H_1 \colon \quad h(x) = ax + b
\end{align}
$$

We will now see which one fares better by approximating what we think would be the best hypothesis from each set. In the case of the constant model, we would choose the line at $y = 0$ to minimize the MSE. The linear model would use a line that also tries to minimize the MSE. The following is the $\outsample$ for both:

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/constant-model.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/linear-model.png">
</div>

It's clear that from approximation, $\mathcal H_1$ seems to be better. We will now see which one fares better through machine learning. For example, for these particular two points we get the following result:

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/learning-constant-model.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/learning-linear-model.png">
</div>

The problem is that these results depend on the two points that we were given, so it complicates the task of comparing the two hypothesis sets. This is why we need bias-variance analysis, it gives us the expected error _with respect to_ the choice of the data set.

If we were derive a hypothesis from any two points, for a large number of different two points, we would come up with something like the left image, where every line represents a derived hypothesis. It therefore stands to reason that the average hypothesis would fall somewhere near $y = 0$ --- the midpoint of the range of possible hypotheses. The error measure of the average hypothesis against the target function is the **bias**, and the **variance** is represented by the gray region which corresponds to the standard deviation of the possible hypotheses. It's apparent that this model has a **high bias** ($0.5$) and a **low variance** ($0.25$):

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/multiple-hypotheses.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/average-hypothesis.png">
</div>

The same is slightly more complicated with the second hypothesis $\mathcal H_1$ because of its linear model, which yields very different hypotheses --- that is, **high variance** ($1.69$). There is **low bias** ($0.21$) however, because it has many different hypotheses to average from:

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/linear-multiple-hypotheses.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/linear-average-hypothesis.png">
</div>

It's clear that when the two components are summed for both models, the expected error of the first hypothesis set $\mathcal H_0$ is much lower than $\mathcal H_1$'s.

The **conclusion** from this example is that we are matching the **model complexity** to the **data resources**, not to the **target complexity**.

## Learning Curves

A learning curve plots the expected value of $\outsample$ and $\insample$ as a function of $N$. For a data set of size $N$, how does the expected $\outsample$ and expected $\insample$ vary with $N$?

The following images are learning curves for a simple and complex model. The simple model shows that $\outsample$ decreases with the $N$, but so does $\insample$ --- which can be attributed to exceeding the degrees of freedom available in the hypothesis set. The second model has so many degrees of freedom that it can fit the training set perfectly until the part where the blue curve appears on the left side, however, $\outsample$ is very high before that point --- this corresponds to not learning anything; just memorizing the examples. Therefore if there are very few examples, then it's clear that the simple model would fare better. This is why we want to match the model's complexity to the data resources that we have.

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/simple-learning-curve.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/complex-learning-curve.png">
</div>

The following is a comparison of the learning curves for a given model using VC analysis and bias-variance analysis. In the VC analysis curve on the left, the blue region is $\insample$ and the red region is $\Omega$ --- what happens within the generalization bound. In the Bias-Variance curve, the black bar is the approximation. Everything below the approximation is the bias, so everything else under the $\outsample$ curve must be the variance. Both curves are talking about approximations. The Bias-Variance curve is concerning over-all approximation, whereas the VC analysis curve is concerning in-sample approximation.

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/bias-variance-tradeoff/vc-learning-curve.png">
  <img src="/images/machine-learning/bias-variance-tradeoff/bias-variance-learning-curve.png">
</div>

## Analysis of Linear Regression

Given a data set $\mathcal D = \{ (\mathbf x_1, y_1), \dots, (\mathbf x_n, y_n)\}$ and a noisy target function:

$$y = \mathbf w^{* \mathrm T} \mathbf x + \text {noise}$$

The linear regression solution is:

$$ \mathbf w = \left( \mathrm {X}^{\mathrm {T}} \mathrm {X} \right)^{-1} \mathrm {X}^{\mathrm {T}} \mathbf y $$

The in-sample error vector would be:

$$ \mathrm X \mathbf w - \mathbf y $$

The 'out-of-sample' error vector would be calculated by generating a new set of points from the same inputs but with different noise and would therefore be:

$$ \mathrm X \mathbf w - \mathbf y' $$

This yields the following analysis learning curve. Up until $d + 1$ data points, the data was being fit perfectly. The best approximation error, the variance of the noise, is denoted by $\sigma^2$.

<img src="/images/machine-learning/bias-variance-tradeoff/linear-regression-learning-curve.png" class="center">

We can observe the following characteristics:

$$
\begin{align}
\text {best approximation error} &= \sigma^2 \vphantom {\left(1 - \frac {d + 1} N \right)} \\
\text {expected in-sample error} &= \sigma^2 \left(1 - \frac {d + 1} N \right) \\
\text {expected out-of-sample error} &= \sigma^2 \left(1 + \frac {d + 1} N\right) \\
\text {expected generalization error} &= 2 \sigma^2 \left(\frac {d + 1} N\right)
\end{align}
$$

# Linear Model II

## Non-Linear Transformations II

To recap, a transformation $\Phi$ transforms the input vector $\mathbf x$ to the feature space, resulting in a feature vector $\mathbf z$, where every element $z_i$ is the result of performing a non-linear transformation $\phi_i$ on the _entire_ input vector. Therefore, $\mathbf z$ can be of different size --- often longer --- than the input vector $\mathbf x$:

$$
\begin{align}
\mathrm {x} = (x_0, x_1, \dots, x_d)\ \xrightarrow{\Phi}\ &\mathrm {z} = (z_0, z_1, \dots \dots, z_{\tilde d}) \\
&\text {each } z_i = \phi_i(\mathbf x) \\
&\mathbf z = \Phi(\mathbf x) \\
\text {e.g. } &\mathbf z = (1, x_1, x_2, x_1 x_2, x_1^2, x_2^2)
\end{align}
$$

It's important to remember that the final hypothesis $g(\mathbf x)$ will always be in the $\mathcal X$-space; the $\mathcal Z$-space is transparent to the user.

$$ g(\mathbf x) = \text {sign}(\tilde w^{\mathrm T} \Phi(\mathbf x)) \qquad \text {or} \qquad \tilde w^{\mathrm T} \Phi(\mathbf x) $$

What is the price paid in using a non-linear transformation? Well it has been established that the VC dimension of a $d + 1$ sized input vector is $d + 1$, therefore the feature vector $\mathbf z$, whose size is $\tilde d + 1$, will have a VC dimension of at most $\tilde d + 1$, where $\tilde d$ is generally larger than $d$. It's "at most" because the VC dimension is always measured in the $\mathcal X$-space, and this is the $\mathcal Z$-space. While this means that we will be able to better fit the data, we won't have much of a chance of generalization.

For example considering the following **case 1** where there are two outliers in the data set. If we want to really fit the data, we could use a 4^th^ order surface in order to completely classify the data in the set. However, this increase in complexity would mean that it'd be very difficult to generalize. Sometimes it's best to accept that there will be an $\insample > 0$.

<div style="text-align: center; margin-top: 10px">
  <img src="/images/machine-learning/linear-model-ii/non-linear-case-1.png">
  <img src="/images/machine-learning/linear-model-ii/non-linear-case-1-transformed.png">
</div>

Now consider **case 2**, where we don't stand a chance using a linear model to fit the data, which clearly falls inside a circular region:

<img src="/images/machine-learning/linear-model-ii/non-linear-case-2.png" class="center">

In this case, we can use a non-linear transformation to map the data to a general 2^nd^ order surface as follows:

$$ \mathbf x = (1, x_1, x_2)\ \xrightarrow{\Phi}\ \mathbf z = (1, x_1, x_2, x_1 x_2, x_1^2, x_2^2) $$

Note that with $\mathbf x$ we were only using three weights, however in $\mathbf z$ we are using six weights. This effectively translates to now requiring twice as many examples to achieve the same level of performance. So a natural conclusion is to try to find a way to avoid paying this increase in cost.

First let's consider the following alternative model, since it seems we only need $x_1^2$ and $x_2^2$ to represent the circle. Now we only have three weights again, the same as with the linear model represented by $\mathbf x$:

$$ \mathbf z = (1, x_1^2, x_2^2) $$

We reduce the cost further by adopting the following model, which does away with representing the independence of $x_1^2$ and $x_2^2$, as they simply represent the radius:

$$ \mathbf z = (1, x_1^2 + x_2^2) $$

Now consider the extreme case, where we completely reduce to a single parameter --- completely doing away with the threshold weight:

$$ \mathbf z = (x_1^2 + x_2^2 - 0.6) $$

The problem with these reductions is that the guarantee of the VC inequality is forfeited if we look at the data. We can consider the concept of us simplifying the model for the machine as performing a hierarchical learning process, where we the human do some learning, and then pass the result for the machine to finish the learning process. From this perspective, it's apparent that the VC dimension resulting from this process is that of the entire hypothesis space that we as the human considered during the simplification process.

The bottom line is that **looking at the data _before_ choosing the model can be hazardous to your** $\outsample$. The act of looking at the data before choosing the model is one of the many mistakes that fall under **data snooping**.

## Logistic Regression

We're already familiar with two linear models. The first is **linear classification** which classifies data points based on what side of a hyperplane they're on. The second is **linear regression**, which makes it straightforward to predict a regression, and is computed by skipping the last step of linear classification, the part where it determines the sign of the result.

A third model called **logistic regression** can predict the probability of an event occurring, and is defined as:

$$ h(\mathbf x) = \theta(s) $$

Where $s$ is simply the dot product of the weight and feature vectors as is done in the other two linear models, and $\theta$ is a non-linear function that is often called a **sigmoid**, and looks something like this:

<img src="/images/machine-learning/linear-model-ii/sigmoid.png" class="center">

In our particular case, the formula we're going to use is:

$$ \theta(s) = \frac {e^s} {1 + e^s} $$

This kind of function is also called a "soft threshold," since it's a gradual increase rather than a hard increase as in linear classification. It is in this way that this kind of function expresses uncertainty.

Given this functions' bounds $(0, 1)$ and the fact that it's a "soft threshold," it's clear that it can be interpreted as a probability. For example, when predicting heart attacks, we want to predict whether there's a small or big risk in having a heart attack anytime soon. If we used a binary function as in linear classification, it would be too hard of a threshold and thus wouldn't really convey as much meaningful information, since it isn't clear exactly which things definitely cause heart attacks.

On the other hand, with a logistic regression model, we could predict the probability of having a heart attack in the next 12 months. In this context, the dot product between the weight and feature vector can be considered a "risk score," which can give one a general idea of the result but which must still be passed through the logistic function in order to determine the probability.

A key point is that this probability is considered as a genuine probability. That is, not only does the logistic function have bounds of $(0, 1)$, but that the examples also have a probabilistic interpretation. For example, the input data consists of feature vectors and binary results. It's clear that there is some sort of probability embedded in yielding the binary results, but it is unknown to us. Therefore it can be said that the binary result is generated by a noisy target:

$$
P(y \mid \mathbf x) = \begin{cases}
                        f(\mathbf x) & \text {for } y = +1; \\ \\
                        1 - f(\mathbf x) & \text {for } y = -1.
                      \end{cases}
$$

So the target $f$ is:

$$ f\colon \mathbb R^d \to [0, 1] \text { is the probability} $$

And we want to learn $g$:

$$ g(\mathbf x) = \theta(\mathbf w^{\mathrm T} \mathbf x) \approx f(\mathbf x) $$

### Error Measure {#linear-regression-error-measure}

We have established that for each data point $(\mathbf x, y)$, $y$ is generated by the probability $f(\mathbf x)$. The plausible error measure is based on **likelihood**, that is, we are going to grade different hypotheses according tot he likelihood that they are actually the target that generated the data. In other words, we are going to assume that a given hypothesis is indeed the target function, and then we will determine how likely it is to get a result from its corresponding feature vector. Expressed mathematically:

$$
P(y \mid \mathbf x) = \begin{cases}
                        h(\mathbf x) & \text {for } y = +1; \\ \\
                        1 - h(\mathbf x) & \text {for } y = -1.
                      \end{cases}
$$

Now substitute $h(\mathbf x)$ with $\theta(\weightT \feature)$, while noting that $\theta(-s) = 1 - \theta(s)$, since:

$$
\begin{align}
\theta(-s) &= \frac {e^{-s}} {1 + e^{-s}} \\
&= \frac {\frac 1 {e^s}} {1 + \frac 1 {e^s}} \\
&= \frac 1 {e^s} * \frac 1 {1 + \frac 1 {e^s}} \\
&= \frac 1 {e^s + 1} \\
1 - \theta(s) &= 1 - \frac {e^s} {1 + e^s} \\
&= \frac {1 + {e^s}} {1 + {e^s}} - \frac {e^s} {1 + e^s} \\
&= \frac 1 {1 + e^s}
\end{align}
$$

So now we can simplify the probability to:

$$ P(y \mid \feature) = \theta(y \weightT \feature) $$

Now we can determine the likelihood of the entire data set $\mathcal D = (\feature_1, y1), \dots, (\feature_N, y_N)$ is:

$$
\prod_{n = 1}^N P(y_n \mid \feature_n) =
\prod_{n = 1}^N \theta(y_n \weightT \feature_n)
$$

It's noteworthy to observe that the same weight vector is being used for each of those products, so that if it's varied to better fit one data point, it might no longer fit another. Therefore, whatever maximizes this product would represent the likelihood that the weight vector is representing the underlying probability distribution.

So now we want to see how to maximize the likelihood with respect to $\weight$. This corresponds to minimizing the error:

$$
\begin{align}
&\phantom {=} \frac 1 N \ln \left( \prod_{n = 1}^N \theta(y_n \weightT \feature_n) \right) \\
&= \frac 1 N \sum_{n = 1}^N \ln \left( \frac 1 {\theta(y_n \weightT \feature_n)} \right), \qquad \theta(s) = \frac {e^{-s} \left(e^s\right)} {e^{-s} \left(e^s + 1\right)} = \frac 1 {1 + e^{-s}} \\
\insample(\weight) &= \frac 1 N \sum_{n = 1}^N \underbrace {\ln \left( 1 + e^{-y_n \weightT \feature_n} \right)}_{e\left( h(\feature_n), y_n \right) \rlap {\text {, the cross-entropy error}}}
\end{align}
$$

### Learning Algorithm {#linear-regression-learning-algorithm}

Now that we have the learning model and error measure we can define the learning algorithm. Compared to the closed-form solution from the other two models, we will have to use an iterative solution called **gradient descent**, which is a general method for non-linear optimization.

Gradient descent starts at a point on the error function and iteratively takes steps along the steepest slope towards the minimum. So we'll have a direction unit vector $\hat v$ pointed in the direction of the steepest slope and a fixed step size $\eta$ which we will define as being small since we want to approximate the surface using the first order expansion of the Taylor series --- the linear approximation --- which works best when the distance between the two points is small. With this in mind, the new position after a step can be expressed as:

$$ \weight(1) = \weight(0) + \eta \hat v $$

To derive the direction unit vector $\hat v$, we first observe that the change in the value of the error is simply the difference between the error at the new point and the error at the original point:

$$
\Delta \insample = \insample(\mathbf w(0) + \eta \hat v) - \insample(\mathbf w(0))
$$

Remembering that the Taylor series is defined as:

$$ \sum_{n = 0}^\infty \frac {f^{(n)}(a)} {n!} (x - a)^n $$

Then the first-order approximation is defined as $n = 1$ so that the series is:

$$ f(x) = f(a) + \frac {f'(a)} {1!} (x - a) + R_2 $$

If we drop the remainder term $R_2$, then we are left with the linear approximation:

$$ f(x) \approx f(a) + f'(a)(x - a) $$

Therefore, for $f(x + a)$ it is defined as:

$$
\begin{align}
f(a + x) &\approx f(a) + f'(a)x \\
f'(a)x   &\approx f(a + x) - f(a)
\end{align}
$$

This representation of the linear approximation is similar to our definition of the change in the value of the error between the two positions:

$$
\Delta \insample = \insample(\mathbf w(0) + \eta \hat v) - \insample(\mathbf w(0))
$$

However, we don't want to use the linear approximation just yet. There's another concept known as the [gradient](http://en.wikipedia.org/wiki/Gradient) which basically defines a vector field so that at any given point in the space, a vector is available at that point which points in the direction of the "steepest ascent." The gradient is simply defined as the vector of all of the possible partial derivatives. For example, given a function:

$$ f(x, y, z) = 2x + 3y^2 - \sin(z) $$

The gradient is denoted by the $\nabla$ symbol and is defined as:

$$
\begin{align}
\nabla f &= \frac {\partial f} {\partial x} \hat i + \frac {\partial f} {\partial y} \hat j + \frac {\partial f} {\partial z} \hat k \\
&= 2 \hat i + 6y \hat j - \cos(z) \hat k \\
&= \begin{bmatrix} 2 & 6y & -\cos(z) \end{bmatrix}
\end{align}
$$

It's obvious how the gradient can be useful here. It provides us with a way to determine --- at any point on the surface --- in what direction to move to go deeper towards the minimum. This is possible by finding the gradient vector --- which points in the direction of the "steepest ascent" --- and negate it so that it then points in the direction of the "steepest descent". This is clearly useful, and fortunately there is a similar linear approximation to a vector-taking function [for a gradient](http://en.wikipedia.org/wiki/Gradient#Linear_approximation_to_a_function):

$$ f(\mathbf x) \approx f(\mathbf x_0) + \nabla f(\mathbf x_0) (\mathbf x - \mathbf x_0) $$

This means that for $f(\mathbf x + \mathbf x_0)$:

$$
\begin{align}
f(\mathbf x + \mathbf x_0) &\approx f(\mathbf x_0) + \nabla f(\mathbf x_0) \cdot (\mathbf x - \mathbf x_0) \\
\nabla f(\mathbf x_0) \cdot \mathbf x &\approx f(\mathbf x + \mathbf x_0) - f(\mathbf x_0) \\
\nabla f(\mathbf x_0)^{\mathrm T} \mathbf x &\approx f(\mathbf x + \mathbf x_0) - f(\mathbf x_0) \\
\end{align}
$$


So we can replace the RHS with the dot product of the gradient and the unit vector $\hat v$ [^gradient_question]:

$$
\begin{align}
\Delta \insample &= \insample(\mathbf w(0) + \eta \hat v) - \insample(\mathbf w(0)) \\
&= \eta \nabla \insample(\mathbf w(0))^{\mathrm T} \hat v
\end{align}
$$

Since we know that $\hat v$ is a unit vector, we know that regardless of the value of $\hat v$, the inner product between $\nabla \insample(\mathbf w(0))$ and $\hat v$ cannot exceed the norm $\lVert \nabla \insample(\mathbf w(0)) \rVert$, be it in the positive or negative direction. Therefore we can guarantee that $\Delta \insample$ must be greater than or equal to the negative norm:

$$ \Delta \insample \geq -\eta \lVert \nabla \insample(\mathbf w(0)) \rVert $$

We would therefore like to choose $\hat v$ that is closest to this lower bound, since we want to minimize the error, and a negative change in error corresponds to a big decrease in error, and therefore descending rapidly. Since $\hat v$ is by definition a unit vector, we can simply take the gradient itself and divide it by its norm to normalize it, then negate it to flip from ascent to descent.

$$ \hat v = - \eta \frac {\nabla \insample(\mathbf w(0))} {\lVert \nabla \insample(\mathbf w(0)) \rVert} $$

The above implies a fixed step. We could instead adopt a step distance based on the slope of the current position, easily accomplished by scaling the direction vector $\hat v$ by the norm of the gradient at the current position. This allows the algorithm to take bigger steps in steeper locations, and take smaller steps once it begins to near the minimum. In this case, $\eta$ now refers to the "learning rate":

$$
\Delta \mathbf w = - \eta \frac {\nabla \insample(\mathbf w(0))} {\lVert \nabla \insample(\mathbf w(0)) \rVert} \lVert \nabla \insample(\mathbf w(0)) \rVert = - \eta \nabla \insample(\mathbf w(0))
$$

Now that we have all of the pieces we can construct the learning algorithm:

1. initialize the weights at $t = 0$ to $\weight(0)$
2. for $t = 0, 1, 2, \dots$ do
    1. compute the gradient
    $$ \nabla \insample = - \frac 1 N \sum_{n = 1}^N \frac {y_n \feature_n} {1 + e^{y_n \weightT(t) \feature_n}} $$

    2. update the weights: $\weight(t + 1) = \weight(t) - \eta \nabla \insample$
    3. repeat until it is time to stop
3. return the final weights $\weight$

# Resources

* [Cambridge Information Theory](http://videolectures.net/course_information_theory_pattern_recognition/)
* [Mathematical Monk](http://www.youtube.com/user/mathematicalmonk/videos?flow=grid&view=1)

*[VC]: Vapnik-Chervonenkis
*[MSE]: Mean-Squared Error
*[RHS]: Right-Hand Side
*[LHS]: Left-Hand Side

[^gradient_question]: [How is the gradient derived here? --- Math.StackExchange](http://math.stackexchange.com/questions/546388/how-is-the-gradient-derived-here)
