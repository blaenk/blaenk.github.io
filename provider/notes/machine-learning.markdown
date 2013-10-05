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

$\nu$ is the error "**in sample**", which is denoted by $E_{in}(h)$

$\mu$ is the error "**out of sample**", which is denoted by $E_{out}(h)$

For clarification, if something performs well "out of sample" then it's likely that learning actually took place. This notation can be used to modify Hoeffding's Inequality:

$$
P \left( \left|E_{in}(h) - E_{out}(h)\right| \gt \epsilon \right) \leq 2e^{- 2\epsilon^2 N}
$$

The problem now is that Hoeffding's Inequality doesn't apply to multiple bins. To account for multiple bins, the inequality can be modified to be:

$$
\begin{align*}
P \left( \left|E_{in}(g) - E_{out}(g)\right| \gt \epsilon \right) &\leq
P \begin{aligned}[t]
              \left( \vphantom {\epsilon} \right. &\hphantom {\text {or}}\
                \left|E_{in}(h_1) - E_{out}(h_1)\right| \gt \epsilon \\
              &\text {or}\ \left|E_{in}(h_2) - E_{out}(h_2)\right| \gt \epsilon \\
              &\dots \\
              &\left. \vphantom{E_{in}(h_1)} \text {or}\ \left|E_{in}(h_M) - E_{out}(h_M)\right| \gt \epsilon \right)
            \end{aligned} \\
&\leq \sum_{m=1}^M P \left( \left| E_{in}(h_m) - E_{out}(h_m) \right| > \epsilon \right) \\
&\leq \sum_{m=1}^M 2e^{- 2\epsilon^2 N} \\
P \left( \left|E_{in}(g) - E_{out}(g)\right| \gt \epsilon \right) &\leq 2Me^{- 2\epsilon^2 N}
\end{align*}
$$

# Resources

* Cal Tech [CS 1156x](https://www.edx.org/course/caltechx/cs1156x/learning-data/1120) by Yaser S. Abu-Mostafa
* Stanford [CS 229](http://academicearth.org/courses/machine-learning/) by Andrew Ng
* Coursera [Machine Learning](https://www.coursera.org/course/ml) by Andrew Ng, dumbed-down version of the above
* [Data Mining and Analysis: Fundamental Concepts and Algorithms](http://www.dcc.ufmg.br/miningalgorithms/files/pdf/dmafca.pdf)
