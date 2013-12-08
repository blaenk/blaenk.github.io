---
title: Applied Machine Learning
published: September 28, 2013
excerpt: Application of Machine Learning concepts
comments: off
toc: left
---

These are separate notes from the [other machine learning notes](/notes/machine-learning/) which are based off of Cal Tech's [CS 1156x](https://www.edx.org/course/caltechx/cs1156x/learning-data/1120) by Yaser S. Abu-Mostafa. These notes are about Stanford's [Machine Learning](https://www.coursera.org/course/ml) class by Andrew Ng. I decided to separate them to make it easier on me, since I'm taking both concurrently. I intend to merge them eventually.

Multivariate linear regression with gradient descent:

$$ \theta_j = \theta_j - \alpha \frac 1 m \sum_{i = 1}^m (h_\theta(x^{(i)}) - y^{(i)}) x_j^{(i)} $$

Use **feature scaling** to get every feature into the approximate range of $-1 \leq x_i \leq 1$.

**Mean normalization** consists of replacing $x_i$ with $x_i \mu_i$ to make features have approximately zero mean. For example, if the average value of the size feature is $1000$, then the mean normalization would consist of:

$$ x_1 = \frac {\text{size} - 1000} {2000} $$

In general:

$$ x_1 \gets \frac {x_1 - \mu_1} {s_1} $$

Where $\mu_1$ is the average value of $x_1$ in the training set, and $s_1$ is the range (max - min).

Convergence in gradient descent can be declared if $J(\theta)$ decreases by less than $10^{-3}$ (value of $\epsilon$) in one iteration.

The pseudo-inverse can be computed in octave using the `pinv` function, so that the normal equation can be computed with:

~~~ octave
pinv(X' * X) * X' * y
~~~

The logistic regression cost function embeds the cost of getting the wrong probability. For example, the cost of predicting $1$ when the result was actually $0$ goes towards $+\infty$:

$$
\begin{align}
\text {Cost}(h_\theta(x^{(i)}, y^{(i)})) &=
\begin{cases}
-\log(h_\theta(x)) & \text {if } y = 1 \\ \\
-\log(1 - h_\theta(x)) & \text {if } y = 0
\end{cases} \\ \\
&= -y \log(h_\theta(x)) - (1 - y)\log(1 - h_\theta(x))
\end{align}
$$

More advanced optimization algorithms aside from gradient descent include:

* conjugate gradient
* BFGS
* L-BFGS

These algorithms have advantages such as not needing to have to manually pick $\alpha$, as well as generally being faster than gradient descent. However, they are more complicated to implement.

Multiclass classification can be performed by training a classifier for each class by treating all members of that class as the positive set and all other data points as the negative set. This is done for each class, and then to classify any new data point, it is classified as the one with the maximum probability.

