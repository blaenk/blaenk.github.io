---
title: Machine Learning
published: September 30, 2013
excerpt: Machine Learning concepts
comments: off
---

Essence of machine learning:

1. pattern exists
2. cannot pin it down mathematically
3. have data on it (to learn from)

A movie recommender system might be modeled such that there are a set of factors of varying degrees of likability for the viewer, and varying degrees of presence in a given movie. These two sets of factors combine to produce a projected viewer rating for that given movie.

The machine learning aspect would take an actual user rating, and given two contributing-factor vectors for the movie and viewer full of random factors, it would modify each until a rating similar to the actual user rating is produced.

# Learning Components

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

# Perceptrons

## Model {#perceptron-model}

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

## Learning Algorithm {#perceptron-learning-algorithm}

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

# Resources

* Cal Tech [CS 1156x](https://www.edx.org/course/caltechx/cs1156x/learning-data/1120) by Yaser S. Abu-Mostafa
* Stanford [CS 229](http://academicearth.org/courses/machine-learning/) by Andrew Ng
* Coursera [Machine Learning](https://www.coursera.org/course/ml) by Andrew Ng, dumbed-down version of the above
* [Data Mining and Analysis: Fundamental Concepts and Algorithms](http://www.dcc.ufmg.br/miningalgorithms/files/pdf/dmafca.pdf)
