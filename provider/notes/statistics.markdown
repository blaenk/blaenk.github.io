---
title: Statistics
published: September 15, 2013
excerpt: Descriptive statistics, probabilities, and inference
toc: left
comments: off
---

I want to freshen up on statistics and probabilities, which are very important for machine learning. The primary source material comes from the UC Berkeley Stat 2.1, 2.2, and 2.3 classes on edX which concern descriptive statistics, probabilities, and inference respectively. Supplementary material at times from Khan Academy. 

* toc

# Descriptive Statistics

# Probability

The probability of an event is the fraction of times it occurs out of the total possible outcomes:

$$ P(E) = \frac {\text {# of events}} {\text {# total outcomes}} $$

The probability is bound in the range of $[0, 1]$. Therefore, the probability of an event not happening---referred to as the probability of the event's complement---is simply defined as:

$$ P(E^c) = 1 - P(E) $$

## Addition Rule

The probability of the union of multiple _mutually exclusive_ events can be determined by simply adding the probabilities of the individual events:

$$
P(E_1 \cup E_2) = P(E_1) + P(E_2) = \sum_{i=1}^{N} P(E_i)
$$

However, if the events are _not mutually exclusive_, then the concept of _inclusion-exclusion_ must be practiced in order to avoid _double counting_, where portions of the probabilities represent the same event:

$$ P(E_1 \cup E_2) = P(E_1) + P(E_2) - P(E_1 \cap E_2) $$

## Conditional Probability

The _conditional probability_ of an event $B$ is the probability that the event $B$ happens given that another event $A$ occurred, and is written as:

$$ P(B \mid A) $$

This is inferred from the problem. For example, in "draw without replacement" problems, the probability of $B$ occurring changes from the mere fact that $A$ occurred, because the total number of outcomes---the denominator of the probability---has decreased by one, since the item from event $A$ was not returned. The numerator also could have decreased by one if it corresponds to the same group as $B$'s. On the other hand, in problems that "draw with replacement," the fact that $A$ has already occurred has no effect on any subsequent events because all items are still present.

## Multiplication Rule

Conditional probability allows us to compute the probability of one event occurring followed by another. This is solved using the _multiplication rule_:

$$
\begin{align}
P(A \cap B) &= P(A) \times P(B \mid A) \\
            &= P(B) \times P(A \mid B)
\end{align}
$$

This is simply read as the probability of $A$ occurring followed by $B$ is the simply the fraction $P(B \mid A)$ of the probability $P(A)$ occurring.

## Problem Solving Techniques

Count the number of **trials**. For example, when drawing cards from a box, every draw is a trial. Given a list of all possible outcomes, count how many outcomes satisfy the conditions of the event.

Example 1
  ~ A random number generator picks at random from the ten digits 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9. Find the chance that the digit it picks is prime and greater than 5.

There is only one **trial** consisting of picking a single digit. The **conditions** of the event are that it is prime and it is greater than 5. Therefore, only one digit satisfies the conditions, 7, so the answer is $1/10$.

Example 2
  ~ In a class, 15% of the students have read _Hamlet_ and 40% have read _The Merchant of Venice_; 5% of the students have read both. What is the chance that a student picked at random has read one of the plays but not the other?

There is only one **trial** consisting of picking a single student at random. The **condition** is simply that the student has read one of the plays but not the other. This can be visualized by drawing a Venn Diagram, with one circle being 15% in size and the other being 40% in size, with an overlap of 5%. Then the areas of interest are either those that don't include the overlap. This can be described as:

$$
\text {A: Hamlet but not The Merchant of Venice:}\ 15\% - 5\% = 10\% \\
\text {B: The Merchant of Venice but not Hamlet:}\ 40\% - 5\% = 35\%
$$

We are interested in both outcomes occurring---the event $A$ _or_ the event $B$---and since they are mutually exclusive, the addition rule can be used to arrive at the answer:

$$
\begin{alignat*}{3}
P(A \cup B)\ & = P(A) && {} + P(B)\ && {} \\
P(A \cup B)\ & = 10\% && {} + 35\%\ && = 45\%
\end{alignat*}
$$

Example 3
  ~ In my pocket I have a quarter, two dimes, and a nickel. If I pull out two coins at random (without replacement), what is the chance that I pull out more than 15 cents?

# Inference

Resources:

* UC Berkeley [Stat 2.1x: Descriptive Statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594)
* UC Berkeley [Stat 2.2x: Probability](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685)
* UC Berkeley [Stat 2.3x: Inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825)
