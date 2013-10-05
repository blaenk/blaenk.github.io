---
title: Statistics
published: September 15, 2013
excerpt: Descriptive statistics, probabilities, and inference
toc: left
---

I want to freshen up on statistics and probabilities, which are very important for machine learning. The primary source material comes from the UC Berkeley Stat 2.1, 2.2, and 2.3 classes on edX which concern descriptive statistics, probabilities, and inference respectively. Supplementary material at times from Khan Academy. 

* toc

# Descriptive Statistics

# Probability

The probability of an event is the fraction of times it occurs out of the total possible outcomes:

$$ P(E) = \frac {\text {# of events}} {\text {# total outcomes}} $$

The probability is bound in the range of $[0, 1]$. Therefore, the probability of an event not happening --- referred to as the probability of the event's complement --- is simply defined as:

$$ P(E^c) = 1 - P(E) $$

## Addition Rule

The probability of the union of multiple **mutually exclusive** events can be determined by simply adding the probabilities of the individual events:

$$
P(E_1 \cup E_2) = P(E_1) + P(E_2) = \sum_{i=1}^{N} P(E_i)
$$

However, if the events are **not mutually exclusive**, then the concept of **inclusion-exclusion** must be practiced in order to avoid _double counting_, where portions of the probabilities represent the same event:

$$
P(E_1 \cup E_2) = P(E_1) + P(E_2) - P(E_1 \cap E_2)
$$

# Inference

Resources:

* Berkeley [Stat 2.1x: Descriptive Statistics](https://www.edx.org/course/uc-berkeley/stat2-1x/introduction-statistics/594)
* Berkeley [Stat 2.2x: Probability](https://www.edx.org/course/uc-berkeley/stat2-2x/introduction-statistics/685)
* Berkeley [Stat 2.3x: Inference](https://www.edx.org/course/uc-berkeley/stat2-3x/introduction-statistics/825)
