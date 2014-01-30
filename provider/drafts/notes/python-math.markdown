---
title: Python Math
published: October 7, 2013
excerpt: Python Math packages
comments: off
toc: left
---

What is:

* [Anaconda](http://continuum.io/downloads)
* [scikit-learn](http://scikit-learn.org/stable/)
* [SciPy](http://www.scipy.org/)
    * [NumPy](http://numpy.scipy.org/)
    * [Matplotlib](http://matplotlib.org/)
    * [pandas](http://pandas.pydata.org/)
    * [IPython](http://ipython.org/)
    * [SciPy](http://www.scipy.org/)
* [PyMC](https://github.com/pymc-devs/pymc)
* gensim
* theano

[Probabilistic Programming & Bayesian Methods for Hackers](http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/)

To use PySide with matplotlib ([pyside integration](http://wiki.scipy.org/Cookbook/Matplotlib/PySide), info on [matplotlibrc](http://matplotlib.org/users/customizing.html)):

``` python
matplotlib.use('Qt4Agg')
matplotlib.rcParams['backend.qt4'] = 'PySide'
import pylab
```
