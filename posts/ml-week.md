---
title: Some Notes on ML in Haskell following MLWeek 
author: Arnaud Bailly 
date: 2015-11-06
---


From Monday 2/11 to Thursday 5/11 I attended [ML Week](http://www.ml-week.com/fr), a training session lead by
[Jeff Abrahamson](http://jeff.purple.com/qr/) who also organizes the
[Nantes ML Meetup](http://www.meetup.com/fr/Nantes-Machine-Learning-Meetup/). This session was great, not only because of its
content, mostly an overview of the main techniques available and a hands-on dive into Python's eco-system to support data analysis,
but also because of the discussions we had with other attendees and with Jeff whose depth of knowledge on the subject is truely
amazing.

However I was a bit frustrated of not being able to epxlorer the topic using my language of choice, namely Haskell. So I took
opportunity of this training to start collecting links and ideas on how to do data analysis and ML in Haskell. Here are a few links
and comments on  my attempts to *map* the tools we were using in Python to equivalent things in the Haskell eco-system: 

* All the hands-on codelabs for the training were provided in the form of [IPython Notebooks](http://ipython.org/notebook.html), so I
  went to install [IHaskell](https://github.com/gibiansky/IHaskell) which provides a Haskell *kernel* for notebooks. It works great
  straight out of the box, I only had some minor glitches with [Charts](https://hackage.haskell.org/package/Chart) display. I must say
  that the community for IHaskell is very responsive!
* [Kronos](http://www.kronosnotebook.com/haskell) provides a packaged interactive data visualization tool that contains everythign
  that's needed to run IHaskell out-of-the-box when you dont' want to bother with installing Haskell eco-system,
* [cassava](https://hackage.haskell.org/package/cassava) provides type-safe parsing of CSV data,
* There is a base statistics package for Haskell: http://hackage.haskell.org/package/statistics, which is maintained by Brian
  O'Sullivan who is also behind [wreq](https://hackage.haskell.org/package/wreq), the one-stop shop for making HTTP clients. This
  package among many other stuff provides linear regression,
* [HLearn](https://github.com/mikeizbicki/HLearn) is an ambitious project to provide efficient pure Haskell implementations of
  various standard ML algorithms,
* Basic matrices operations are provided by [hmatrix](http://dis.um.es/~alberto/hmatrix/hmatrix.html) which is based on efficient
  routines implemented by LAPACK, BLAS, and GSL,
* [hstatistics](https://hackage.haskell.org/package/hstatistics) is another statistics package based on hmatrix,
* There is a very interesting series of post from Dominik Steinitz: The ones I have been particularly interested in are on
  [linear](https://idontgetoutmuch.wordpress.com/2013/04/26/regression-and-automated-differentiation-4/)
  and [logistic](https://idontgetoutmuch.wordpress.com/2013/04/30/logistic-regression-and-automated-differentiation-3/) regressions
  using *automatic differentiation*. There has been some code drift in AD since the posts were written so they don't compile as-is
  using latest versions of libraries but modifications are minor,
* I thus turned to [ad](http://hackage.haskell.org/package/ad-3.4) package by E.Kmett which happens to contain a routine for
  computing directly approximations of functions through gradient descent techniques,
* [chatter](https://hackage.haskell.org/package/chatter) implements some "standard" NLP algorithms which we had to deal with to
  implement a spam detector,
* Support Vector Machines support in Haskell is implemented in a couple of packages:
    - There are [haskell bindings](http://hackage.haskell.org/package/bindings-svm) to the (apparently) state-of-the-art library
      [libsvm](https://www.csie.ntu.edu.tw/~cjlin/libsvm/) which is what is used by scikit-learn,
    - [svm](https://hackage.haskell.org/package/svm) is another package which seems a bit oldish and unmaintained,
* I don't think there is a compelling implementation of general purpose neural networks of any kind, although there appear to be
quite a few package dealing with those beasts on hackage,
* There are two libraries for computing K-means, both pretty recent:
    * [kmeans-vector](https://hackage.haskell.org/package/kmeans-vector),
    * [kmeans](https://hackage.haskell.org/package/kmeans),
* For Principal Component Analysis, there is 
  [hstatistics](https://hackage.haskell.org/package/hstatistics-0.2.5.3/docs/Numeric-Statistics-PCA.html) or 
  [hmatrix-nipals](https://hackage.haskell.org/package/hmatrix-nipals) 
