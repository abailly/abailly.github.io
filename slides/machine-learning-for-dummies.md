------------
title: Understanding word2vec
subtitle: Machine Learning for Dummies
author: Arnaud Bailly
date: 2016-09-05
theme: black
------------ 

# Motivation

----

## I Applied for a Job

> Extract the top 400 articles from Arxiv corresponding to the query `big data`, analyze 
> their content using Google's word2vec algorithm, then run a principal component analysis over
> the resulting words matrix and display the 100 most frequent words' position on a 2D plane.
> In Haskell...

## Data Analysis Pipeline

* Data acquisition and preparation
* Data analysis and modelling 
* Data visualisation

## Understanding ML

* Beyond tools implementation: Word2vec is already implemented in various toolkits: [gensim](https://radimrehurek.com/gensim/models/word2vec.html), [Google's TensorFlow](https://www.tensorflow.org/versions/r0.10/tutorials/word2vec/index.html), [spark-mllib](http://spark.apache.org/docs/latest/mllib-feature-extraction.html#word2vec)...
* Beyond mathematical formulas and theoretical principles
* Sharpen intuitions about ML

## Challenges

* Understanding Basic Algorithm
* Optimizing for very large datasets
* Completing the Pipeline

# Understanding Basic `word2vec` Algorithm

## Basic formulation

* Goal: Build a *words embedding* model, e.g. a function $e: W \rightarrow R^d$ that maps each word from a given vocabulary $W$ to a high-dimensional *vector* space
* Word2vec is actually more a *family* of models: 
    * 2 basic models: Continuous Bag-of-Words (CBOW) and **Skip-Gram** and various optimisations 
    * Several variations 

## Basic Formulation

![](/images/w2v-model.png)

## Skip-Gram Model

Maximises probability of identifying context words for each word of the vocabulary $W$

$$
\frac{1}{T} \sum_{t=1}^{T} \sum_{-c\leq j \leq c, j\neq 0} \log p(w_{t+j}|w_t)
$$

* $T$ is the size of the vocabulary $W$, $w_j$ is the $j$-th word of $W$
* $c$ is the size of the *context window*

## Skip-Gram Model

Define conditional probability $p(w'|w)$ using *softmax* function:

$$
p(w_O|w_I) = \frac{\exp(v'_{w_O}^{\top} v_{w_I})}{\sum_{i=1}^{T} \exp(v'_{w_i}^{\top} v_{w_I})}
$$

## Neural Network

![](/images/w2v-nn.png)

## Feed Forward

![](/images/w2v-ff.png)

## Back-Propagation

![](/images/w2v-backprop-output.png)

-----

![](/images/w2v-backprop-input.png)

---- 

$$
W_{new}' = W' - \alpha G_O
$$ 


$$
w_I_{new} = w_I - \alpha h'
$$ 

## Coding the Model

# Optimizing

## Problem

* While correct and straightforward, complexity of basic implementation is huge: For each sample, we need to compute error gradient over $W'$ which has size $T x D$.
* Training speed is about $1/20^th$  of reference implementation
* Major contribution of word2vec papers is their ability to handle billions of words...
* How can they do it?

## Proposed Optimisations

* Input words sub-sampling: Randomly discard frequent words while keeping relative frequencies identical
* Parallel training: Train model in parallel, requires model update to be non-conflicting
* Negative sampling
* **Hierarchical Softmax**

## Hierarchical Softmax

**Idea**: Approximate probability over $V$ with probabilities over *binary encoding* of $V$

* Output vectors encode a word's  *path* within the binary tree
* Reduces complexity of model training to updating $\log(V)$ output vectors instead of $V$ 

## Huffman Tree

![](/images/w2v-huffman.png)

---- 

Huffman coding encode words according to their frequencies: More frequent words are assigned shorter codes

```.haskell
> huffmanEncode [("A", 25), ("B", 25), ("C", 20), ("D", 15)
                 , ("E", 10), ("F",5)]
("A",Coding {frequency = 25, huffman = 10,   wordPoints = [2,4]})
("D",Coding {frequency = 15, huffman = 011,  wordPoints = [1,3,4]})
("E",Coding {frequency = 10, huffman = 1111, wordPoints = [0,1,3,4]})
("B",Coding {frequency = 25, huffman = 01,   wordPoints = [3,4]})
("C",Coding {frequency = 20, huffman = 00,   wordPoints = [2,4]})
("F",Coding {frequency = 5,  huffman = 0111, wordPoints = [0,1,3,4]})
```

## Original Code

![](/images/w2v-code.png)

# Other Challenges

## Data Acquisition

* Retrieve PDFs from Arxiv site
* Extract textual from PDF
* Cleanup text for analysis

## Data Visualisation

![](https://www.tensorflow.org/versions/r0.10/images/linear-relationships.png)

---- 

* Find some way to reduce dimensionality of space
* Most well-known technique is **Principal Component Analysis**
* Other techniques: t-SNE

## Computing PCA

* Standard tool from statistical analysis and linear algebra
* Transform the "basis" of the vector space to order then by decreasing amount of variance
* Select the first 2 or 3 axis to display data on 2D or 3D diagram

## Optimizing PCA

![](/images/w2v-pca-perf-bench.png)

----

* Textbook computation of PCA means computing full covariance matrix for dataset then eigenvectors of this covariance matrix
* For a 50000 x 200 matrix this is extremely time consuming...
* There exist iterative methods to compute principal components one at a time

# Conclusions

## Machine Learning is Hard

* *In theory*: Get some data, find a suitable model, fit model to data using standard logistic regression, use model
* *In practice*:
    * Datasets need to be large which means algorithms need to be efficient
    * Efficient algorithms require clever optimisations that are non obvious
    * Hard to get "right"...

## Machine Learning is Hard (2)

* Still an active research field: Going from research paper to tool is not straightforward
* Reverse engineering code was a painful process given the low-level of engineering effort put into it
* To really understand what one's doing requires understanding of lot of maths: Linear algebra, statistics, numerical analysis, Natural Language Processing...

## Machine Learning is Fun

* Stretches your programming skills beyond their limits
* Forces you to tackle tough concepts and techniques
* Definitely *hype*

## Takeaways

* There is not best way to understand software than to implement it
* For production, don't roll your own ML engine unless:
    *  that's your core skills domain 
    *  and/or you are prepared to spend time and money

