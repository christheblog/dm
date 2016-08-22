# Data-mining algorithms and experiments


## Description

This project will contain some data-mining algorithm implementations and some experiments I am doing in my spare time.
Hopefully it will end-up providing some usable neat (and simple !) data-mining algorithm implementations like KNN, Naive Bayes, KMeans, NN, etc.

The idea will be to keep things as much functional as possible.

On-going work.


## KHeap

Just a helper functional data-structure to hold the k-nearest neighbours of a point.
This is a log(n) max-Heap implementation, keeping only the k smallest elements.

Access to the biggest element (head operation) and heap size in O(1).
Insertion/Deletion are log(n).

## KDTree

Implementation of KD-Trees + some range and nearest neighbours queries.
