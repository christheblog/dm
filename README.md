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


## Majority

A dummy classifier which just returns the most frequent class in the dataset.
This is useful when benchmarking an algorithm.


## KNN

Implementation of KNN algorithms.

Classic implementation which returns a KHeap containing the k nearest neighbours.
```scala
def knn(ds: RealDataset)(distfn: DistFn)(k: Int)(target: RealVec): KHeap[(Distance,RealVec)]
```

Returning KHeap[(Distance,RealVec)] allows a divide-and-conquer approach to compute the neighbours, in conjunction with KHeap.merge
This is definitely useful when using some parallelism.

Another implementation is using directly a KDTree and performs a nearest-neighbours query on it.
```scala
def knn(tree: KDTree)(distfn: DistFn)(k: Int)(target: RealVec): KHeap[(Distance,RealVec)]
```


## KMeans

Implementation of KMeans algorithms.

```scala
def kmeans(ds: RealDataset)(distfn: DistFn)(clusters: Clusters): Stream[Clusters]
```

It returns a Stream[Clusters], so the stop condition can easily be customized.

```scala
// Doing 100 iterations of kmeans
(kmeans(ds)(Euclidean)(init)).drop(100).head

// Waiting for convergence (assuming a stable test function with for clusters)
```scala
val kms = kmeans(ds)(Euclidean)(init)
val result = (kms zip kms.tail)
  .take(5000) // Making sure we don't do more than 5000 iterations
  .dropWhile { case (prev,current) => !stable(prev,current) } // iterating until stable
  .map(_._2)
  .headOption
```





