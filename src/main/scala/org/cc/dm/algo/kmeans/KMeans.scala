package org.cc.dm.algo.kmeans

object KMeans {

  import collection.immutable.Seq
  import org.cc.dm.data._

  type Cluster = RealVec
  type Clusters = Seq[Cluster]

  // Stream of k-means Clusters
  def kmeans(ds: RealDataset)(distfn: DistFn)(clusters: Clusters): Stream[Clusters] = {
    val iter = kmeansIter(ds)(distfn) _
    clusters #:: kmeans(ds)(distfn)(iter(clusters).map(_._1))
  }

  // One iteration of the k-means algorithm
  // Return a Seq[(RealVec,Int)] (and not just the average vector) to allow for divide-and-conquer computations
  def kmeansIter(ds: RealDataset)(distfn: DistFn)(clusters: Clusters): Seq[(RealVec,Int)] = {
    partitionByClosest[RealVec,RealDataset](ds)(distfn)(clusters)              // Groups by closest vector (index)
      .mapValues { vecs => (Vector.add(vecs),vecs.size) } // compute data for average
      .toIndexedSeq                                       // to sequence
      .sortBy(_._1)                                       // sort by index (just to make sure we retrieve an identical order to our clusters)
      .map(_._2)                                          // extracting vec average data (ie added vectors and count)
  }

  // Partition of a dataset by grouping vectors into the closest cluster
  def partitionByClosest[V <: RealVec, T <: Dataset[V]](ds: T)(distfn: DistFn)(clusters: Clusters): Map[Int,Seq[V]] = {
    val closestfn = Distance.closest(distfn)(clusters) _
    val closestIndexfn = (v: V) => closestfn(v)._1
    ds.groupBy(closestIndexfn) // group by closest vector (index)
  }

}
