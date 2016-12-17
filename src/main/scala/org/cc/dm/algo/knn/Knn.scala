package org.cc.dm.algo.knn

// Various Knn implementations
object Knn {

  import org.cc.dm.data._
  import org.cc.dm.datastructure.KHeap
  import org.cc.dm.datastructure.KHeap.KHeap
  import org.cc.dm.datastructure.KDTree
  import org.cc.dm.datastructure.KDTree.KDTree


  // Ordering for vectors, relatively to their distance to the target
  val NearestNeighbourDistanceOrdering = new Ordering[(Distance,RealVec)] {
    def compare(x: (Distance,RealVec), y: (Distance,RealVec)): Int =
      if(x._1 < y._1) -1
      else if(x._1 > y._1) 1
      else 0
  }


  // Classic implementations

  // Plain Knn implementation using a KHeap structure
  // Returns a KHeap as a result
  // Note : Result is not interpreted as a predicted class, but could be merged with some other knn calls
  // for a divide-and-conquer parallel/distributed approach for instance.
  def knn(ds: RealDataset)(distfn: DistFn)(k: Int)(target: RealVec): KHeap[(Distance,RealVec)] = {
    // Implicit ordering for vectors, relatively to their distance to the target
    implicit val order = NearestNeighbourDistanceOrdering
    // Computing K-nearest neighbours using a KHeap
    ds.toStream
      .map { vec => (distfn(vec,target),vec) }
      .foldLeft(KHeap.empty[(Distance,RealVec)]) { case (heap, (distance,vec)) =>
        KHeap.insert(k)(heap)((distance,vec))
      }
  }

  // KD-Tree implementation

  // Knn using a KD-Tree with a nearest-neighbours query
  // Returns a KHeap as a result for consistency with other
  def knn(tree: KDTree)(distfn: DistFn)(k: Int)(target: RealVec): KHeap[(Distance,RealVec)] = {
    val kNeigbours = KDTree.nnQuery(tree)(distfn)(k)(target)
    implicit val order = NearestNeighbourDistanceOrdering
    KHeap.from(k)(kNeigbours.map(_.swap):_*)
  }


}
