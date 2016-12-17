package org.cc.dm.algo.knn

// Knn-like algorithm performed on each dimension independently
// Idea is to select a closest vector dimension per dimension.
object Knn1D {

  import scala.collection.immutable.Seq

  import org.cc.dm.data._
  import org.cc.dm.datastructure.KHeap.KHeap


  type DimSortedDataset = Seq[RealDataset]

  // Sort a dataset dimension per dimension
  def dimSort(ds: RealDataset): DimSortedDataset = {
    def dimSortK(ds: RealDataset, k : Int): RealDataset = ds.sortBy(_(k))
    (0 until Dataset.dimension(ds)).map(dimSortK(ds,_))
  }

  // Binary search to find the index of the closest vector when considering the given dimension
  // Note : Dimension is one-based
  def findClosestIndex(ds: DimSortedDataset)(dim: Int)(target: RealVec): Option[(RealDataset,Int)] = {
    val dsForDim = ds(dim-1)
    val targetValue = target(dim-1)
    // FIXME wrong algo - we need to do a 'look ahead' to determine the correct value
    // I am just lazy tonight ...
    def search(min: Int, max: Int): Int = {
      if(min == max) min
      else {
        val half = (min + max) / 2
        val halfValue = dsForDim(half)(dim-1)
        if(targetValue > halfValue) search(half,max)
        else search(min,half)
      }
    }

    if(Dataset.isEmpty(dsForDim)) None
    else Some((dsForDim,search(0,Dataset.size(dsForDim)-1)))
  }

  // Selecting k nn, dimension per dimension, and then inserting them into a KHeap
  // These may not be the real neighbours that a classic knn would have found
  // Hope is that a number of them are similar, so good classification rate remains close to a classic knn one
  def knn1d(dimSorted: DimSortedDataset)(distfn: DistFn)(k: Int)(target: RealVec): KHeap[(Distance,RealVec)] = {
    val subDatasets = dimSorted.zipWithIndex.map { case (_,index) =>
      val closest = findClosestIndex(dimSorted)(index+1)(target)
      closest.map { case (ds,closestIndex) => Dataset.subset(ds,closestIndex-k,closestIndex+k) }.getOrElse(Dataset.empty)
    }
    val reduced = subDatasets.reduce(_ ++ _)
    // Performing a standard Knn on the reduced set
    Knn.knn(reduced)(distfn)(k)(target)
  }

}
