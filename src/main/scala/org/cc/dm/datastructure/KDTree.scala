package org.cc.dm.datastructure

object KDTree {

  import collection.immutable.Seq
  import org.cc.dm.data._

  // KDTree
  sealed trait KDTree
  case object Empty extends KDTree
  case class KDNode(point: Vec, k: Int, less: KDTree, more: KDTree) extends KDTree

  def build(ds: Dataset, k: Int = 0): KDTree = {
    // Sort data and split at the median for dimension k - to get a balanced KDTree
    def split(ds: Dataset, k: Int): (Dataset, Vec, Dataset) = {
      val sorted = ds.sortBy(_(k))
      val med = sorted.size / 2
      val (left,right) = sorted.splitAt(med)
      (left,right.head,right.tail)
    }

    if (ds.isEmpty) Empty
    else {
      val dim = ds.head.size
      val (less, vec, more) = split(ds,k)
      KDNode(vec, k, build(less,(k+1)%dim), build(more,(k+1)%dim))
    }
  }

  // Returns a stream of vectors encountered during a search for a target vector
  def stream(tree: KDTree)(target: Vec): Stream[Vec] = tree match {
    case Empty => Stream()
    case KDNode(v,k,less,more) =>
      if(v == target) Stream(v)
      else if(target(k) < v(k)) v #:: stream(less)(target)
      else if(target(k) > v(k)) v #:: stream(more)(target)
      else (v #:: stream(less)(target)) #::: stream(more)(target)
  }


  // Exact match query
  def contains(tree: KDTree)(target: Vec): Boolean = tree match {
    case Empty => false
    case KDNode(v,k,less,more) =>
      if(v == target) true
      else if(target(k) < v(k)) contains(less)(target)
      else if(target(k) > v(k)) contains(more)(target)
      else contains(less)(target) || contains(more)(target)
  }


  // Range query

  // Hyper-cube definition from 2 opposite corners
  type Point = Vec
  type Cube = (Point,Point)

  // Generic Range query to return points in a specified hyper-volume
  // The inside of the hyper-volume is specified by a Vec => Boolean
  // The wrappingCube hyper-cube MUST contain totally the hyper-volume
  def rangeQuery(tree: KDTree)(wrappingCube: Cube)(inside: Vec => Boolean): Stream[Vec] = {
    val (lower,upper) = wrappingCube
    def loop(tree: KDTree): Stream[Vec] = {
      tree match {
        case Empty => Stream()
        case KDNode(v, k, less, more) =>
          if(v(k) < lower(k)) loop(more)
          else if(v(k) > upper(k)) loop(less)
          else if(inside(v)) (v #:: loop(less)) #:::  loop(more)
          else loop(less) #::: loop(more)
      }
    }
    loop(tree)
  }


  // Orthogonal range query

  // Orthogonal Range query to return points inside the specified hyper-cube
  def orthogonalQuery(tree: KDTree)(cube: Cube): Stream[Vec] = {
    val (lower,upper) = cube
    def inside(pt: Vec) =
      (lower zip pt).forall { case (x1,x2) => x1 <= x2 } &&
      (upper zip pt).forall { case (x1,x2) => x1 >= x2 }
    rangeQuery(tree)(cube)(inside)
  }


  // Circular range query

  // Hyper-sphere definition from center and ray
  type Center = Point
  type Ray = Double
  type Sphere = (Center,Ray)

  def circularQuery(tree: KDTree)(distfn: DistFn)(sphere: Sphere): Stream[Vec] = {
    val (center,ray) = sphere
    val (lower,upper) = (center.map(_ - ray),center.map(_ + ray))
    val cube = (lower,upper)
    def inside(pt: Vec) = distfn(pt,center) <= ray
    rangeQuery(tree)(cube)(inside)
  }


  // K-nearest neighbours query

  type Distance = Double
  def nnQuery(tree: KDTree)(distfn: DistFn)(k: Int)(target: Vec): Seq[(Vec,Distance)] = {
    import KHeap._
    // Ordering of neighbours
    implicit val order = new Ordering[(Vec,Distance)] {
      def compare(o1: (Vec,Distance), o2: (Vec,Distance)) =
        if(o1._2 - o2._2 < 0) -1
        else if(o1._2 - o2._2 > 0) 1
        else 0
    }

    def loop(node: KDTree, heap: KHeap[(Vec,Distance)]): KHeap[(Vec,Distance)] = node match {
      case Empty => heap
      case KDNode(v,dim,less,more) =>
        val distance = distfn(v,target)
        val sz = size(heap)
        val (nearer,farther) = if(v(dim) < target(dim)) (less,more) else (more,less)
        val newheap = loop(nearer, insert(k)(heap)(v,distance))
        if(distance < head(newheap)._2) loop(farther, newheap)
        else newheap
    }

    // Streaming the neighbours found
    toStream(loop(tree,empty[(Vec,Distance)]))
  }

}
