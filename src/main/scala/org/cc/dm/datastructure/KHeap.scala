package org.cc.dm.datastructure

// Heap retaining at most k objects
// Use of natural ordering will keep the k smallest elements in a Max-Heap
// Useful in a KNN context to keep the K nearest neighbours according to some distance ordering
object KHeap {

  // We keep the cached size to avoid log(n) recomputation each time we need to decide if we insert or not
  sealed trait KHeap[+T] { def size(): Int }
  private case object KHeapEmpty extends KHeap[Nothing] { val size = 0 }
  private case class KHeapNode[T : Ordering](value: T, left: KHeap[T], right: KHeap[T]) extends KHeap[T] {
    val size = 1 + left.size + right.size
  }


  def empty[T : Ordering]: KHeap[T] = KHeapEmpty

  def from[T : Ordering](k: Int)(xs: T*) = {
    val insertk = insert[T](k) _
    xs.foldLeft(empty[T]) { case (heap,x) => insertk(heap)(x) }
  }

  def size[T](heap: KHeap[T]) = heap.size()

  def isEmpty[T](heap: KHeap[T]) = size(heap)==0

  def headOption[T : Ordering](heap: KHeap[T]): Option[T] = heap match {
    case KHeapEmpty => None
    case KHeapNode(value,_,_) => Some(value)
  }

  def head[T : Ordering](heap: KHeap[T]): T = headOption(heap) match {
    case None => throw new RuntimeException("Cannot access head of an empty KHeap")
    case Some(x) => x
  }

  def tail[T : Ordering](heap: KHeap[T]): KHeap[T] = heap match {
    case KHeapEmpty => throw new RuntimeException("Cannot access tail of an empty KHeap")
    case KHeapNode(_,left,right) => merge(left,right)
  }

  // Extract the first element of a Heap
  def take[T : Ordering](heap: KHeap[T]): (KHeap[T],T) = heap match {
    case KHeapEmpty => throw new RuntimeException("Cannot take an element from an empty KHeap")
    case KHeapNode(root, left, right) => (merge(left,right),root)
  }

  // Insert an element in the Heap, making sure we don't have more than k elements
  // If the heap is full (contains already k) elements and thts it should be there, nothing is done
  def insert[T : Ordering](k: Int)(heap: KHeap[T])(value: T): KHeap[T] = {
    require( k > 0)
    heap match {
      case KHeapEmpty => insert(heap)(value)
      case n@KHeapNode(_,_,_) if n.size < k  => insert(heap)(value)
      case KHeapNode(root,left,right) =>
        val order = implicitly[Ordering[T]]
        if(order.compare(value,root) > 0) heap
        else restrict(k)(insert(heap)(value))
    }
  }

  // Merge 2 heaps and restrict their size to k
  def merge2[T : Ordering](k: Int)(h1: KHeap[T], h2: KHeap[T]): KHeap[T] =
    restrict(k)(merge(h1,h2))

  // Merge n heaps and restrict their size to k
  def merge[T : Ordering](k: Int)(hs: KHeap[T]*): KHeap[T] =
    hs.foldLeft(empty[T]) { case (res,heap) => merge2(k)(res,heap) }


  // Restrict an existing heap to k elements
  def restrict[T : Ordering](k: Int)(heap: KHeap[T]): KHeap[T] = {
    require( k > 0)
    if (heap.size <= k) heap
    else restrict(k)(take(heap)._1)
  }

  // Extract data from the KHeap into a stream of elements
  def toStream[T : Ordering](heap: KHeap[T]): Stream[T] = heap match {
    case KHeapEmpty => Stream()
    case _ =>
      val (newheap,elt) = take(heap)
      elt #:: toStream(newheap)
  }


  // private helper functions - not restricted to k elements

  // Unconstrained insert
  private def insert[T : Ordering](heap: KHeap[T])(value: T): KHeap[T] = heap match {
    case KHeapEmpty => KHeapNode(value,KHeapEmpty,KHeapEmpty)
    case KHeapNode(root,left,right)  =>
      val order = implicitly[Ordering[T]]
      order.compare(value,root) match {
        case cmp if cmp > 0 =>
          if(left.size < right.size) KHeapNode(value,insert(left)(root),right)
          else KHeapNode(value,left,insert(right)(root))
        case _ => // We balance the tree
          if(left.size < right.size) KHeapNode(root,insert(left)(value),right)
          else KHeapNode(root,left,insert(right)(value))
      }
  }

  // Unconstrained merge
  private def merge[T : Ordering](h1: KHeap[T], h2: KHeap[T]): KHeap[T] = (h1,h2) match {
    case (KHeapEmpty, _) => h2
    case (_, KHeapEmpty) => h1
    case (h1@KHeapNode(v1, l1, r1), h2@KHeapNode(v2, l2, r2)) =>
      val order = implicitly[Ordering[T]]
      if (order.compare(v1, v2) > 0) KHeapNode(v1, merge(l1, r1), h2)
      else KHeapNode(v2, h1, merge(l2, r2))
  }

}


