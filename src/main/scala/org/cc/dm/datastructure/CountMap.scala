package org.cc.dm.datastructure

// An immutable Map to count keys
case class CountMap[K](private val map: Map[K,Int])

object CountMap {

  import collection.immutable.Seq

  def empty[K] = CountMap[K](Map())

  def from[K](keys: K*): CountMap[K] =
    keys.foldLeft(empty[K]) { case (acc,elt) => increment(acc)(elt) }


  def isEmpty[K](cm: CountMap[K]) =
    cm.map.isEmpty

  def increment[K](cm: CountMap[K])(key: K) =
     CountMap(cm.map + (key->(cm.map.getOrElse(key,0)+1)))

  // We don't allow decrementing under 0
  def decrement[K](cm: CountMap[K])(key: K) =
    CountMap(cm.map + (key->(math.max(0, cm.map.getOrElse(key,0) - 1))))

  // Merges 2 CountMap
  def merge2[K](cm1: CountMap[K], cm2: CountMap[K]): CountMap[K] =
    if(cm1.map.size > cm2.map.size)
      merge2(cm2,cm1)
    else
      CountMap(cm1.map.foldLeft(cm2.map) { case (merged,(k,v)) => merged + (k -> (merged.getOrElse(k,0) + v))})

  def merge[K](cms: Seq[CountMap[K]]) = cms.foldLeft(empty[K])(merge2)

  def keys[K](cm: CountMap[K]): Seq[K] =
    cm.map.keys.toIndexedSeq

  def total[K](cm: CountMap[K]) =
    cm.map.values.sum

  def count[K](cm: CountMap[K])(key: K) =
    cm.map.getOrElse(key,0)

  // Frequency of the key
  def frequency[K](cm: CountMap[K])(key: K): Option[Double] = {
    val cnt = count(cm)(key)
    if(cnt==0) None
    else Some(count(cm)(key) / total(cm).toDouble)
  }

  // All frequencies per keys
  def frequencies[K](cm: CountMap[K]): Seq[(K,Double)] = {
    val sum = total(cm).toDouble
    cm.map.map { case (key,cnt) => (key -> cnt / sum) }.toIndexedSeq
  }


  // Find the most frequent key
  def max[K](cm: CountMap[K]): Option[(K,Int)] =
    if(isEmpty(cm)) None
    else Some(cm.map.maxBy(_._2))

  // Find the least frequent key which is registerd in the CountMap
  def min[K](cm: CountMap[K]): Option[(K,Int)] =
    if(isEmpty(cm)) None
    else Some(cm.map.maxBy(_._2))

  def filter[K](cm: CountMap[K])(p: ((K,Int)) => Boolean) =
    CountMap(cm.map.filter(p))

  def map[K,T](cm: CountMap[K])(f: ((K,Int)) => (T,Int)) =
    CountMap(cm.map.map(f))

  def toMap[K](cm: CountMap[K]): Map[K,Int] = cm.map

}
