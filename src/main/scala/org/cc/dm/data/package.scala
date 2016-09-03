package org.cc.dm

package object data {

  import collection.immutable.Seq

  // Vectors and Dataset - Unsupervised / Supervised

  trait Vec {
    def apply(i: Int): Double
    def toSeq: Seq[Double]
    // provide the size of the vector
    def size = toSeq.size
  }

  trait UVec extends Vec
  trait SVec[C] extends Vec {
    def sclass: C
  }

  type Dataset = Seq[Vec]
  type UDataset = Dataset
  type SDataset[C] = Seq[SVec[C]]

  type DistFn = (Vec,Vec) => Double


  // Building vectors

  // Unsupervised vector
  def vec(s: Seq[Double]) = new UVec {
    def apply(i: Int) = s(i)
    def toSeq = s
  }

  // Supervised vector
  def vec[C](s: Seq[Double], cl: C) = new SVec[C] {
    def apply(i: Int) = s(i)
    def toSeq = s
    def sclass = cl
  }


  // Implicit conversion

  implicit def vec2Seq(vec: Vec): Seq[Double] = vec.toSeq
  implicit def seq2UVec(s: Seq[Double]): UVec = vec(s)

}
