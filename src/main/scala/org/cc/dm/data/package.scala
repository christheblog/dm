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

  // Unsupervised
  trait UVec extends Vec
  private case class UVecImpl(vector: Seq[Double]) extends UVec {
    def apply(i: Int) = vector(i)
    def toSeq = vector
  }

  // Supervised
  trait SVec[C] extends Vec {
    def sclass: C
  }
  private case class SVecImpl[C](vector: Seq[Double],sclass: C) extends SVec[C] with UVec {
    def apply(i: Int) = vector(i)
    def toSeq = vector
  }

  type Dataset = Seq[Vec]
  type UDataset = Dataset
  type SDataset[C] = Seq[SVec[C]]

  type DistFn = (Vec,Vec) => Double


  // Building vectors

  // Unsupervised vector
  def vec(s: Seq[Double]): UVec = new UVecImpl(s)

  // Supervised vector
  def vec[C](s: Seq[Double], cl: C): SVec[C] = new SVecImpl(s,cl)


  // Implicit conversion

  implicit def vec2Seq(vec: Vec): Seq[Double] = vec.toSeq
  implicit def seq2UVec(s: Seq[Double]): UVec = vec(s)

}
