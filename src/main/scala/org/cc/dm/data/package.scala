package org.cc.dm

package object data {

  import collection.immutable.Seq

  // Vectors - Unsupervised / Supervised

  trait Vec {
    def apply(i: Int): Any
    def toSeq: Seq[Any]
    // Provide the size of the vector
    def size = toSeq.size
  }

  // Unsupervised vector
  trait UVec extends Vec
  // Supervised vector
  trait SVec[C] extends Vec { def sclass: C }


  // Real vector - all components are real
  trait RealVec extends Vec {
    def apply(i: Int): Double
    def toSeq: Seq[Double]
  }

  // Unsupervised real-valued vector
  trait URealVec extends RealVec with UVec
  private case class UVecImpl(vector: Seq[Double]) extends URealVec {
    def apply(i: Int) = vector(i)
    def toSeq = vector
  }

  // Supervised real-valued vector
  trait SRealVec[C] extends RealVec with SVec[C] {
    def sclass: C
  }
  private case class SVecImpl[C](vector: Seq[Double], sclass: C) extends SRealVec[C] with URealVec {
    def apply(i: Int) = vector(i)
    def toSeq = vector
  }


  // Building vectors

  // Unsupervised real-valued vector
  def vec(s: Seq[Double]): URealVec = new UVecImpl(s)

  // Supervised real-valued vector
  def vec[C](s: Seq[Double], cl: C): SRealVec[C] = new SVecImpl(s,cl)


  // Implicit conversion

  implicit def realVec2Seq(vec: RealVec): Seq[Double] = vec.toSeq
  implicit def seq2URealVec(s: Seq[Double]): URealVec = vec(s)


  object Vector {

    // Creates an empty real-valued vector with the given dimension
    def empty(dim: Int): RealVec = Seq.fill(dim)(0.0)

    // Dot product
    def dot(v1: RealVec, v2: RealVec) =
      (v1 zip v2).map { case (x,y) => x * y }.sum

    def add(v1: RealVec, v2: RealVec): RealVec =
      (v1 zip v2).map { case (x,y) => x + y }

    def add(vectors: Seq[RealVec]): RealVec =
      vectors.reduceOption(add).getOrElse(empty(0))

  }


  // Distances

  type Distance = Double
  type DistFn = (RealVec,RealVec) => Distance

  object Distance {

    // Find the closest candidate according to the provided distance function
    // Returns th index of the closest in the original sequence, the closest vector and the min distance computed
    def closest(distfn: DistFn)(candidates: Seq[RealVec])(vec: RealVec): (Int,RealVec,Distance) =
      candidates.zipWithIndex.map { case (v,index) => (index,v,distfn(v,vec)) }.minBy(_._3)

  }



  // Datasets - Unsupervised / Supervised

  trait Dataset[+V <: Vec] {
    def apply(i: Int): V
    def toSeq: Seq[V]       // conversion to immutable sequence MUST be fast
    def size = toSeq.size
  }

  trait UDataset[+T <: UVec] extends Dataset[T]
  type UnsupervisedDataset = UDataset[UVec]

  trait SDataset[C,+T <: SVec[C]] extends Dataset[T]
  type SupervisedDataset[C] = SDataset[C,SVec[C]]


  // Dataset of real-valued
  trait RealDataset extends Dataset[RealVec] {
    def apply(i: Int): RealVec
    def toSeq: Seq[RealVec]
  }

  trait URealDataset extends RealDataset with UDataset[URealVec]
  trait SRealDataset[C] extends RealDataset with SDataset[C,SRealVec[C]]

  implicit def dataset2Seq[T <: Vec](ds: Dataset[T]): Seq[T] = ds.toSeq
  implicit def seqToDataset[T <: Vec](seq: Seq[T]): Dataset[T] = new Dataset[T] {
    def apply(i: Int): T = seq(i)
    def toSeq: Seq[T] = seq
  }
  implicit def seqToRealDataset(seq: Seq[RealVec]): RealDataset = new RealDataset {
    def apply(i: Int): RealVec = seq(i)
    def toSeq: Seq[RealVec] = seq
  }


  object Dataset {

    def empty: RealDataset = Seq[RealVec]()

    def dimension[T <: Vec](ds: Dataset[T]) =
      ds.toSeq.headOption.map(_.size).getOrElse(0)

    def size[T <: Vec](ds: Dataset[T]) = ds.size

    def isEmpty[T <: Vec](ds: Dataset[T]) = size(ds) == 0

    // Extract safely a given subset from a dataset
    def subset(ds: RealDataset, min: Int, max: Int): RealDataset =
      (math.max(0,min) to math.min(max,size(ds)-1)).map(ds(_))

    // Splitting a dataset

    def split2[T <: Vec](ds: Dataset[T]): (Dataset[T],Dataset[T]) = ??? // ds.splitAt(ds.size / 2)
    def splitn[T <: Vec](ds: Dataset[T])(n: Int): Seq[Dataset[T]] = ???

    // Non-deterministic
    // TODO functional style random generator
    def sample[T <: Vec](ds: Dataset[T])(n: Int): Dataset[T] = ???
    def sample[T <: Vec](ds: Dataset[T])(percentage: Double): Dataset[T] = ???
    def boostrap[T <: Vec](ds: Dataset[T])(n: Int): Stream[Dataset[T]] = ???
  }

}
