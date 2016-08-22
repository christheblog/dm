package org.cc.dm

package object data {

  import collection.immutable.Seq

  type Vec = Seq[Double]
  type Dataset = Seq[Vec]
  type DistFn = (Vec,Vec) => Double

}
