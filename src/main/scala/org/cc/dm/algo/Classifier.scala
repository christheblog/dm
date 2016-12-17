package org.cc.dm.algo

object Classifier {

  import org.cc.dm.data._

  type Classifier[C] = (Vec => Option[C])
  type Algo[C] = (SDataset[C,SVec[C]] => Classifier[C])
  type ErrorRate = Double

  // Estimate the error rate of the classifier on the given dataset
  def learn[C](algo: Algo[C])(ds: SDataset[C,SVec[C]]): Classifier[C] = algo(ds)

  // Estimate the error rate of the classifier on the given dataset
  def benchmark[C](classifier: Classifier[C])(ds: SDataset[C,SVec[C]]): ErrorRate = {
    val classified = ds.map { svec => (svec, classifier(svec)) }
    val errorRate = classified.count { case (svec,cl) => cl.get!=Some(svec.sclass) } / Dataset.size(ds).toDouble
    errorRate
  }

}
