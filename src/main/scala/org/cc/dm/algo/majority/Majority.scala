package org.cc.dm.algo.majority

// Majority classifier
// Useful as benchmark - or on processed parts of a dataset for instance
object Majority {

  import org.cc.dm.data._

  type MajorityModel[C] = Option[C]

  // Computing the majority model is just finding out the most frequent class out of the dataset
  def model[C](ds: SupervisedDataset[C]): Option[C] = {
    ds.groupBy(_.sclass).mapValues(_.size).toList
      .sortBy(- _._2).headOption.map(_._1) // Extracting directly the majority class
  }

  // Classifying is just returning the most frequent class without any other processing
  def classify[C](model: MajorityModel[C])(target: RealVec): Option[C] = model

}
