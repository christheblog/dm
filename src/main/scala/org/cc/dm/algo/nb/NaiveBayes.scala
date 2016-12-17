package org.cc.dm.algo.nb

// Naive Bayes implementation
object NaiveBayes {

  import collection.immutable.Seq
  import org.cc.dm.data._
  import org.cc.dm.datastructure.CountMap

  // An attribute value is given by a dimension and a value
  type Attribute = Int
  type AttributeValue = (Attribute,Any)
  type Probability = Double
  type Frequency = Double


  // Naive bayes model, keeping data stats to allow
  // Bayes' Rule : P(A|B) = P(B|A)P(A) / P(B)
  // P(A|B) : Posterior
  // P(B|A) : Likelihood
  // P(A) : prior
  // P(B) : marginal likelihood - not required when computing
  trait NaiveBayesModel[C] {
    def classes: Seq[C]
    def size: Int
    def prior(cl: C): Double
    def likelihood(attr: AttributeValue,cl: C): Double
    def marginalLikelihood(attr: AttributeValue): Double
  }

  def model[C](ds: SupervisedDataset[C]): NaiveBayesModel[C] =
    ds.foldLeft(new NaiveBayesModelImpl[C]()) { case (model,vec) => model.add(vec) }

  // Classification is estimating the most probable class
  def classify[C](model: NaiveBayesModel[C])(vec: Vec): Option[C] = {
    model.classes.map { cl =>
      val prior = model.prior(cl)
      // Note : this is NOT a real proba as we don't divide by the marginal likelihood
      // But we don't need compute a real proba as we are interested in only in the MAP class
      val proba = vec.toSeq.zipWithIndex.map { case (attr, index) => model.likelihood((index + 1, attr),cl) * prior }.product
      (cl, proba)
    }
    // we could use maxBy, but this protect us against an empty model that contains no classes
    .sortBy(-_._2)
    .headOption.map(_._1)
  }




  // The model can be incrementally enriched.
  // Note : Computes value using m-estimate - See : Machine Learning, Tom M. Mitchell, p179
  // m-estimate = (nc + m * p) / (n + m). We choose m*p such as : m*p = 1
  private case class NaiveBayesModelImpl[C](attributes: CountMap[AttributeValue] = CountMap.empty,
                                            attributesPerClass: CountMap[(C,AttributeValue)] = CountMap.empty[(C,AttributeValue)],
                                            classCount: CountMap[C] = CountMap.empty[C]) extends NaiveBayesModel[C] {
    // Adding new supervised vector's data to the model
    def add(vec: SVec[C]): NaiveBayesModelImpl[C] = {
      NaiveBayesModelImpl(
        vec.toSeq.zipWithIndex.foldLeft(attributes) { case (cm,(attr,index)) => CountMap.increment(cm)((index+1,attr)) },
        vec.toSeq.zipWithIndex.foldLeft(attributesPerClass) { case (cm,(attr,index)) => CountMap.increment(cm)((vec.sclass,(index+1,attr))) },
        CountMap.increment(classCount)(vec.sclass))
    }

    // NaiveBayesModel implementation

    def classes = CountMap.keys(classCount)
    def size = CountMap.total(classCount)

    def prior(cl: C): Double = {
      val sz = size.toDouble
      if(sz==0) 0.0
      else CountMap.count(classCount)(cl) / classes.size
    }

    def likelihood(attrValue: AttributeValue,cl: C): Double = {
      val clCount = CountMap.count(classCount)(cl).toDouble
      // We are estimating probability here using m-estimate
      val (attr,_) = attrValue
      // number of possible values for a given attribute
      val distinctValuesForAttribute = (CountMap.keys(attributes)).count { case (a,_) => a == attr }
      (CountMap.count(attributesPerClass)((cl, attrValue)) + 1 ) / (clCount + distinctValuesForAttribute)
    }

    def marginalLikelihood(attr: AttributeValue): Double =
      CountMap.count(attributes)(attr) / size
  }


}
