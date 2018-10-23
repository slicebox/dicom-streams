package se.nimsa.dicom.data

case class Multiplicity(min: Int, max: Option[Int]) {
  def isSingle: Boolean = this == Multiplicity.single
  def isMultiple: Boolean = !isSingle
  def isBounded: Boolean = max.isDefined
  def isUnbounded: Boolean = !isBounded
}

object Multiplicity {
  def fixed(value: Int) = Multiplicity(value, Some(value))
  def bounded(min: Int, max: Int) = Multiplicity(min, Some(max))
  def unbounded(min: Int) = Multiplicity(min, None)
  final val single: Multiplicity = fixed(1)
  final val oneToMany: Multiplicity = unbounded(1)
}

