package probably

import rudiments.*

import scala.compiletime.*

object Tolerance:
  erased given CanEqual[Tolerance, Double] = ###
  erased given CanEqual[Double, Tolerance] = ###

case class Tolerance(value: Double, tolerance: Double):
  override def equals(that: Any): Boolean = that.asMatchable match
    case double: Double => value >= (double - tolerance) && value <= (double + tolerance)
    case _              => false

  override def hashCode: Int = value.hashCode

extension (value: Double)
  @targetName("plusOrMinus")
  def +/-(tolerance: Double): Tolerance = Tolerance(value, tolerance)