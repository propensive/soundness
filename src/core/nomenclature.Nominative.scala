package nomenclature

import prepositional.*

object Nominative:
  def apply[SelfType, ConstraintType](): SelfType is Nominative under ConstraintType =
    new Nominative:
      type Self = SelfType
      type Constraint = ConstraintType

trait Nominative:
  type Self
  type Constraint
