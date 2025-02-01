package anticipation

trait Generalizable:
  type Self
  type Result
  type Domain
  def generalization(value: Self): Result
  extension (value: Self) def generalize: Result = generalization(value)
