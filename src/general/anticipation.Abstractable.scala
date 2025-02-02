package anticipation

trait Abstractable:
  type Self
  type Result
  type Domain
  def generalization(value: Self): Result
  extension (value: Self) def abstraction: Result = generalization(value)
