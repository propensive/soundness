package anticipation

trait Concretizable:
  type Self
  type Source
  type Domain
  def apply(source: Source): Self
