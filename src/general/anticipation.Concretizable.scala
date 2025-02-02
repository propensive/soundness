package anticipation

trait Concretizable:
  type Self
  type Source
  type Domain
  def specialization(source: Source): Self
  extension (source: Source) def concretize: Self = specialization(source)
