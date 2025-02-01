package anticipation

trait Specializable:
  type Self
  type Source
  type Domain
  def specialization(source: Source): Self
  extension (source: Source) def specialize: Self = specialization(source)
