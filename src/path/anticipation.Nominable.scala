package anticipation

trait Nominable:
  type Self
  def name(value: Self): Text
