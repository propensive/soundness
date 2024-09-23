package geodesy

trait Locatable:
  type Self
  def location(value: Self): Location