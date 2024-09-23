package geodesy

trait Bearing[BearingType]:
  def from(radians: Radians): BearingType
