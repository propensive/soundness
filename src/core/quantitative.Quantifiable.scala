package quantitative

trait Quantifiable[QuantityType, UnitsType <: Units[?, ?]]:
  extension (value: QuantityType) def quantify: Quantity[UnitsType]
