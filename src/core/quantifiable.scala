package quantify

trait Quantifiable[QuantityType, UnitsType <: Units[?, ?]]:
  def quantify(value: QuantityType): Quantity[UnitsType]

extension [QuantityType, UnitsType <: Units[?, ?]]
    (value: QuantityType)
    (using quantifiable: Quantifiable[QuantityType, UnitsType])
  def quantify: Quantity[UnitsType] = quantifiable.quantify(value)