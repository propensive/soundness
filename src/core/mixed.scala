package quantify

import rudiments.*

import scala.quoted.*

object TallyQuaques:
  opaque type Tally[UnitsType <: Tuple] = Long

  object Tally:
    def apply[UnitsType <: Tuple](long: Long): Tally[UnitsType] = long

  extension [UnitsType <: Tuple](tally: Tally[UnitsType])
    inline def apply[UnitType[PowerType <: Nat] <: Units[PowerType, ? <: Dimension]]: Int =
      ${QuantifyMacros.get[UnitsType, UnitType[1]]('tally)}

export TallyQuaques.Tally

type TimeMinutes = (Hours[1], Minutes[1])
type TimeSeconds = (Hours[1], Minutes[1], Seconds[1])
