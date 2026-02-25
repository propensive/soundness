package gossamer

import anticipation.*
import fulminate.*
import spectacular.*

import decimalFormatters.java

object BoundsError:
  def range(minimum: Double, maximum: Double): Text = minimum match
    case Double.MinValue =>
      maximum.match
        case Double.MaxValue => "not a valid number"
        case maximum         => t"? ≤ $maximum"

    case minimum =>
      maximum match
        case Double.MaxValue => t"$minimum ≤ ?"
        case maximum         => t"$minimum ≤ ? ≤ $maximum"

case class BoundsError(value: Double, minimum: Double, maximum: Double)(using Diagnostics)
extends Error(m"the value ${value.show} is not in the range ${BoundsError.range(minimum, maximum)}")
