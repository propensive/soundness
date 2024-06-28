package metamorphose

import fulminate.*

object PermutationError:
  enum Reason:
    case BaseRange(value: Int, base: Int)
    case DuplicateIndex(index: Int, element: Int)
    case InvalidIndex(last: Int, max: Int)
    case TooShort(length: Int, min: Int)

  import Reason.*

  given Reason is Communicable =
    case BaseRange(value, base) =>
      msg"the value $value is too large for its positional base $base"

    case DuplicateIndex(element, index) =>
      msg"the index $element was duplicated at $index"

    case InvalidIndex(index, max) =>
      msg"the index $index appears, but every index should be in the range 0-$max"

    case TooShort(size, min) =>
      msg"the input, of size $size, is too short for the permutation of size $min"

case class PermutationError(reason: PermutationError.Reason)
extends Error(msg"could not construct permutation because $reason")
