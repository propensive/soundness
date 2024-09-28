package serpentine

import anticipation.*
import gossamer.*

enum Case:
  case Sensitive, Lower, Upper, Preserving

  def apply(text: Text): Text = this match
    case Sensitive | Preserving => text
    case Lower                  => text.lower
    case Upper                  => text.upper

  def equal(left: Text, right: Text): Boolean = this match
    case Sensitive => left == right
    case _         => left.lower == right.lower

  def equal(left: List[Text], right: List[Text]): Boolean = left match
    case Nil          => right.isEmpty
    case head :: tail => right match
      case Nil            => false
      case head2 :: tail2 => equal(head, head2) && equal(tail, tail2)