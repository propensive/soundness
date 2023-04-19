package serpentine

import gossamer.*
import rudiments.*
import deviation.*

import language.experimental.captureChecking

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidName(name: Text)
    case ParentOfRoot

  given Show[Reason] =
    case Reason.InvalidChar(char) => t"the character '$char' cannot appear in a path"
    case Reason.InvalidName(name) => t"the name '$name' is reserved"
    case Reason.ParentOfRoot      => t"the root has no parent"

case class PathError(reason: PathError.Reason) extends Error(err"the path is invalid because $reason")
