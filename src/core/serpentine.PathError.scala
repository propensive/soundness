package serpentine

import anticipation.*
import fulminate.*
import gossamer.*
import vacuous.*

import scala.compiletime.*

object PathError:
  object Reason:
    given Reason is Communicable =
      case Reason.RootParent  => m"the root has no parent"
      case Reason.InvalidRoot => m"the root is not valid"

  enum Reason:
    case RootParent
    case InvalidRoot

case class PathError(reason: PathError.Reason, path: Optional[Text] = Unset)(using Diagnostics)
extends Error(m"the path ${path.lay(t"")(_+t" ")}was invalid because $reason")
