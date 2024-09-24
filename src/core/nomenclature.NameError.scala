package nomenclature

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import spectacular.*
import vacuous.*

import scala.compiletime.*

object NameError:
  enum Reason:
    case InvalidChar(char: Char)
    case Empty
    case ForbiddenName(text: Text)
  
  given Reason is Communicable =
    case Reason.InvalidChar(char)   => val desc = char.description.lay(t"") { d => t" ($d)" }
                                       m"the character $char$desc is not valid"
    case Reason.Empty               => m"the name cannot be empty"
    case Reason.ForbiddenName(name) => m"the name $name is forbidden"

case class NameError(name: Text, position: Ordinal, reason: NameError.Reason)(using Diagnostics)
extends Error(m"the name $name is not valid because $reason at ${position.show}")
