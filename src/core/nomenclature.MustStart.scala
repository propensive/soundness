package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

object MustStart extends Rule({ text => m"must start with $text" }, _.starts(_))
erased trait MustStart[TextType <: Label] extends Check[TextType]
