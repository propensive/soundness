package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

object MustNotStart extends Rule({ text => m"must not start with $text"}, !_.starts(_))
erased trait MustNotStart[TextType <: Label] extends Check[TextType]
