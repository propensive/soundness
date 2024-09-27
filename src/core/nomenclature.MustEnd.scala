package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

object MustEnd extends Rule({ text => m"must end with $text"}, _.ends(_))
erased trait MustEnd[TextType <: Label] extends Check[TextType]
