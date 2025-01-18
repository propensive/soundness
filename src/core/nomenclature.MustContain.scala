package nomenclature

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*

object MustContain extends Rule({ text => m"must contain $text" }, _.contains(_))

erased trait MustContain[TextType <: Label] extends Check[TextType]
