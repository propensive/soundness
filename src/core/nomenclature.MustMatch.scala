package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*

object MustMatch extends Rule
   ({ text => m"must match $text" }, { (text, param) => text.s.matches(param.s) })

erased trait MustMatch[TextType <: Label] extends Check[TextType]
