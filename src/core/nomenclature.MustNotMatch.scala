package nomenclature

import anticipation.*
import fulminate.*
import rudiments.*

object MustNotMatch extends Rule
   ({ text => m"must not match $text" }, { (text, param) => !text.s.matches(param.s) })

erased trait MustNotMatch[TextType <: Label] extends Check[TextType]
