package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*

object MustNotMatch extends Rule
   ({ text => m"must not match $text" }, { (text, param) => !text.s.matches(param.s) })

erased trait MustNotMatch[TextType <: Label] extends Check[TextType]
