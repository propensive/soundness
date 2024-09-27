package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*

object MustNotEqual extends Rule({ text => m"must not equal $text"}, _ != _)
erased trait MustNotEqual[TextType <: Label] extends Check[TextType]