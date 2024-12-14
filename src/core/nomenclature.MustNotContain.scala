package nomenclature

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*

object MustNotContain extends Rule({ text => m"must not contain $text"}, !_.contains(_))
erased trait MustNotContain[TextType <: Label] extends Check[TextType]
