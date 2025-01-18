package nomenclature

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*

object MustNotEnd extends Rule({ text => m"must not end with $text"}, !_.ends(_))

erased trait MustNotEnd[TextType <: Label] extends Check[TextType]
