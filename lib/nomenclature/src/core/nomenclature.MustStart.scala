package nomenclature

import anticipation.*
import fulminate.*
import gossamer.*
import proscenium.*

object MustStart extends Rule({ text => m"must start with $text" }, _.starts(_))

erased trait MustStart[TextType <: Label] extends Check[TextType]
