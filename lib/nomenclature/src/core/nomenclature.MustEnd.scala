package nomenclature

import anticipation.*
import fulminate.*
import gossamer.*
import proscenium.*

object MustEnd extends Rule({ text => m"must end with $text"}, _.ends(_))

erased trait MustEnd[TextType <: Label] extends Check[TextType]
