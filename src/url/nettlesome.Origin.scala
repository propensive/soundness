package nettlesome

import proscenium.*
import vacuous.*

case class Origin[+SchemeType <: Label](scheme: Scheme[SchemeType], authority: Optional[Authority])
