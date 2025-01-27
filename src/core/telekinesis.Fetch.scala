package telekinesis

import language.dynamics

import anticipation.*
import nettlesome.*
import proscenium.*

case class Fetch(url: HttpUrl) extends Dynamic:
  inline def applyDynamicNamed
     (id: "apply")
     (inline headers: (Label, Any)*)
     (using online: Online, loggable: HttpEvent is Loggable, postable: Postable[Unit])
  :     HttpResponse =

    ${Telekinesis.fetch('this, 'headers, 'online, 'loggable)}
