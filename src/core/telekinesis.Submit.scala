package telekinesis

import language.dynamics

import scala.quoted.*

import anticipation.*
import nettlesome.*
import proscenium.*

case class Submit(url: HttpUrl) extends Dynamic:
  inline def applyDynamicNamed[PayloadType]
     (id: "apply")
     (inline headers: (Label, Any)*)
     (payload: PayloadType)
     (using online: Online, loggable: HttpEvent is Loggable, postable: Postable[PayloadType])
  :     HttpResponse =

    ${Telekinesis.submit[PayloadType]('this, 'headers, 'online, 'loggable, 'payload, 'postable)}

  def apply[PayloadType: Postable](method: HttpMethod = Post)(payload: PayloadType)(using Online)
  :     HttpResponse logs HttpEvent =

    Http.request(url, payload, method, Nil)
