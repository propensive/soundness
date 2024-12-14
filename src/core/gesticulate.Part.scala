package gesticulate

import anticipation.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Part:
  given Part is Readable by Bytes as readable = _.body

case class Part
   (disposition: Multipart.Disposition,
    headers:     Map[Text, Text],
    name:        Optional[Text],
    filename:    Optional[Text],
    body:        LazyList[Bytes])
