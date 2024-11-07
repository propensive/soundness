package scintillate

import anticipation.*
import rudiments.*
import prepositional.*
import vacuous.*
import turbulence.*

object Part:
  given Part is Readable by Bytes as readable = _.body

case class Part
    (disposition: Multipart.Disposition,
     headers:     Map[Text, Text],
     name:        Optional[Text],
     filename:    Optional[Text],
     body:        LazyList[Bytes])
