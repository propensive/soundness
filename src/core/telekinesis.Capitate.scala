package telekinesis

import anticipation.*
import hieroglyph.*
import gesticulate.*
import prepositional.*
import proscenium.*

object Capitate:
  given contentEncoding: [EncodingType <: Encoding]
  =>    ("contentEncoding" is Capitate of EncodingType) = _.name

  given accept: ("accept" is Capitate of MediaType) = _.basic

trait Capitate:
  type Self <: Label
  type Subject
  def encode(value: Subject): Text
