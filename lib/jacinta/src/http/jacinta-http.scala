package jacinta

import anticipation.*
import contingency.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import spectacular.*
import telekinesis.*

package postables:
  given jsonIsPostable: (encoder: CharEncoder, printer: JsonPrinter) => Json is Postable =
    Postable(media"application/json"(charset = "UTF-8"), value => Stream(value.show.data))

package servables:
  given jsonIsServable: (encoder: CharEncoder, printer: JsonPrinter) => Json is Servable =
    Servable[Json](_ => media"application/json"(charset = "UTF-8")): value =>
      Stream(value.show.data)
