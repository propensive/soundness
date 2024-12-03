package cosmopolite

import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*

object Locale:
  given [LanguageType] => Locale[LanguageType] is Encodable in Text = _.language.code

  given Decoder[Locale[en & pl]] =
    case t"pl" => Locale(pl)
    case _     => Locale(en)

case class Locale[-LanguageType](language: Language)
