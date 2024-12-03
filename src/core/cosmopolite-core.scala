package cosmopolite

import anticipation.*
import gossamer.*

trait en
object en extends Language(t"en"):
  type Code = en

trait pl
object pl extends Language(t"pl"):
  type Code = pl

infix type speaks [ValueType, LanguageType] = Locale[LanguageType] ?=> ValueType
