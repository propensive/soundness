package cosmopolite

import rudiments.*

case class Polyglot[+ValueType, LanguageType](values: Map[Language, ValueType]):
  @targetName("or")
  transparent inline infix def | [ValueType2 >: ValueType, LanguageType2]
    (polyglot: Polyglot[ValueType2, LanguageType2])
          : Polyglot[ValueType2, LanguageType & LanguageType2] | ValueType2 =
    compiletime.summonFrom:
      case locale: Locale[LanguageType & LanguageType2] =>
        (values ++ polyglot.values)(locale.language)

      case _ =>
        Polyglot[ValueType2, LanguageType & LanguageType2](values ++ polyglot.values)

  def apply()(using locale: Locale[LanguageType]): ValueType = values(locale.language)
