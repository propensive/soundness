package nomenclature

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*
import spectacular.*

import scala.compiletime.*

object Nomenclature:
  opaque type Name[-PlatformType] = Text

  object Name:
    given [PlatformType] => Name[PlatformType] is Communicable = name => Message(name.text)
    given [PlatformType] => Name[PlatformType] is Encodable in Text = _.text

    inline given [PlatformType](using PlatformType is Nominative, Tactic[NameError])
        => Decoder[Name[PlatformType]] =
      apply(_)

    private inline def check[CheckType <: Matchable](name: Text): Unit raises NameError =
      inline erasedValue[CheckType] match
        case _: EmptyTuple     => ()
        case _: (head *: tail) => inline erasedValue[head & Matchable] match
          case _: Check[param] =>
            inline staticCompanion[head] match
              case rule: Rule =>
                if !rule.check(name, constValue[param].tt)
                then raise(NameError(name, rule, constValue[param].tt))

              case other =>
                error("The companion object was not a subtype of Rule")

            check[tail](name)

    inline def apply[PlatformType](name: Text)(using nominative: PlatformType is Nominative)
            : Name[PlatformType] raises NameError =

      inline disintersect[nominative.Constraint] match
        case v => check[v.type](name)

      name.asInstanceOf[Name[PlatformType]]

    given [PlatformType] => Name[PlatformType] is Showable = identity(_)

  extension [RulesType](name: Name[RulesType]) def text: Text = name
