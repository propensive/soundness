package nomenclature

import anticipation.*
import fulminate.*
import rudiments.*
import contingency.*
import spectacular.*

import scala.quoted.*
import scala.compiletime.*

object Nomenclature:
  opaque type Name[-PlatformType] = Text

  object Name:

    private inline def check[CheckType](name: Text): Unit raises NameError =
      inline erasedValue[CheckType] match
        case _: EmptyTuple     => ()
        case _: (head *: tail) =>
          val checkable = summonInline[head is Checkable]
          if !checkable.check(name) then raise(NameError(name, express[head]))
          check[tail](name)

    inline def apply[PlatformType](name: Text)(using erased nominative: PlatformType is Nominative)
            : Name[PlatformType] raises NameError =
      
      inline disintersect[nominative.Constraint] match
        case v => check[v.type](name)

      name.asInstanceOf[Name[PlatformType]]

    given [PlatformType] => Name[PlatformType] is Showable = identity(_)

  extension [RulesType](name: Name[RulesType]) def text: Text = name

  def parse[PlatformType: Type, RulesType: Type]
      (context: Expr[StringContext])
      (using Quotes)
          : Expr[Name[RulesType]] =
    val text: Text = context.valueOrAbort.parts.head.tt
    val rules = Type.of[RulesType]
    
    '{${Expr(text)}.asInstanceOf[Name[RulesType]]}
