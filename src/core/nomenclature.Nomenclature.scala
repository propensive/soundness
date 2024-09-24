package nomenclature

import anticipation.*
import prepositional.*
import rudiments.*
import spectacular.*

import scala.quoted.*

object Nomenclature:
  transparent trait WithPlatform:
    type Platform
  
  private inline def platform[Platform](text: Text): Name on Platform =
    text.asInstanceOf[Name on Platform]

  opaque type Name <: WithPlatform = Text & WithPlatform

  object Name:
    def apply[PlatformType: Nominative](name: Text): Name on PlatformType =
      PlatformType.validate(name)
      platform[PlatformType](name)

    given Name is Showable = identity(_)

  def parse[PlatformType: Type](context: Expr[StringContext], nominative: Expr[PlatformType is Nominative])(using Quotes)
          : Expr[Name on PlatformType] =
    val text: Text = context.valueOrAbort.parts.head.tt
    '{???}
