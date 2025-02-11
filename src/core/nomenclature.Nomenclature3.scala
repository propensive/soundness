package nomenclature

import scala.quoted.*

object Nomenclature3:
  def staticCompanion[InstanceType: Type](using Quotes): Expr[Matchable] =
    import quotes.reflect.*
    Ident(TypeRepr.of[InstanceType].typeSymbol.companionModule.termRef).asExprOf[Matchable]

transparent inline def staticCompanion[InstanceType]: Matchable =
  ${Nomenclature3.staticCompanion[InstanceType]}
