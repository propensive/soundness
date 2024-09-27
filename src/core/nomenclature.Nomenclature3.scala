package nomenclature

import scala.quoted.*

object Nomenclature3:
  def staticCompanion[InstanceType: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*
    Ident(TypeRepr.of[InstanceType].typeSymbol.companionModule.termRef).asExpr

transparent inline def staticCompanion[InstanceType] =
  ${Nomenclature3.staticCompanion[InstanceType]}
