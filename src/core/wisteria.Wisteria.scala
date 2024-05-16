package wisteria

import rudiments.*
import vacuous.*

import scala.quoted.*

object Wisteria:
  inline def default[ProductType, FieldType](index: Int): Optional[FieldType] =
    ${getDefault[ProductType, FieldType]('index)}

  def getDefault[ProductType: Type, FieldType: Type](index: Expr[Int])(using Quotes)
          : Expr[Optional[FieldType]] =

    import quotes.reflect.*

    val methodName: String = "$lessinit$greater$default$"+(index.valueOrAbort + 1)
    val productSymbol = TypeRepr.of[ProductType].typeSymbol

    productSymbol.companionClass.declaredMethod(methodName).headOption.map: method =>
      Ref(productSymbol.companionModule).select(method)
    .map: selection =>
      TypeRepr.of[ProductType].typeArgs match
        case Nil  => selection
        case args => selection.appliedToTypes(args)
    .map(_.asExprOf[FieldType]).getOrElse('{Unset})
