package vicarious

import fulminate.*
import vacuous.*

import scala.deriving.*
import scala.quoted.*

object Vicarious:
  def make[KeyType: Type](using Quotes): Expr[Proxy[KeyType]] =
    import quotes.reflect.*

    def recompose[LabelsType <: Tuple: Type, ElementsType <: Tuple: Type](result: TypeRepr)
            : TypeRepr =

      Type.of[LabelsType] match
        case '[ EmptyTuple ] =>
          result

        case '[ type tail <: Tuple; headLabel *: tailLabels ] =>
          Type.of[ElementsType] match
            case '[ type tailElement <: Tuple; headElement *: tailElements ] =>
              TypeRepr.of[headLabel] match
                case ConstantType(StringConstant(label)) =>
                  val refinement = Refinement(result, label, proxy[headElement])
                  recompose[tailLabels, tailElements](refinement)


    def proxy[KeyType: Type]: TypeRepr =
      Expr.summon[Mirror.ProductOf[KeyType]] match
        case Some('{ type labels <: Tuple
                     type types <: Tuple
                     $mirror: Mirror.Product
                       { type MirroredElemLabels = labels
                         type MirroredElemTypes = types } }) =>
          recompose[labels, types](TypeRepr.of[Proxy[KeyType]])

        case _ =>
          TypeRepr.of[Proxy[KeyType]]

    proxy[KeyType].asType match
      case '[ type resultType <: Proxy[KeyType]; resultType ] =>
        '{Proxy[KeyType](Unset).asInstanceOf[resultType]}
