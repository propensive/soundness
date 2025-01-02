package vicarious

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object Vicarious:
  def proxy[KeyType: Type, ValueType: Type](using Quotes): Expr[Proxy[KeyType, ValueType]] =
    import quotes.reflect.*

    def recompose[LabelsType <: Tuple: Type, ElementsType <: Tuple: Type](result: TypeRepr)
            : TypeRepr =

      (Type.of[LabelsType]: @unchecked) match
        case '[ EmptyTuple ] =>
          result

        case '[ type headLabel <: String; type tailLabels <: Tuple; headLabel *: tailLabels ] =>
          (Type.of[ElementsType]: @unchecked) match
            case '[ type tailElement <: Tuple; headElement *: tailElements ] =>
              (TypeRepr.of[headLabel].asMatchable: @unchecked) match
                case ConstantType(StringConstant(label)) =>
                  recompose[tailLabels, tailElements](Refinement(result, label, proxy[headElement]))

    def proxy[KeyType2: Type]: TypeRepr = Expr.summon[Mirror.ProductOf[KeyType2]] match
      case Some('{ type labels <: Tuple
                   type types <: Tuple
                   $mirror: Mirror.Product
                             { type MirroredElemLabels = labels
                               type MirroredElemTypes = types } }) =>
        recompose[labels, types](TypeRepr.of[Proxy[KeyType, ValueType]])

      case _ =>
        TypeRepr.of[ValueType]

    (proxy[KeyType].asType: @unchecked) match
      case '[ type resultType <: Proxy[KeyType, ValueType]; resultType ] =>
        '{Proxy().asInstanceOf[resultType]}

  // This is a lazy copy of the method above. The two should be combined into one.
  def matchProxy[KeyType: Type, ValueType: Type](using Quotes): Expr[MatchProxy[KeyType, ValueType]] =
    import quotes.reflect.*

    def recompose[LabelsType <: Tuple: Type, ElementsType <: Tuple: Type](result: TypeRepr)
            : TypeRepr =

      Type.of[LabelsType] match
        case '[ type headLabel <: String; type tailLabels <: Tuple; headLabel *: tailLabels ] =>
          (Type.of[ElementsType]: @unchecked) match
            case '[ type tailElements <: Tuple; headElement *: tailElements ] =>
              (TypeRepr.of[headLabel].asMatchable: @unchecked) match
                case ConstantType(StringConstant(label)) =>
                  recompose[tailLabels, tailElements](Refinement(result, label, proxy[headElement]))

        case _ =>
          result

    def proxy[KeyType2: Type]: TypeRepr = Expr.summon[Mirror.ProductOf[KeyType2]] match
      case Some('{ type labels <: Tuple
                   type types <: Tuple
                   $mirror: Mirror.Product
                             { type MirroredElemLabels = labels
                               type MirroredElemTypes = types } }) =>
        recompose[labels, types](TypeRepr.of[MatchProxy[KeyType, ValueType]])

      case _ =>
        TypeRepr.of[ValueType]

    (proxy[KeyType].asType: @unchecked) match
      case '[ type resultType <: MatchProxy[KeyType, ValueType]; resultType ] =>
        '{MatchProxy().asInstanceOf[resultType]}
