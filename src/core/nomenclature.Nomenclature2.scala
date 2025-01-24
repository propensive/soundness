package nomenclature

import anticipation.*
import fulminate.*
import rudiments.*

import scala.quoted.*
import scala.compiletime.*

given Realm = realm"nomenclature"

object Nomenclature2:
  def build(using Quotes)(todo: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    todo match
      case Nil          => TypeRepr.of[EmptyTuple]
      case next :: todo => next.asType.absolve match
        case '[next] => build(todo).asType.absolve match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]

  def decompose(using Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)

  def disintersection[IntersectionType: Type](using Quotes): Expr[Tuple] =
    import quotes.reflect.*

    build(decompose(TypeRepr.of[IntersectionType]).to(List)).asType.absolve match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

  def extractor(context: Expr[StringContext])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val string = context.valueOrAbort.parts.head

    ConstantType(StringConstant(string)).asType match
      case '[type stringType <: Label; stringType] => '{NameExtractor[stringType]()}
      case _ =>
        panic(m"StringContext did not contains Strings")

  def parse2[PlatformType: Type, NameType <: String: Type](scrutinee: Expr[Name[PlatformType]])
     (using Quotes)
  :     Expr[Boolean] =
    parse[PlatformType, NameType]
    '{${Expr(constant[NameType])}.tt == $scrutinee.text}

  def constant[TextType <: String: Type](using Quotes): TextType =
    import quotes.reflect.*
    TypeRepr.of[TextType].asMatchable.absolve match
      case ConstantType(StringConstant(value)) => value.tt.asInstanceOf[TextType]

  def companion[CompanionType: Typeable](using Quotes)(symbol: quotes.reflect.Symbol)
  :     CompanionType =
    Class.forName(s"${symbol.companionModule.fullName}$$").nn.getField("MODULE$").nn.get(null) match
      case module: CompanionType => module
      case _                     => halt(m"The companion object did not have the expected type.")

  def parse[PlatformType: Type, NameType <: String: Type](using Quotes): Expr[Name[PlatformType]] =
    import quotes.reflect.*

    val name: Text = constant[NameType].tt

    Expr.summon[PlatformType is Nominative] match
      case Some('{ type constraintType
                   type nominativeType <: Nominative { type Constraint = constraintType }
                   $value: nominativeType }) =>
        decompose(TypeRepr.of[constraintType]).to(List).each: repr =>
          val text = repr.asMatchable match
            case AppliedType(_, List(param)) => param.asMatchable match
              case ConstantType(StringConstant(text)) => text.tt
              case _ => halt(m"Bad type")
            case _                                                        => halt(m"Bad type")
          val rule = companion[Rule](repr.typeSymbol)
          if !rule.check(name, text)
          then halt(m"the name is not valid because it ${rule.describe(text)}")
      case _ =>
        halt(m"Could not access constraint")


    '{${Expr(name)}.asInstanceOf[Name[PlatformType]]}
