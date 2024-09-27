package nomenclature

import anticipation.*
import rudiments.*
import fulminate.*

import scala.quoted.*
import scala.compiletime.*

given Realm = realm"nomenclature"

object Nomenclature2:
  def build(using Quotes)(todo: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    todo match
      case Nil          => TypeRepr.of[EmptyTuple]
      case next :: todo => (next.asType: @unchecked) match
        case '[next] => (build(todo).asType: @unchecked) match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]
    
  def decompose(using Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)
        
  def disintersection[IntersectionType: Type](using Quotes): Expr[Tuple] =
    import quotes.reflect.*

    (build(decompose(TypeRepr.of[IntersectionType]).to(List)).asType: @unchecked) match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

  def extractor(context: Expr[StringContext])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val string = context.valueOrAbort.parts.head

    ConstantType(StringConstant(string)).asType match
      case '[type stringType <: Label; stringType] => '{NExtractor[stringType]()}

  def parse2[PlatformType: Type, NameType <: String: Type](scrutinee: Expr[Name[PlatformType]])
      (using Quotes)
          : Expr[Boolean] =
    import quotes.reflect.*
    '{${Expr(constant[NameType])}.tt == $scrutinee.text}

  def constant[TextType <: String: Type](using Quotes): TextType =
    import quotes.reflect.*
    (TypeRepr.of[TextType].asMatchable: @unchecked) match
      case ConstantType(StringConstant(value)) => value.tt.asInstanceOf[TextType]
    
  def parse[PlatformType: Type, NameType <: String: Type](using Quotes): Expr[Name[PlatformType]] =
    import quotes.reflect.*

    val name: Text = constant[NameType].tt
    
    def assess[RuleType: {Expressible, Checkable}]: Unit =
      if !RuleType.check(name)
      then abandon(m"the name is not valid because it ${RuleType.express()}")

    Expr.summon[PlatformType is Nominative] match
      case Some('{type constraintType; type nominativeType <: Nominative { type Constraint = constraintType }; $value: nominativeType }) =>
        decompose(TypeRepr.of[constraintType]).to(List).each:
          _.asType match
            case '[MustStart[text]]      => ValueOf(constant[text]).give(assess[MustStart[text]])
            case '[MustNotStart[text]]   => ValueOf(constant[text]).give(assess[MustNotStart[text]])
            case '[MustEnd[text]]        => ValueOf(constant[text]).give(assess[MustEnd[text]])
            case '[MustNotEnd[text]]     => ValueOf(constant[text]).give(assess[MustNotEnd[text]])
            case '[MustContain[text]]    => ValueOf(constant[text]).give(assess[MustContain[text]])
            case '[MustNotContain[text]] => ValueOf(constant[text]).give(assess[MustNotContain[text]])
            case '[MustNotEqual[text]]   => ValueOf(constant[text]).give(assess[MustNotEqual[text]])
      case _ =>
        abandon(m"Could not access constraint")

    
    '{${Expr(name)}.asInstanceOf[Name[PlatformType]]}

transparent inline def disintersect[IntersectionType] =
  ${Nomenclature2.disintersection[IntersectionType]}