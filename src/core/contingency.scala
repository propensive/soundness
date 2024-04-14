package contingency

import fulminate.*
import rudiments.*
import anticipation.*

import scala.quoted.*
import scala.compiletime.*

given realm: Realm = realm"contingency"

object Contingency:
  def mitigate[ErrorType <: Error: Type, ResultType: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ? <: Error]])
      (using Quotes)
           : Expr[Any] = '{???}
  
  def remedy[ErrorType <: Error: Type, ResultType: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ResultType]])
      (using Quotes)
           : Expr[Any] =

    import quotes.reflect.*

    val action: Expr[Errant[ErrorType] ?=> ResultType] = context.asTerm match
      case Inlined(None, Nil, Apply(_, List(action))) =>
        action.asExprOf[Errant[ErrorType] ?=> ResultType]

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || fail(msg"bad pattern")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || fail(msg"bad pattern")

      case other =>
        fail(msg"bad pattern")

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)
    
    val requiredHandlers = unpack(TypeRepr.of[ErrorType]).map(_.typeSymbol)
    
    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => fail(msg"wildcard")
      
      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[ErrorType]) then List(TypeRepr.of[ErrorType])
        else fail(msg"Unapply ${select.symbol.declaredType.toString}")
      
      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil
      
      case other =>
        fail(msg"this pattern could not be recognized as a distinct `Error` type")

    val caseDefs: List[CaseDef] = handler.asTerm match
      case Inlined(None, Nil, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil
      
      case _ =>
        fail(msg"unexpected lambda")

    val handledTypes: List[Symbol] =
      caseDefs.flatMap:
        case CaseDef(pattern, _, _) => patternType(pattern)
      .map(_.typeSymbol)
      
    val resolutionErrorTypes: List[TypeRepr] = caseDefs.flatMap:
      case CaseDef(_, _, rhs) => List(rhs.tpe)

    def pack(reprs: List[TypeRepr]): TypeRepr = reprs.foldLeft(TypeRepr.of[Error])(OrType(_, _))
    
    val unhandledErrorTypes: List[Symbol] = (requiredHandlers -- handledTypes).to(List)

    report.info(s"Unhandled error types: ${unhandledErrorTypes.toString}")

    def typeLambda[InsideType[_]: Type](errorTypes: List[TypeRepr])
        (makeExpr: Expr[PartialFunction[ErrorType, Nothing] => ResultType] => Expr[InsideType[ResultType]])
            : Term =

      errorTypes match
        case Nil =>
          makeExpr
           ('{
              (pf: PartialFunction[ErrorType, Nothing]) =>
                boundary: label ?=>
                  $action(using new RemedyErrant[ResultType, ErrorType]($handler.andThen(boundary.break(_)).orElse(pf), label))
            }).asTerm
        
        case errorType :: more => errorType.asType match
          case '[type errorType <: ErrorType; errorType] =>
            typeLambda[[ParamType] =>> Errant[errorType] ?=> InsideType[ParamType]](more):
              (makeResult: Expr[PartialFunction[ErrorType, Nothing] => ResultType]) =>
                '{ (errant: Errant[errorType]) ?=>
                  val makeResult2: PartialFunction[ErrorType, Nothing] => ResultType =
                    pf => $makeResult(pf.orElse { case error: `errorType` => errant.abort(error) })
                  ${makeExpr('makeResult2)}
                }
    
    val result = typeLambda[[ParamType] =>> ParamType](unhandledErrorTypes.map(_.typeRef)): fn =>
      '{$fn(PartialFunction.empty[ErrorType, Nothing])}
    
    result.asExpr

class RemedyErrant[ResultType, ErrorType <: Error](pf: PartialFunction[ErrorType, Nothing], label: boundary.Label[ResultType])
extends Errant[ErrorType]:
  def record(error: ErrorType): Unit = abort(error)
  def abort(error: ErrorType): Nothing = pf(error)
   