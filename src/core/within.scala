/*
    Perforate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package perforate

import fulminate.*
import rudiments.*

import scala.quoted.*

//import language.experimental.captureChecking

transparent inline def mitigate
    [ErrorType <: Error]
    (inline mitigation: PartialFunction[Throwable, ErrorType])
    : Mitigator[ErrorType] =
  ${Macros.mitigate[ErrorType]('mitigation)}

trait Mitigator[+ErrorType]:
  type Context[+ResultType]

  def within[ResultType](block: Context[ResultType]): ResultType

object Macros:
  def mitigate
      [ErrorType <: Error: Type]
      (handlers: Expr[PartialFunction[Throwable, ErrorType]])
      (using Quotes)
      : Expr[Mitigator[ErrorType]] =
    import quotes.reflect.*

    type Q = (Int, Int)

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean =
     pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        params.zip(patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)).forall(exhaustive) ||
            fail(msg"this pattern will not match every ${patternType.show}")
        
      case Unapply(Select(target, method), _, params) =>
        params.zip(patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)).forall(exhaustive) ||
            fail(msg"this pattern will not match every ${patternType.show}")
      
      case other => fail(msg"this pattern will not match every ${patternType.show}")
        

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Wildcard()          => Nil
      case Typed(_, matchType) => List(matchType.tpe)
      case Bind(_, pattern)    => patternType(pattern)
      
      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        target.tpe.typeSymbol.methodMember(method).head.info match
          case MethodType(_, _, unapplyType) =>
            if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil
          case _ =>
            Nil
      case other =>
        Nil

    val patternTypes: List[(TypeRepr, TypeRepr)] = handlers.asTerm match
      case Inlined(_, _, Block(List(defDef), term)) => defDef match
        case DefDef(ident, scrutineeType, returnType, Some(Match(matchId, caseDefs))) => caseDefs.flatMap:
          case CaseDef(pattern, None, rhs) =>
            rhs.asExpr match
              case '{$rhs: rhsType} => patternType(pattern).map((_, TypeRepr.of[rhsType]))
          case _ => Nil
        case _ =>
          Nil
      case _ =>
        Nil
    
    val raiseTypes = patternTypes.map(_(0)).map(_.asType).map:
      case '[type errorType <: Error; errorType] => TypeRepr.of[Raises[errorType]]
    
    TypeLambda(List("ResultType"), _ => List(TypeBounds.empty), lambda =>
      def recur(raiseTypes: List[TypeRepr]): TypeRepr = raiseTypes match
        case Nil => lambda.param(0)
        case head :: tail => AppliedType(defn.FunctionClass(1, true).typeRef, List(head, recur(tail)))
      recur(raiseTypes)
    ).asType match
      case '[type contextType[+_]; contextType] => '{
        new Mitigator[ErrorType]:
          type Context[+ResultType] = contextType[ResultType]

          def within[ResultType](block: contextType[ResultType]): ResultType = ${
            def recur(current: Expr[Any], patternTypes: List[(TypeRepr, TypeRepr)]): Expr[ResultType] =
              patternTypes match
                case Nil => current.asExprOf[ResultType]
                  
                case (source, target) :: tail => source.asType match
                  case '[type sourceType <: Error; sourceType] => target.asType match
                    case '[type targetType <: Error; targetType] =>
                      val raises = Expr.summon[Raises[targetType]].getOrElse:
                        fail(msg"there is no capability to raise a ${target.show} in this context")
                      
                      val raises2 = '{
                        $raises.contraMap[sourceType] { error => $handlers(error).asInstanceOf[targetType] }
                      }
                      
                      recur('{${current.asExprOf[Raises[sourceType] ?=> Any]}(using ${raises2})}, tail)
  
            recur('block, patternTypes)
          }
      }
