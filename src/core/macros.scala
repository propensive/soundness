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
import scala.compiletime.*

import language.experimental.captureChecking

object Perforate:
  def readUnion(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    repr.dealias.asMatchable match
      case OrType(left, right) => readUnion(left) ++ readUnion(right)
      case other               => Set(other)

  def makeUnion(using quotes: Quotes)(symbols: List[Type[?]]): quotes.reflect.TypeRepr = symbols match
    case Nil => quotes.reflect.TypeRepr.of[Nothing]
    case head :: tail => (makeUnion(tail).asType: @unchecked) match
      case '[type unionType <: Error; unionType] => (head: @unchecked) match
        case '[type headType <: Error; headType] => quotes.reflect.TypeRepr.of[unionType | headType]

  def mitigate
      (handler: Expr[PartialFunction[Error, Error]])
      (using Quotes)
      : Expr[Mitigation[Nothing]] =
    import quotes.reflect.*
    
    val types: List[Type[?]] = rhsTypes(handler)
    val inputTypes: List[Type[?]] = patternTypes(handler)
    
    makeUnion(inputTypes).asType match
      case '[type inputErrorType <: Error; inputErrorType] =>
        '{
          new Mitigation[inputErrorType]:
            def handle[SuccessType](mitigated: Mitigated[SuccessType, inputErrorType]): SuccessType = mitigated match
              case Mitigated.Success(value) =>
                value
              
              case Mitigated.Failure(error) =>
                ${
                  def recur(scrutinee: Expr[Error], types: List[Type[?]], result: Expr[Nothing]): Expr[Nothing] = types match
                    case Nil =>
                      result
                      
                    case rhsType :: types =>
                      rhsType match
                        case '[type errorType <: Error; errorType] =>
                          Expr.summon[Raises[errorType]] match
                            case None =>
                              fail(msg"cannot raise ${TypeRepr.of[errorType].show}")
                              
                            case Some(raises) =>
                              recur(scrutinee, types, '{ if $scrutinee.isInstanceOf[errorType] then abort($scrutinee.asInstanceOf[errorType])(using $raises) else $result })

                  recur('{$handler(error)}, types, '{???})
                }
        }

  def rhsTypes(using quotes: Quotes)(handler: Expr[PartialFunction[Error, Error]]): List[Type[?]] =
    import quotes.reflect.*

    handler.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(matchId, caseDefs)))), term)) =>
        def recur(types: List[Type[?]], todo: List[CaseDef]): List[Type[?]] = todo match
          case Nil =>
            types
          
          case caseDef :: rest => caseDef match
            case CaseDef(pattern, None, rhs) => rhs.asExpr match
              case '{type rhsType <: Error; $expr: rhsType} =>
                recur(TypeRepr.of[rhsType].asType :: types, rest)
              
        recur(Nil, caseDefs)

  def patternTypes(using quotes: Quotes)(handler: Expr[PartialFunction[Error, Error]]): List[Type[?]] =
    import quotes.reflect.*
    
    handler.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(matchId, caseDefs)))), term)) =>
        def recur(types: List[Type[?]], todo: List[CaseDef]): List[Type[?]] = todo match
          case Nil =>
            types
          
          case caseDef :: todo => caseDef match
            case CaseDef(pattern, None, _) => pattern match
              case Unapply(x, y, z) => fail(msg"Unapply(${x.toString}, ${y.toString}, ${z.toString})")
              case Unapply(Select(ident, "unapply"), _, _) => recur(ident.symbol.companionClass.typeRef.asType :: types, todo)
              case Bind(_, ident)                          => recur(ident.symbol.typeRef.asType :: types, todo)
              case Typed(ident, _)                         => recur(ident.symbol.typeRef.asType :: types, todo)
              case TypedOrTest(Unapply(x, y, z), ident)                       =>
                println(s"typed: $ident, $x, $y, $z")
                recur(ident.symbol.typeRef.asType :: types, todo)
              case other                                   => recur(types, todo)
          
        recur(Nil, caseDefs)
/*
  def mitigateOld
      [ErrorType <: Error: Type, SuccessType: Type]
      (mitigation: Expr[Mitigated[SuccessType, ErrorType]], handler: Expr[PartialFunction[ErrorType, Error]])
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val resultTypes = handler.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(matchId, caseDefs)))), term)) =>
        def recur(done: Set[TypeRepr], original: Set[TypeRepr], caseDefs: List[CaseDef]): Set[TypeRepr] = caseDefs match
          case Nil =>
            done ++ original
          
          case caseDef :: todo => caseDef match
            case CaseDef(pattern, None, rhs) => rhs.asExpr match
              case '{type rhsType <: Error; $expr: rhsType} =>
                pattern match
                  case Unapply(Select(ident, "unapply"), _, _) =>
                    original.find(_.typeSymbol == ident.symbol.companionClass.typeRef.typeSymbol) match
                      case None      => recur(done, original, todo)
                      case Some(ref) => recur(done + TypeRepr.of[rhsType], original - ref, todo)
                  case Typed(_, ident) =>
                    original.find(_.typeSymbol == ident.symbol.typeRef.typeSymbol) match
                      case None      => recur(done, original, todo)
                      case Some(ref) => recur(done + TypeRepr.of[rhsType], original - ref, todo)
                  case Bind(_, ident) =>
                    original.find(_.typeSymbol == ident.symbol.typeRef.typeSymbol) match
                      case None      => recur(done, original, todo)
                      case Some(ref) => recur(done + TypeRepr.of[rhsType], original - ref, todo)
                  case _ =>
                    recur(done + TypeRepr.of[rhsType], original, todo)
              
        recur(Set(), readUnion(TypeRepr.of[ErrorType]), caseDefs)

    resultTypes.foldLeft(TypeRepr.of[Nothing]): (acc, next) =>
      acc.asType match
        case '[type acc <: Error; acc] => next.asType match
          case '[type next <: Error; next] => TypeRepr.of[acc | next]
    .asType match
      case '[type errorType <: Error; errorType] =>
        '{$mitigation.handle($handler).asInstanceOf[Mitigated[SuccessType, errorType]]}*/
  
