/*
    Panopticon, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package panopticon

import rudiments.*
import fulminate.*

import scala.quoted.*
import scala.compiletime.*

import language.dynamics


class Target[FromType, PathType <: Tuple]() extends Dynamic:
  transparent inline def selectDynamic(member: String): Any =
    ${Panopticon.dereference[FromType, PathType]('member)}

export Panopticon.Lens

extension [FromType, PathType <: Tuple, ToType](lens: Lens[FromType, PathType, ToType])
  @targetName("append")
  infix def ++
      [ToType2, PathType2 <: Tuple]
      (right: Lens[ToType, PathType2, ToType2])
      : Lens[FromType, Tuple.Concat[PathType, PathType2], ToType2] =
    Lens.make()
  
  inline def get(target: FromType): ToType =
    ${Panopticon.get[FromType, PathType, ToType]('target)}
  
  inline def set(target: FromType, newValue: ToType): FromType =
    ${Panopticon.set[FromType, PathType, ToType]('target, 'newValue)}

trait MemberType[TargetType, LabelType <: String & Singleton]:
  type ReturnType

object Panopticon:
  opaque type Lens[FromType, PathType <: Tuple, ToType] = Int
  opaque type InitLens[FromType] = Int

  object Lens:
    def apply[FromType]: InitLens[FromType] = 0
    def make[FromType, PathType <: Tuple, ToType](): Lens[FromType, PathType, ToType] = 0


  extension [FromType](initLens: InitLens[FromType])
    def apply
      [PathType <: Tuple, ToType]
      (lambda: Target[FromType, EmptyTuple] => Target[ToType, PathType])
      : Lens[FromType, PathType, ToType] =
    0
  
  private def getPath[TupleType <: Tuple: Type](path: List[String] = Nil)(using Quotes): List[String] =
    import quotes.reflect.*

    Type.of[TupleType] match
      case '[type tail <: Tuple; head *: tail] => (TypeRepr.of[head].asMatchable: @unchecked) match
        case ConstantType(StringConstant(str)) => getPath[tail](str :: path)
      case _                                   => path

  def getPaths[TupleType <: Tuple: Type](paths: List[List[String]] = Nil)(using Quotes): List[List[String]] =
    import quotes.reflect.*

    Type.of[TupleType] match
      case '[type tail <: Tuple; head *: tail] => Type.of[head] match
        case '[type tupleType <: Tuple; tupleType] => getPath[tupleType]() :: getPaths[tail]()
      case _ =>
        fail(msg"unexpectedly did not match")

  def get
      [FromType: Type, PathType <: Tuple: Type, ToType: Type](value: Expr[FromType])
      (using Quotes): Expr[ToType] =
    import quotes.reflect.*
    
    def select[TargetType: Type](path: List[String], expr: Expr[TargetType]): Expr[ToType] = path match
      case Nil          => expr.asExprOf[ToType]
      case next :: tail => ConstantType(StringConstant(next)).asType match
        case '[type nextType <: Label; nextType] =>
          Expr.summon[Dereferencer[TargetType, nextType]] match
            case Some('{type fieldType; $dereferencer: Dereferencer[TargetType, labelType] { type FieldType = fieldType }}) =>
              select[fieldType](tail, '{$dereferencer.field($expr)})
      
            case None =>
              expr.asTerm.select(TypeRepr.of[TargetType].typeSymbol.fieldMember(next)).asExpr match
                case '{$expr: targetType} => select[targetType](tail, expr)

    select[FromType](getPath[PathType](), value).asExprOf[ToType]

  def set
      [FromType: Type, PathType <: Tuple: Type, ToType: Type]
      (value: Expr[FromType], newValue: Expr[ToType])
      (using Quotes): Expr[FromType] =
    import quotes.reflect.*

    val fromTypeRepr: TypeRepr = TypeRepr.of[FromType]

    def rewrite(path: List[String], term: Term): Term =
      path match
        case Nil =>
          term
        
        case next :: tail =>
          val newParams = term.tpe.typeSymbol.caseFields.map: field =>
            if field.name == next then
              if tail == Nil then newValue.asTerm else rewrite(tail, Select(term, field))
            else Select(term, field)
          
          term.tpe.classSymbol match
            case Some(classSymbol) =>
              Apply(Select(New(TypeIdent(classSymbol)), term.tpe.typeSymbol.primaryConstructor),
                  newParams)
            
            case None =>
              fail(msg"the type ${fromTypeRepr.show} does not have a primary constructor")
        
    rewrite(getPath[PathType](), value.asTerm).asExprOf[FromType]
  
  def dereference[TargetType: Type, TupleType <: Tuple: Type](member: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    
    val fieldName = member.valueOrAbort
    val fieldNameType = ConstantType(StringConstant(fieldName)).asType
    val targetType = TypeRepr.of[TargetType]

    fieldNameType match
      case '[type fieldNameType <: Label; fieldNameType] =>
        Expr.summon[Dereferencer[TargetType, fieldNameType]] match
          case Some('{type fieldType; $dereferencer: Dereferencer[TargetType, labelType] { type FieldType =
              fieldType }}) =>
            '{Target[fieldType, fieldNameType *: TupleType]()}
    
          case None =>
            targetType.typeSymbol.caseFields.find(_.name == fieldName) match
              case None =>
                fail(msg"the field $fieldName is not a member of ${targetType.show}")
              
              case Some(symbol) => (symbol.info.asType: @unchecked) match
                case '[returnType] => '{Target[returnType, fieldNameType *: TupleType]()}
  
trait Dereferencer[TargetType, LabelType <: Label]:
  type FieldType
  def field(target: TargetType): FieldType