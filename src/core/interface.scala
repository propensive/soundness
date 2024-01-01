/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import rudiments.*

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]):
  def full: String = s"$owner.$short"

object CaseClass:
  trait Param[Typeclass[_], Type](val label: String, val index: Int,
      val repeated: Boolean, val annotations: IArray[Matchable], val typeAnnotations: IArray[Any]):
    
    type PType
    
    def typeclass: Typeclass[PType]
    def deref(param: Type): PType
    def default: Option[PType]
  
  object Param:
    def apply[F[_], T, P](name: String, idx: Int, repeated: Boolean, cbn: CallByNeed[F[P]],
        defaultVal: CallByNeed[Option[P]], annotations: IArray[Matchable],
        typeAnnotations: IArray[Any]): Param[F, T] =
      new CaseClass.Param[F, T](name, idx, repeated, annotations, typeAnnotations):
        type PType = P
        def default: Option[PType] = defaultVal.value
        def typeclass = cbn.value
        def deref(value: T): P = value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]
  end Param
end CaseClass


abstract class CaseClass[Typeclass[_], Type]
                        (val typeInfo: TypeInfo,
                         val isObject: Boolean,
                         val isValueClass: Boolean,
                         val params: IArray[CaseClass.Param[Typeclass, Type]],
                         val annotations: IArray[Matchable],
                         val typeAnnotations: IArray[Any]) extends Serializable:

  type Param = CaseClass.Param[Typeclass, Type]
  
  def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): Type
  def constructMonadic[Monad[_]: Monadic, PType](using ClassTag[PType])(make: Param => Monad[PType]): Monad[Type]
  
  def constructEither[Err, PType: ClassTag](makeParam: Param => Either[Err, PType])
      : Either[List[Err], Type]
  
  def rawConstruct(fieldValues: Seq[Any]): Type

  def param[P](name: String, idx: Int, repeated: Boolean, cbn: CallByNeed[Typeclass[P]],
      defaultVal: CallByNeed[Option[P]], annotations: IArray[Matchable],
      typeAnnotations: IArray[Any]): Param =
    new CaseClass.Param[Typeclass, Type](name, idx, repeated, annotations, typeAnnotations):
      type PType = P
      def default: Option[PType] = defaultVal.value
      def typeclass = cbn.value
      def deref(value: Type): P = value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]
end CaseClass

case class SealedTrait[Typeclass[_], Type]
                      (typeInfo: TypeInfo, subtypes: IArray[SealedTrait.Subtype[Typeclass, Type, ?]],
                           annotations: IArray[Matchable], typeAnnotations: IArray[Any])
extends Serializable:

  type Subtype[S] = SealedTrait.SubtypeValue[Typeclass, Type, S]

  def choose[Return](value: Type)(handle: Subtype[?] => Return): Return =
    @tailrec def rec(ix: Int): Return =
      if ix < subtypes.length then
        val sub = subtypes(ix)
        if sub.cast.isDefinedAt(value) then handle(SealedTrait.SubtypeValue(sub, value))
        else rec(ix + 1)
      else throw IllegalArgumentException(s"The given value `$value` is not a sub type of `$typeInfo`")

    rec(0)

object SealedTrait:
  class Subtype[Typeclass[_], Type, SType]
               (val typeInfo: TypeInfo, val annotations: IArray[Matchable], val typeAnnotations: IArray[Any],
                    val index: Int, callByNeed: CallByNeed[Typeclass[SType]], isType: Type => Boolean,
                    asType: Type => SType & Type):

    def typeclass: Typeclass[SType & Type] = callByNeed.value.asInstanceOf[Typeclass[SType & Type]]
    
    def cast: PartialFunction[Type, SType & Type] =
      case t if isType(t) => asType(t)
    
    def isDefinedAt(t: Type): Boolean = isType(t)
    def apply(t: Type): SType & Type = asType(t)

  class SubtypeValue[Typeclass[_], Type, S](val subtype: Subtype[Typeclass, Type, S], v: Type):
    export subtype.{typeclass, typeAnnotations, annotations, cast, typeInfo}
    def value: S & Type = cast(v)

object CallByNeed:
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a)

final class CallByNeed[+A](private var eval: () => A) extends Serializable:
  lazy val value: A =
    val result = eval()
    import language.unsafeNulls
    eval = null
    result
