/*
    Wisteria, version 2.4.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria.examples

import wisteria.*
import gossamer.*

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `AsString` than is usual, as it permits the return type to
  *  be something other than a string. */
trait AsString[Out, T] { def asString(value: T): Out }

trait GenericAsString[Out] extends Derivation[[X] =>> AsString[Out, X]] {

  def joinElems(typeName: Txt, strings: Seq[Txt]): Out
  def prefix(s: Txt, out: Out): Out

  /** creates a new [[AsString]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def join[T](ctx: CaseClass[Typeclass, T]): AsString[Out, T] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.asString(param.deref(value))
    else
      val paramStrings: Seq[Txt] = ctx.params.map { param =>
        val attribStr = if param.annotations.isEmpty then str"" else {
          param.annotations.map(_.toString.show).join(str"{", str", ", str"}")
        }
        val tpeAttribStr = if param.typeAnnotations.isEmpty then str"" else {
          param.typeAnnotations.map(_.toString.show).join(str"{", str", ", str"}")
        }
        str"${param.label}$attribStr$tpeAttribStr=${param.typeclass.asString(param.deref(value)).toString.show}"
      }

      val anns = ctx.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if anns.isEmpty then str"" else anns.map(_.toString.show).join(str"{", str",", str"}")

      val tpeAnns = ctx.typeAnnotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val typeAnnotationStr = if tpeAnns.isEmpty then str"" else tpeAnns.map(_.toString.show).join(str"{", str",", str"}")


      def typeArgsString(typeInfo: TypeInfo): Txt =
        if typeInfo.typeParams.isEmpty then str""
        else typeInfo.typeParams.map(arg => str"${arg.short}${typeArgsString(arg)}").join(str"[", str",", str"]")

      joinElems(str"${ctx.typeInfo.short}${typeArgsString(ctx.typeInfo)}${annotationStr}${typeAnnotationStr}", paramStrings)
  }

  /** choose which typeclass to use based on the subtype of the sealed trait
    * and prefix with the annotations as discovered on the subtype. */
  override def split[T](ctx: SealedTrait[Typeclass, T]): AsString[Out, T] = (value: T) =>
    ctx.choose(value) { sub =>
      val anns = sub.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if anns.isEmpty then str"" else anns.map(_.toString.show).join(str"{", str",", str"}")

      prefix(annotationStr, sub.typeclass.asString(sub.value))
    }
}

/** companion object to [[AsString]] */
object AsString extends GenericAsString[Txt]:

  def prefix(s: Txt, out: Txt): Txt = s + out
  def joinElems(typeName: Txt, params: Seq[Txt]): Txt = params.join(str"$typeName(", str",", str")")

  given AsString[Txt, Txt] = identity(_)
  given AsString[Txt, Int] = _.show
  given AsString[Txt, Long] = long => str"${long.show}L"
  
  given[A](using AsString[Txt, A]): AsString[Txt, Seq[A]] =
    _.iterator.map(summon[AsString[Txt, A]].asString(_)).to(List).join(str"[", str",", str"]")
