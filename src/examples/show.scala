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

package wisteria.examples

import wisteria.*
import gossamer.*

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `AsString` than is usual, as it permits the return type to
  *  be something other than a string. */
trait AsString[Out, T] { def asString(value: T): Out }

trait GenericAsString[Out] extends Derivation[[X] =>> AsString[Out, X]] {

  def joinElems(typeName: Text, strings: Seq[Text]): Out
  def prefix(s: Text, out: Out): Out

  /** creates a new [[AsString]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def join[T](ctx: CaseClass[Typeclass, T]): AsString[Out, T] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.asString(param.deref(value))
    else
      val paramStrings: Seq[Text] = ctx.params.map { param =>
        val attribStr = if param.annotations.isEmpty then t"" else {
          param.annotations.map(Showable(_).show).join(t"{", t", ", t"}")
        }
        val tpeAttribStr = if param.typeAnnotations.isEmpty then t"" else {
          param.typeAnnotations.map(Showable(_).show).join(t"{", t", ", t"}")
        }
        t"${param.label}$attribStr$tpeAttribStr=${Showable(param.typeclass.asString(param.deref(value))).show}"
      }

      val anns = ctx.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if anns.isEmpty then t"" else anns.map(Showable(_).show).join(t"{", t",", t"}")

      val tpeAnns = ctx.typeAnnotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val typeAnnotationStr = if tpeAnns.isEmpty then t"" else tpeAnns.map(Showable(_).show).join(t"{", t",", t"}")


      def typeArgsString(typeInfo: TypeInfo): Text =
        if typeInfo.typeParams.isEmpty then t""
        else typeInfo.typeParams.map(arg => t"${arg.short}${typeArgsString(arg)}").join(t"[", t",", t"]")

      joinElems(t"${ctx.typeInfo.short}${typeArgsString(ctx.typeInfo)}${annotationStr}${typeAnnotationStr}", paramStrings)
  }

  /** choose which typeclass to use based on the subtype of the sealed trait
    * and prefix with the annotations as discovered on the subtype. */
  override def split[T](ctx: SealedTrait[Typeclass, T]): AsString[Out, T] = (value: T) =>
    ctx.choose(value) { sub =>
      val anns = sub.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if anns.isEmpty then t"" else anns.map(Showable(_).show).join(t"{", t",", t"}")

      prefix(annotationStr, sub.typeclass.asString(sub.value))
    }
}

/** companion object to [[AsString]] */
object AsString extends GenericAsString[Text]:

  def prefix(s: Text, out: Text): Text = s + out
  def joinElems(typeName: Text, params: Seq[Text]): Text = params.join(t"$typeName(", t",", t")")

  given AsString[Text, Text] = identity(_)
  given AsString[Text, Int] = _.show
  given AsString[Text, Long] = long => t"${long.show}L"
  
  given[A](using AsString[Text, A]): AsString[Text, Seq[A]] =
    _.iterator.map(summon[AsString[Text, A]].asString(_)).to(List).join(t"[", t",", t"]")
