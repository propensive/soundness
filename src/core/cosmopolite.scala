/*
    Cosmopolite, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cosmopolite

import rudiments.*
import gossamer.*
import spectacular.*

import scala.util.NotGiven
import scala.quoted.*
import scala.compiletime.*

case class Polyglot[+ValueType, +LocalizationType <: Localization](value: Map[String, ValueType]):
  def apply[LocalizationType <: LocalizationType & Singleton] =
    ${Cosmopolite.access}
  
  def map[ValueType2](fn: ValueType => ValueType2): Polyglot[ValueType2, LocalizationType] =
    Polyglot(value.mapValues(fn))
  
  def ap(polyglotFn: Polyglot[ValueType => ValueType2, LocalizationType])
  
  def &
      [ValueType2 >: ValueType, LocalizationType2]
      (other: Polyglot[ValueType2, LocalizationType2])
      : Polyglot[ValueType2, LocalizationType2]

object Cosmopolite:
  def access[ValueType: Type, LocalizationType <: Localization: Type]
      (value: Expr[Map[String, ValueType]])(using Quotes)
      : Expr[ValueType] =
    import quotes.reflect.*

    val choice: String = summon[LocalizationType].value

    def recur(


// case class Language[+L <: String](value: String)

// object Language:
//   @targetName("make")
//   def apply[L <: String: ValueOf]: Language[L] = Language(summon[ValueOf[L]].value)
  
//   inline def parse[L <: String](str: String): Option[Language[L]] =
//     Option.when(reifyToSet[L].contains(str))(Language(str))

//   private inline def reifyToSet[L <: String]: Set[String] = ${reifyToSetMacro[L]}

//   private def reifyToSetMacro[L <: String: Type](using Quotes): Expr[Set[String]] =
//     import quotes.reflect.*

//     def langs(t: TypeRepr): Set[String] = t.dealias.asMatchable match
//       case OrType(left, right)                => langs(left) ++ langs(right)
//       case ConstantType(StringConstant(lang)) => Set(lang)
//       case _                                  => fail(msg"expected a union or constant type")

//     Expr(langs(TypeRepr.of[L]))

// object Messages:
//   def apply[L <: String: ValueOf](seq: Seq[Text], parts: Seq[Messages[? >: L]]): Messages[L] =
//     val string: Text = parts.zip(seq.tail).map: (msg, s) =>
//       t"${msg(using summon[ValueOf[L]])}$s"
//     .join(seq.head, t"", t"")

//     Messages[L](Map(summon[ValueOf[L]].value.show -> string))
   
// case class Messages[-L <: String](text: Map[Text, Text]):
//   @targetName("and") 
//   infix def &[L2 <: String & Singleton](messages: Messages[L2])(using NotGiven[L2 <:< L]): Messages[L | L2] =
//     Messages(text ++ messages.text)
   
//   def apply[L2 <: L: ValueOf]: Text = text(summon[ValueOf[L2]].value.show)
//   def apply[L2 <: L]()(using ctx: Language[L2]): Text = text(ctx.value.show)

// import languages.common.*

// extension [L <: String](str: Text)
//   def as(using ValueOf[L]): Messages[L] = Messages[L](List(str), Nil)

// extension (ctx: StringContext)
//   def en(msgs: Messages[En]*): Messages[En] = Messages(ctx.parts.map(_.show), msgs)
//   def ru(msgs: Messages[Ru]*): Messages[Ru] = Messages(ctx.parts.map(_.show), msgs)
//   def de(msgs: Messages[De]*): Messages[De] = Messages(ctx.parts.map(_.show), msgs)
//   def es(msgs: Messages[Es]*): Messages[Es] = Messages(ctx.parts.map(_.show), msgs)
//   def fr(msgs: Messages[Fr]*): Messages[Fr] = Messages(ctx.parts.map(_.show), msgs)
//   def ja(msgs: Messages[Ja]*): Messages[Ja] = Messages(ctx.parts.map(_.show), msgs)
//   def pt(msgs: Messages[Pt]*): Messages[Pt] = Messages(ctx.parts.map(_.show), msgs)
//   def zh(msgs: Messages[Zh]*): Messages[Zh] = Messages(ctx.parts.map(_.show), msgs)
//   def it(msgs: Messages[It]*): Messages[It] = Messages(ctx.parts.map(_.show), msgs)
//   def pl(msgs: Messages[Pl]*): Messages[Pl] = Messages(ctx.parts.map(_.show), msgs)
