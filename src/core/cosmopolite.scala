package cosmopolite

import scala.reflect._
import scala.util.NotGiven
import scala.annotation._
import scala.quoted._

case class Language[+L <: String](value: String)

object Language:
   @targetName("make")
   def apply[L <: String: ValueOf]: Language[L] = new Language(summon[ValueOf[L]].value)
   
   inline def parse[L <: String](str: String): Option[Language[L]] =
      Option.when(reifyToSet[L].contains(str))(Language(str))

   private inline def reifyToSet[L <: String]: Set[String] = ${reifyToSetMacro[L]}

   private def reifyToSetMacro[L <: String: Type](using quotes: Quotes): Expr[Set[String]] =
      import quotes.reflect._

      def langs(t: TypeRepr): Set[String] = t.dealias match
         case OrType(left, right) => langs(left) ++ langs(right)
         case ConstantType(StringConstant(lang)) => Set(lang)

      Expr(langs(TypeRepr.of[L]))

object Messages:
   def apply[L <: String: ValueOf](seq: Seq[String], parts: Seq[Messages[? >: L]]): Messages[L] =
      val string = seq.head+(parts.zip(seq.tail).map { (msg, s) => msg(summon[ValueOf[L]])+s }.mkString)
      Messages[L](Map(summon[ValueOf[L]].value -> string))
   
case class Messages[-L <: String](text: Map[String, String]):
   def &[L2 <: String & Singleton](messages: Messages[L2])(using NotGiven[L2 <:< L]): Messages[L | L2] =
      Messages(text ++ messages.text)
   
   def apply[L2 <: L: ValueOf]: String = text(summon[ValueOf[L2]].value)
   def apply[L2 <: L]()(using ctx: Language[L2]): String = text(ctx.value)

import languages.common._

extension[L <: String] (str: String)
   def as(using ValueOf[L]): Messages[L] = Messages[L](List(str), Nil)

extension (ctx: StringContext)
   def en(msgs: Messages[En]*): Messages[En] = Messages(ctx.parts, msgs)
   def ru(msgs: Messages[Ru]*): Messages[Ru] = Messages(ctx.parts, msgs)
   def de(msgs: Messages[De]*): Messages[De] = Messages(ctx.parts, msgs)
   def es(msgs: Messages[Es]*): Messages[Es] = Messages(ctx.parts, msgs)
   def fr(msgs: Messages[Fr]*): Messages[Fr] = Messages(ctx.parts, msgs)
   def ja(msgs: Messages[Ja]*): Messages[Ja] = Messages(ctx.parts, msgs)
   def pt(msgs: Messages[Pt]*): Messages[Pt] = Messages(ctx.parts, msgs)
   def zh(msgs: Messages[Zh]*): Messages[Zh] = Messages(ctx.parts, msgs)
   def it(msgs: Messages[It]*): Messages[It] = Messages(ctx.parts, msgs)
   def pl(msgs: Messages[Pl]*): Messages[Pl] = Messages(ctx.parts, msgs)

