package cosmopolite
import scala.reflect._
import scala.util.NotGiven
import scala.annotation._
import scala.language.implicitConversions
import scala.quoted._
import reflect.Selectable.reflectiveSelectable

import scala.annotation.targetName

case class Language[+L <: String](value: String)

object Language:
   @targetName("make")
   def apply[L <: String: ValueOf]: Language[L] = new Language(summon[ValueOf[L]].value)
   
   inline def parse[L <: String](str: String): Option[Language[L]] =  ${parseCode[L]('str)}

   def parseCode[L <: String: Type](str: Expr[String])(using quotes: Quotes): Expr[Option[Language[L]]] =
      import quotes.reflect._

      def langs(t: TypeRepr): List[String] = t.dealias match
         case OrType(left, right) => langs(left) ++ langs(right)
         case ConstantType(StringConstant(lang)) => List(lang)

      langs(TypeRepr.of[L]).foldLeft('{ None: Option[Language[L]] }) { (agg, lang) =>
         '{ if $str == ${Expr(lang)} then Some(Language[L](${Expr(lang)})) else $agg }
      }

object Messages:
   def make[L <: String: ValueOf](seq: Seq[String], parts: Seq[Messages[? >: L]]): Messages[L] =
      val string = seq.head+(parts.zip(seq.tail).map { (msg, s) => msg(summon[ValueOf[L]])+s }.mkString)
      Messages[L](Map(summon[ValueOf[L]].value -> string))

case class Messages[L <: String](text: Map[String, String]):
   def &[L2 <: String](messages: Messages[L2])(using NotGiven[L2 <:< L]): Messages[L | L2] =
      Messages(text ++ messages.text)
   
   def apply[L2 <: L: ValueOf]: String = text(summon[ValueOf[L2]].value)
   def apply[L2 <: L]()(using ctx: Language[L2]): String = text(ctx.value)

import languages.common._
extension (ctx: StringContext)
   def en(msgs: Messages[? >: En]*): Messages[En] = Messages.make[En](ctx.parts, msgs)
   def ru(msgs: Messages[? >: Ru]*): Messages[Ru] = Messages.make[Ru](ctx.parts, msgs)
   def de(msgs: Messages[? >: De]*): Messages[De] = Messages.make[De](ctx.parts, msgs)
   def es(msgs: Messages[? >: Es]*): Messages[Es] = Messages.make[Es](ctx.parts, msgs)
   def fr(msgs: Messages[? >: Fr]*): Messages[Fr] = Messages.make[Fr](ctx.parts, msgs)
   def ja(msgs: Messages[? >: Ja]*): Messages[Ja] = Messages.make[Ja](ctx.parts, msgs)
   def pt(msgs: Messages[? >: Pt]*): Messages[Pt] = Messages.make[Pt](ctx.parts, msgs)
   def zh(msgs: Messages[? >: Zh]*): Messages[Zh] = Messages.make[Zh](ctx.parts, msgs)
   def it(msgs: Messages[? >: It]*): Messages[It] = Messages.make[It](ctx.parts, msgs)
   def pl(msgs: Messages[? >: Pl]*): Messages[Pl] = Messages.make[Pl](ctx.parts, msgs)