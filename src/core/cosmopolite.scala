package cosmopolite
import scala.reflect._
import scala.util.NotGiven
import scala.annotation._
import scala.language.implicitConversions

object Language:
   def apply[L2 <: String & Singleton: ValueOf]: Language[L2] =
      new Language(summon[ValueOf[L2]].value)

case class Language[+L <: String: ValueOf](value: L)

object Messages:
   def make[L <: String: ValueOf](seq: Seq[String], parts: Seq[Messages[? >: L]]): Messages[L] =
      val string = seq.head+(parts.zip(seq.tail).map { (msg, s) => msg(summon[ValueOf[L]])+s }.mkString)
      Messages[L](Map(summon[ValueOf[L]].value -> string))

case class Messages[L <: String](text: Map[String, String]):
   def &[L2 <: String](messages: Messages[L2])(using NotGiven[(L | L2) =:= L]): Messages[L | L2] =
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

type MyLangs = En | De | Es | Fr

var dynamicLang = "es"

given Language[MyLangs] = dynamicLang match
   case "de" => Language[De]
   case "es" => Language[Es]
   case "fr" => Language[Fr]
   case _    => Language[En]

@main
def run(): Unit =
   def number(n: Int): Messages[En | Fr | De | Es] = n match
      case 1 => en"one" & fr"un" & de"ein" & es"uno"
      case 2 => en"two" & fr"deux" & de"zwei" & es"dos"
      case 3 => en"three" & fr"trois" & de"drei" & es"tres"

   val x: Messages[MyLangs] = en"This is ${number(1)} in English" &
       de"Das ist ${number(1)} auf Deutsch" &
       es"Es ${number(1)} en español" &
       fr"C'est ${number(1)} en français"

   println(x())
