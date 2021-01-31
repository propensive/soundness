package example

import cosmopolite._
import languages.common._

type MyLangs = En | De | Es | Fr

var dynamicLang = "es"

@main
def run(lang: String): Unit =
   def number(n: Int): Messages[MyLangs] = n match
      case 1 => en"one" & fr"un" & de"ein" & es"uno"
      case 2 => en"two" & fr"deux" & de"zwei" & es"dos"
      case 3 => en"three" & fr"trois" & de"drei" & es"tres"

   val msg: Messages[MyLangs] =
      en"This is ${number(1)} in English" &
      de"Das ist ${number(1)} auf Deutsch" &
      es"Es ${number(1)} en español" &
      fr"C'est ${number(1)} en français"

   Language.parse[MyLangs](lang).foreach { lang =>
      given Language[MyLangs] = lang
      println(msg())
   }