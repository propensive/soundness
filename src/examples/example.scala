package example

import cosmopolite._, languages.common._

type MyLangs = En | De | Es | Fr

type SubsetLangs = En | Fr

var dynamicLang = "es"

@main
def run(lang: String): Unit =
   def number(n: Int): Messages[MyLangs] = n match
      case 1 => en"one" & fr"un" & de"ein" & es"uno"
      case 2 => en"two" & fr"deux" & de"zwei" & es"dos"
      case 3 => en"three" & fr"trois" & de"drei" & es"tres"

   val msg: Messages[SubsetLangs] =
      en"This is the number ${number(1)} in English" &
      de"Das ist die Nummer ${number(1)} auf Deutsch" &
      es"Es el numero ${number(1)} en español" &
      fr"C'est le numéro ${number(1)} en français"

   Language.parse[En | Fr](lang).foreach { lang =>
      given Language[Fr | En] = lang
      println(msg())
   }
