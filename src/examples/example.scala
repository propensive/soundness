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

package example

import cosmopolite.*, languages.common.*

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
