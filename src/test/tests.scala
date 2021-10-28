/*
    Cosmopolite, version 0.5.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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

import languages.common.*

import probably.*
import rudiments.*
import gossamer.*
import eucalyptus.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

object Tests extends Suite(str"Cosmopolite Tests"):
  def run(using Runner): Unit =
    test(str"extract language from string (English)") {
      val two = en"two" & fr"deux"
      two[En]
    }.check(_ == str"two")
    
    test(str"extract language from string (French)") {
      val two = en"two" & fr"deux"
      two[Fr]
    }.check(_ == str"deux")
    
    test(str"extract default language") {
      val two = en"two" & fr"deux"
      given Language[Fr] = Language[Fr]("fr")
      two()
    }.check(_ == str"deux")
