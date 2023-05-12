/*
    Spectacular, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import rudiments.*

class Match1[ValueType, Param1Type](val value: ValueType) extends AnyVal, Product:
  def _1: Param1Type = ???
  def canEqual(that: Any): Boolean = true
  def productArity: Int = 1
  def productElement(n: Int): Any = _1

trait Parser[+ValueType]:
  def unapply(text: Text): Match1[Text, Char] = new Match1[Text, Char](text)

object Parser

