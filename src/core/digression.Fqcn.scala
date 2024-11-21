/*
    Digression, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package digression

import rudiments.*
import anticipation.*
import contingency.*
import prepositional.*

import language.experimental.captureChecking

object Fqcn:
  def valid(char: Char): Boolean =
    char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z' || char >= '0' && char <= '9' || char == '_'

  def apply(name: Text): Fqcn raises FqcnError =
    val parts = IArray.from(name.s.split("\\.").nn.map(_.nn))

    parts.foreach: part =>
      if part.length == 0 then raise(FqcnError(name, FqcnError.Reason.EmptyName), ())
      if Digression.javaKeywords.has(part) then raise(FqcnError(name, FqcnError.Reason.JavaKeyword(part.tt)), ())

      part.foreach: char =>
        if !valid(char) then raise(FqcnError(name, FqcnError.Reason.InvalidChar(char)), ())

      if part.head >= '0' && part.head <= '9'
      then raise(FqcnError(name, FqcnError.Reason.InvalidStart(part.head)), ())

    new Fqcn(parts.map(_.tt))

  given Fqcn is Encodable in Text = _.text

class Fqcn(val parts: IArray[Text]):
  def text: Text = parts.mkString(".").tt
  def className: Text = parts.last
  def packageName: Text = parts.init.mkString(".").tt
