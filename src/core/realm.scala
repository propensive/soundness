/*
    Fulminate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fulminate

import anticipation.*

import scala.quoted.*

object Realm:
  def make(name: Text): Realm = new Realm(name)

case class Realm(name: Text)

object Fulminate:
  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    import quotes.reflect.*
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+")
    then fail(msg"the realm name should comprise only of lowercase letters")(using Realm("fulminate".tt))
    else '{Realm.make(${Expr(name)}.tt)}

extension (inline context: StringContext)
  inline def realm(): Realm = ${Fulminate.realm('context)}
