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

import language.experimental.into

import scala.quoted.*

import anticipation.*

object Fulminate:

  opaque type Diagnostics = Boolean

  object Diagnostics:
    val capture: Diagnostics = true
    val omit: Diagnostics = false

  extension (diagnostics: Diagnostics) def captureStack: Boolean = diagnostics

  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+")
    then halt(m"the realm name should contain only lowercase letters")(using Realm("fulminate"))
    else '{Realm.make(${Expr(name)}.tt)}
