/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import anticipation.*
import rudiments.*
import vacuous.*
import contingency.*
import fulminate.*
import contingency.errorHandlers.throwUnsafely

import scala.deriving.*
import scala.compiletime.*

trait SimpleSumDerivation[TypeclassType[_]] extends SimpleSumDerivationMethods[TypeclassType]:
  inline given derived[DerivationType](using reflection: SumReflection[DerivationType])
    : TypeclassType[DerivationType] = 
      split[DerivationType](using reflection)

trait Show[T] {
  def show(value: T): String
}

object Show extends SimpleSumDerivation[Show] {
  inline def split[DerivationType: SumReflection]: Show[DerivationType] = value =>
    variant(value): 
      [VariantType <: DerivationType] => variant =>
        s"label($label), index($index)"
}

trait Read[T] {
  def read(value: String): T
}

object Read extends SimpleSumDerivation[Read] {
  inline def split[DerivationType: SumReflection]: Read[DerivationType] = input =>
    delegateFromName(input)
}

enum Test:
  case First
  case Second
  case Third

// @main
def prototypeTest(): Unit =
  val showImpl = summon[Show[Test]]
  val readImpl = summon[Read[Test]]

  val value = readImpl.read("Second")
  println(showImpl.show(value))
