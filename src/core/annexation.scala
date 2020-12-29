/*

  Annexation, version 1.0.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

 */
package rudiments

import language.higherKinds
import language.implicitConversions

object Annex {

  def apply[Typeclass[_], Type](value: Type, typeclass: Typeclass[Type]) =
    Annexed[Typeclass, Type](value, typeclass)

  implicit def annexed[Type, Typeclass[_]](
    value: Type
  )(implicit typeclass: Typeclass[Type]): Annex[Typeclass] { type Value = Type } =
    Annexed[Typeclass, Type](value, typeclass)
}

trait Annex[Typeclass[_]] {
  type Value
  def value: Value
  def typeclass: Typeclass[Value]
  def apply[Return](function: Typeclass[Value] => Value => Return): Return =
    function(typeclass)(value)
}

private[rudiments] case class Annexed[Typeclass[_], Type](value: Type,
                                                           typeclass: Typeclass[Type])
    extends Annex[Typeclass] {
  type Value = Type
}
