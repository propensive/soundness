/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.into

trait Bond[TypeclassType <: Any { type Self }]:
  val typeclass: TypeclassType
  val value: typeclass.Self
  def apply(): typeclass.Self = value
  type Value = value.type

  def over[InputType, ResultType](lambda: TypeclassType ?=> typeclass.Self ?=> ResultType): ResultType =
    lambda(using typeclass)(using value)

object Bond:
  inline given [TypeclassType <: Any { type Self }]
     (using typeclass0: TypeclassType, value0: typeclass0.Self) => Bond[TypeclassType]:
    val typeclass: typeclass0.type = typeclass0
    val value: typeclass.Self = value0
