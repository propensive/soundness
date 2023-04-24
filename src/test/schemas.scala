/*
    Cellulose, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import gossamer.*
import rudiments.*
import digression.*

import Arity.*

val Greek = Struct(Optional,
  t"alpha" -> Field(Optional),
  t"beta"  -> Field(One),
  t"gamma" -> Field(Many),
  t"delta" -> Field(AtLeastOne),
  t"eta"   -> Field(Unique),
  t"iota"  -> Field(Optional),
  t"kappa" -> Field(Many)
)

object GreekRecords extends RecordType(Greek):
  transparent inline def record(inline fn: String => Any): polyvinyl.Record = ${build('fn)}

val example1 = unsafely:
  Greek.parse(t"""
    alpha  one
    beta   two
    gamma  three four five
    delta  six seven
    eta    eight
    kappa  nine
    kappa  ten eleven
  """)
