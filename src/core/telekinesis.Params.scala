/*
    Telekinesis, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

import language.dynamics

case class Params(values: List[(Text, Text)]):
  def append(more: Params): Params = Params(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty

  def prefix(str: Text): Params = Params:
    values.map { (k, v) => if k.length == 0 then str -> v else t"$str.$k" -> v }

  def queryString: Text =
    values.map: (k, v) =>
      if k.length == 0 then v.urlEncode else t"${k.urlEncode}=${v.urlEncode}"

    . join(t"&")
