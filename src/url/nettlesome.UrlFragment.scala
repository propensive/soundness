/*
    Nettlesome, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import anticipation.*
import contextual.*
import prepositional.*

object UrlFragment:
  def apply(value: Text): UrlFragment = UrlFragment.Textual(value)
  def apply(value: Int): UrlFragment = UrlFragment.Integral(value)

  given Substitution[UrlFragment, Text, "x"] = UrlFragment.Textual(_)
  given Substitution[UrlFragment, Raw, "x"] = raw => UrlFragment.RawTextual(raw.text)
  given Substitution[UrlFragment, Int, "80"] = UrlFragment.Integral(_)

  given [ValueType: Encodable in Text] => Substitution[UrlFragment, ValueType, "x"] =
    value => UrlFragment.Textual(ValueType.encode(value))

enum UrlFragment:
  case Integral(value: Int)
  case Textual(value: Text)
  case RawTextual(value: Text)
