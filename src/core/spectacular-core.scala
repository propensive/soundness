/*
    Spectacular, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*

import language.experimental.captureChecking

extension [ValueType: Showable](value: ValueType)
  def show: Text = ValueType.text(value)

extension [ValueType: Inspectable](value: ValueType) def inspect: Text = ValueType.text(value)

extension (text: Text)
  def decode[ValueType: Decoder]: ValueType = ValueType.decode(text)

extension [ValueType: Encodable](value: ValueType)
  def encode: ValueType.Format = ValueType.encode(value)

package booleanStyles:
  given yesNo: BooleanStyle = BooleanStyle("yes".tt, "no".tt)
  given onOff: BooleanStyle = BooleanStyle("on".tt, "off".tt)
  given trueFalse: BooleanStyle = BooleanStyle("true".tt, "false".tt)
  given oneZero: BooleanStyle = BooleanStyle("1".tt, "0".tt)
