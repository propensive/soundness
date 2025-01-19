/*
    Anticipation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import prepositional.*

object Encodable:
  given bytes: Bytes is Encodable in Bytes = identity(_)
  given int: Int is Encodable in Text = _.toString.tt
  given double: Double is Encodable in Text = _.toString.tt
  given byte: Byte is Encodable in Text = _.toString.tt
  given short: Short is Encodable in Text = _.toString.tt
  given long: Long is Encodable in Text = _.toString.tt
  given float: Float is Encodable in Text = _.toString.tt
  given text: Text is Encodable in Text = identity(_)
  given char: Char is Encodable in Text = _.toString.tt

trait Encodable:
  private inline def encodable: this.type = this
  type Self
  type Format
  def encode(value: Self): Format
  def omit(value: Self): Boolean = false

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Encodable in Format =
    new Encodable:
      type Self = SelfType2
      type Format = encodable.Format

      def encode(value: Self): Format = encodable.encode(lambda(value))
      override def omit(value: Self): Boolean = encodable.omit(lambda(value))
