/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

trait Encodable:
  private inline def encodable: this.type = this
  type Self
  type Codec
  def encode(value: Self): Codec
  def omit(value: Self): Boolean = false

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Encodable in Codec =
    new Encodable:
      type Self = SelfType2
      type Codec = encodable.Codec

      def encode(value: Self): Codec = encodable.encode(lambda(value))
      override def omit(value: Self): Boolean = encodable.omit(lambda(value))
