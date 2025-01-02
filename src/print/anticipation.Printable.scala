/*
    Anticipation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.annotation.*

object Printable:
  given Text is Printable = (text, termcap) => text
  given String is Printable = (string, termcap) => string.tt
  given Char is Printable = (char, termcap) => char.toString.tt

trait Printable:
  type Self
  def print(text: Self, termcap: Termcap): Text

  def contramap[SelfType](lambda: Termcap ?=> SelfType => Self): SelfType is Printable =
    (self, termcap) => print(lambda(using termcap)(self), termcap)