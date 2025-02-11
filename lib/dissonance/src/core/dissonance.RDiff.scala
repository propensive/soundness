/*
    Dissonance, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import language.experimental.captureChecking

import vacuous.*

case class RDiff[ElemType](changes: Change[ElemType]*):
  def flip: RDiff[Optional[ElemType]] =
    val changes2: Seq[Change[Optional[ElemType]]] = changes.map:
      case Par(left, right, value)                 => Par(right, left, value)
      case Del(left, value)                        => Ins(left, value)
      case Ins(right, value)                       => Del(right, value)
      case Sub(left, right, leftValue, rightValue) => Sub(right, left, rightValue, leftValue)

    RDiff(changes2*)

  def map[ElemType2](lambda: ElemType => ElemType2): RDiff[ElemType2^{lambda}] =
    RDiff(changes.map(_.map(lambda))*)
