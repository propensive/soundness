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

import anticipation.*
import denominative.*

object Segmentable:
  given [ElementType] => IndexedSeq[ElementType] is Segmentable =
    (seq, interval) => seq.slice(interval.start.n0, interval.end.n0)

  given [ElementType] => IArray[ElementType] is Segmentable as iarray =
    (iarray, interval) => iarray.slice(interval.start.n0, interval.end.n0)

  given Text is Segmentable = (text, interval) =>
    text.s.substring(interval.start.n0, interval.end.n0).nn.tt

trait Segmentable:
  type Self
  def segment(entity: Self, interval: Interval): Self
