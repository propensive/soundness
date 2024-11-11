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

import language.experimental.captureChecking

import vacuous.*

object Cursor:
  opaque type Cursor = Int
  opaque type CursorSeq[T] <: IndexedSeq[T] = IndexedSeq[T]

  extension (cursor: Cursor)
    inline def index: Int = cursor

    inline def of[ElemType](inline seq: CursorSeq[ElemType]): ElemType = seq(cursor.index)

    inline def of[ElemType](inline seq: CursorSeq[ElemType], inline offset: Int): Optional[ElemType] =
      if (cursor.index + offset) >= 0 && (cursor.index + offset) < seq.length then seq(cursor.index + offset)
      else Unset

  inline def curse[ElemType, ElemType2](seq: IndexedSeq[ElemType])
     (inline block: (CursorSeq[ElemType], Cursor) ?=> ElemType2)
          : IndexedSeq[ElemType2] =

    seq.indices.map { index => block(using seq, index) }
