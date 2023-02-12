/*
    Rudiments, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

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

extension [ValueType](xs: Iterable[ValueType])
  transparent inline def mtwin: Iterable[(ValueType, ValueType)] = xs.map { x => (x, x) }
  transparent inline def mtriple: Iterable[(ValueType, ValueType, ValueType)] = xs.map { x => (x, x, x) }

  def indexBy[ValueType2](fn: ValueType -> ValueType2): Map[ValueType2, ValueType] throws DuplicateIndexError =
    val map = xs.map: value =>
      (fn(value), value)
    
    if xs.size != map.size then throw DuplicateIndexError() else map.to(Map)

case class KeyNotFoundError(name: Text)
extends Error(ErrorMessage[Text *: EmptyTuple](List(Text("key "), Text(" not found")), name *: EmptyTuple))

case class DuplicateIndexError()
extends Error(ErrorMessage[EmptyTuple](
  List(Text("the sequence contained more than one element that mapped to the same index")), EmptyTuple
))

extension [ElemType](value: IArray[ElemType])
  inline def mutable(using erased Unsafe.type): Array[ElemType] = value match
    case array: Array[ElemType] @unchecked => array
    case _                                  => throw Mistake("Should never match")

extension [ElemType](value: Array[ElemType])
  inline def immutable(using erased Unsafe.type): IArray[ElemType] = value match
    case array: IArray[ElemType] @unchecked => array
    case _                                  => throw Mistake("Should never match")

  def snapshot(using ClassTag[ElemType]): IArray[ElemType] =
    val newArray = new Array[ElemType](value.length)
    System.arraycopy(value, 0, newArray, 0, value.length)
    newArray.immutable(using Unsafe)

extension [KeyType, ValueType](map: Map[KeyType, ValueType])
  def upsert(key: KeyType, op: Maybe[ValueType] => ValueType): Map[KeyType, ValueType] =
    map.updated(key, op(if map.contains(key) then map(key) else Unset))

  def collate(otherMap: Map[KeyType, ValueType])(merge: (ValueType, ValueType) => ValueType)
             : Map[KeyType, ValueType] =
    otherMap.foldLeft(map): (acc, kv) =>
      acc.updated(kv(0), acc.get(kv(0)).fold(kv(1))(merge(kv(1), _)))

extension [K, V](map: Map[K, List[V]])
  def plus(key: K, value: V): Map[K, List[V]] = map.updated(key, map.get(key).fold(List(value))(value :: _))

extension[ElemType](xs: Seq[ElemType])
  def random: ElemType = xs(util.Random().nextInt(xs.length))
  transparent inline def shuffle: Seq[ElemType] = util.Random().shuffle(xs)
  
  def runs(fn: ElemType => Any): List[List[ElemType]] =
    
    @tailrec
    def recur(current: Any, todo: Seq[ElemType], run: List[ElemType], done: List[List[ElemType]])
             : List[List[ElemType]] =
      if todo.isEmpty then (run.reverse :: done).reverse
      else
        val focus = fn(todo.head)
        if current == focus then recur(current, todo.tail, todo.head :: run, done)
        else recur(focus, todo.tail, List(todo.head), run.reverse :: done)

    if xs.isEmpty then Nil else recur(fn(xs.head), xs.tail, List(xs.head), Nil)

object Cursor:
  opaque type Cursor = Int
  opaque type CursorSeq[T] = IndexedSeq[T]

  extension [T](xs: CursorSeq[T])
    def apply(idx: Cursor): T = xs(idx)
    def apply(idx: Cursor, offset: Int): Maybe[T] =
      if (idx + offset) >= 0 && (idx + offset) < xs.length then apply(idx + offset) else Unset
    
  extension (cursor: Cursor) def index: Int = cursor

  transparent inline def curse[T, S](xs: IndexedSeq[T])(inline fn: (CursorSeq[T], Cursor) ?=> S)
                              : IndexedSeq[S] =
    xs.indices.map { i => fn(using xs, i) }

inline def cursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor): ElemType =
  xs(cur)

inline def precursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                    : Maybe[ElemType] =
  xs(cur, -1)

inline def postcursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                     : Maybe[ElemType] =
  xs(cur, 1)

inline def cursorIndex(using inline cur: Cursor.Cursor): Int = cur.index

inline def cursor[ElemType](n: Int)(using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                 : Maybe[ElemType] =
  xs(cur, n)


extension [ElemType](xs: IndexedSeq[ElemType])
  transparent inline def curse[ElemType2](inline fn: (Cursor.CursorSeq[ElemType], Cursor.Cursor) ?=> ElemType2)
                              : IndexedSeq[ElemType2] =
    Cursor.curse(xs)(fn)
