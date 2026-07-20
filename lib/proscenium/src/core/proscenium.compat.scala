                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package proscenium.compat

import scala.math

import scala.collection.immutable as sci

import proscenium.{List, Map, Set, Progression}

// MIGRATION SHIMS — temporarily restore the stdlib surface of the opaque `Set` so call sites
// compile unchanged, one `import proscenium.compat.*` per file. Each shim is an independently
// deletable inline forwarder; the drain loop is deprecate → fix call sites → delete. This file's
// emptiness is the completion signal for the `Set` migration. Not auto-imported: `-Yimports`
// pulls only top-level `proscenium` members, so every use is greppable.
extension [element](set: Set[element])
  inline def apply(element: element): Boolean = set.stdlib(element)
  inline def forall(predicate: element => Boolean): Boolean = set.stdlib.forall(predicate)
  inline def count(predicate: element => Boolean): Int = set.stdlib.count(predicate)
  inline def find(predicate: element => Boolean): Option[element] = set.stdlib.find(predicate)
  inline def isEmpty: Boolean = set.stdlib.isEmpty
  inline def nonEmpty: Boolean = set.stdlib.nonEmpty
  inline def size: Int = set.stdlib.size
  inline def head: element = set.stdlib.head
  inline def toList: List[element] = List.of(set.stdlib.toList)
  inline def toSeq: Seq[element] = set.stdlib.toSeq
  inline def toSet: sci.Set[element] = set.stdlib
  inline def iterator: Iterator[element] = set.stdlib.iterator
  inline def mkString(sep: String): String = set.stdlib.mkString(sep)

  inline def mkString(start: String, sep: String, end: String): String =
    set.stdlib.mkString(start, sep, end)

  inline def minBy[key](lambda: element => key)(using math.Ordering[key]): element =
    set.stdlib.minBy(lambda)

  inline infix def + (element: element): Set[element] = Set.of(set.stdlib + element)
  inline infix def - (element: element): Set[element] = Set.of(set.stdlib - element)

  inline infix def ++ (elements: IterableOnce[element]): Set[element] =
    Set.of(set.stdlib ++ elements)

  inline infix def ++ (other: Set[element]): Set[element] = Set.of(set.stdlib ++ other.stdlib)

  inline infix def -- (elements: IterableOnce[element]): Set[element] =
    Set.of(set.stdlib -- elements)

  inline infix def -- (other: Set[element]): Set[element] = Set.of(set.stdlib -- other.stdlib)
  inline def intersect(other: Set[element]): Set[element] = Set.of(set.stdlib.intersect(other.stdlib))
  inline def union(other: Set[element]): Set[element] = Set.of(set.stdlib.union(other.stdlib))
  inline def diff(other: Set[element]): Set[element] = Set.of(set.stdlib.diff(other.stdlib))
  inline def subsetOf(other: Set[element]): Boolean = set.stdlib.subsetOf(other.stdlib)
  inline def filterNot(predicate: element => Boolean): Set[element] = Set.of(set.stdlib.filterNot(predicate))

  inline def partition(predicate: element => Boolean): (Set[element], Set[element]) =
    val (left, right) = set.stdlib.partition(predicate)
    (Set.of(left), Set.of(right))

extension [key, value](map: Map[key, value])
  inline def get(key: key): Option[value] = map.stdlib.get(key)

  // NO `getOrElse` shim: its by-name default parameter in an inline extension crashes the capture
  // checker's Setup phase (boxDeeply assertion); call sites bridge via `stdlib` instead.

  inline def apply(key: key): value = map.stdlib(key)
  inline def keySet: Set[key] = Set.of(map.stdlib.keySet)
  inline def keys: Iterable[key] = map.stdlib.keys
  inline def values: Iterable[value] = map.stdlib.values
  inline def isEmpty: Boolean = map.stdlib.isEmpty
  inline def nonEmpty: Boolean = map.stdlib.nonEmpty
  inline def size: Int = map.stdlib.size
  inline def iterator: Iterator[(key, value)] = map.stdlib.iterator
  inline def toList: List[(key, value)] = List.of(map.stdlib.toList)
  inline def toSeq: Seq[(key, value)] = map.stdlib.toSeq
  inline def toMap: sci.Map[key, value] = map.stdlib
  inline def find(predicate: ((key, value)) => Boolean): Option[(key, value)] =
    map.stdlib.find(predicate)

  inline def forall(predicate: ((key, value)) => Boolean): Boolean = map.stdlib.forall(predicate)
  inline def count(predicate: ((key, value)) => Boolean): Int = map.stdlib.count(predicate)

  inline def updated[value2 >: value](key: key, value: value2): Map[key, value2] =
    Map.of(map.stdlib.updated(key, value))

  inline def removed(key: key): Map[key, value] = Map.of(map.stdlib.removed(key))

  inline def concat[value2 >: value](other: Map[key, value2]): Map[key, value2] =
    Map.of(map.stdlib.concat(other.stdlib))

  inline def filterNot(predicate: ((key, value)) => Boolean): Map[key, value] =
    Map.of(map.stdlib.filterNot(predicate))

  inline def collect[result](lambda: PartialFunction[(key, value), result]): Iterable[result] =
    map.stdlib.collect(lambda)

  inline def mkString(sep: String): String = map.stdlib.mkString(sep)

// MIGRATION SHIMS for the opaque `List`, following the same drain loop as `Set` and `Map`.
// Notable omissions: `getOrElse`-style by-name defaults (cc boxer crash), `++`/`contains`
// (owned by symbolism/gossamer extensions for other receivers — call sites bridge or use
// house names), and `to(...)` (already supported via the companion `Factory` conversion).
extension [element](list: List[element])
  inline def filterNot(predicate: element => Boolean): List[element] =
    List.of(list.stdlib.filterNot(predicate))

  inline def forall(predicate: element => Boolean): Boolean = list.stdlib.forall(predicate)
  inline def count(predicate: element => Boolean): Int = list.stdlib.count(predicate)
  inline def find(predicate: element => Boolean): Option[element] = list.stdlib.find(predicate)

  inline def collect[element2](lambda: PartialFunction[element, element2]): List[element2] =
    List.of(list.stdlib.collect(lambda))

  inline def collectFirst[element2](lambda: PartialFunction[element, element2]): Option[element2] =
    list.stdlib.collectFirst(lambda)

  inline def foldRight[state](initial: state)(lambda: (element, state) => state): state =
    list.stdlib.foldRight(initial)(lambda)

  inline def zipWithIndex: List[(element, Int)] = List.of(list.stdlib.zipWithIndex)
  inline def head: element = list.stdlib.head
  inline def headOption: Option[element] = list.stdlib.headOption
  inline def last: element = list.stdlib.last
  inline def lastOption: Option[element] = list.stdlib.lastOption
  inline def tail: List[element] = List.of(list.stdlib.tail)
  inline def init: List[element] = List.of(list.stdlib.init)
  inline def take(count: Int): List[element] = List.of(list.stdlib.take(count))
  inline def drop(count: Int): List[element] = List.of(list.stdlib.drop(count))
  inline def takeRight(count: Int): List[element] = List.of(list.stdlib.takeRight(count))
  inline def dropRight(count: Int): List[element] = List.of(list.stdlib.dropRight(count))

  inline def takeWhile(predicate: element => Boolean): List[element] =
    List.of(list.stdlib.takeWhile(predicate))

  inline def dropWhile(predicate: element => Boolean): List[element] =
    List.of(list.stdlib.dropWhile(predicate))

  inline def span(predicate: element => Boolean): (List[element], List[element]) =
    val (left, right) = list.stdlib.span(predicate)
    (List.of(left), List.of(right))

  inline def splitAt(index: Int): (List[element], List[element]) =
    val (left, right) = list.stdlib.splitAt(index)
    (List.of(left), List.of(right))

  inline def partition(predicate: element => Boolean): (List[element], List[element]) =
    val (left, right) = list.stdlib.partition(predicate)
    (List.of(left), List.of(right))

  inline def reverse: List[element] = List.of(list.stdlib.reverse)
  inline def isEmpty: Boolean = list.stdlib.isEmpty
  inline def nonEmpty: Boolean = list.stdlib.nonEmpty
  inline def length: Int = list.stdlib.length
  inline def size: Int = list.stdlib.size
  inline def mkString: String = list.stdlib.mkString
  inline def mkString(separator: String): String = list.stdlib.mkString(separator)

  inline def mkString(start: String, separator: String, end: String): String =
    list.stdlib.mkString(start, separator, end)

  inline def sorted(using math.Ordering[element]): List[element] = List.of(list.stdlib.sorted)
  inline def toSet: Set[element] = Set.of(list.stdlib.toSet)
  inline def toSeq: Seq[element] = list.stdlib
  inline def toList: List[element] = list
  inline def iterator: Iterator[element] = list.stdlib.iterator
  inline def indexOf(element: element): Int = list.stdlib.indexOf(element)
  inline def indexWhere(predicate: element => Boolean): Int = list.stdlib.indexWhere(predicate)
  inline def apply(index: Int): element = list.stdlib.apply(index)
  inline def updated(index: Int, element2: element): List[element] =
    List.of(list.stdlib.updated(index, element2))

  infix def ::: [element2 >: element](suffix: List[element2]): List[element2] =
    List.of(list.stdlib ::: suffix.stdlib)

  inline infix def :+ [element2 >: element](element2Value: element2): List[element2] =
    List.of(list.stdlib :+ element2Value)

extension [element](head: element)
  infix def +: [element2 >: element](list: List[element2]): List[element2] =
    List.of(head +: list.stdlib)

extension [element](list: List[element])

  inline def grouped(count: Int): Iterator[List[element]] =
    list.stdlib.grouped(count).map(List.of(_))

  inline def sliding(count: Int): Iterator[List[element]] =
    list.stdlib.sliding(count).map(List.of(_))

  inline def maxBy[key](lambda: element => key)(using math.Ordering[key]): element =
    list.stdlib.maxBy(lambda)

  inline def minBy[key](lambda: element => key)(using math.Ordering[key]): element =
    list.stdlib.minBy(lambda)

  inline def max(using math.Ordering[element]): element = list.stdlib.max
  inline def min(using math.Ordering[element]): element = list.stdlib.min
  inline def sum(using numeric: scala.math.Numeric[element]): element = list.stdlib.sum

  inline def reduce[element2 >: element](lambda: (element2, element2) => element2): element2 =
    list.stdlib.reduce(lambda)

extension [element](list: List[List[element]])
  inline def flatten: List[element] = List.of(list.stdlib.flatten(_.stdlib))

extension [key, value](list: List[(key, value)])
  inline def toMap: Map[key, value] = Map.of(list.stdlib.toMap)
  inline def unzip: (List[key], List[value]) =
    val (keys, values) = list.stdlib.unzip(using pair => pair)
    (List.of(keys), List.of(values))

// MIGRATION SHIMS for the opaque `Progression`. Non-umbrella names only — `map`/`filter`/`flatMap`/
// `fold`/`each` come from the typeclass surface (`Traversable`/`Reshapable`). Forcing operations
// (`length`/`size`) are deliberately NOT shimmed: they must announce themselves via `.stdlib` so
// the `Exhaust`-gated `Countable.lazyList` is not silently bypassed. Laziness is preserved: `tail`,
// `take`, `drop`, `takeWhile`, `dropWhile` and `lazyAppendedAll` do not force the stream's tail.
extension [element](lazyList: Progression[element])
  inline def head: element = lazyList.stdlib.head
  inline def headOption: Option[element] = lazyList.stdlib.headOption
  inline def tail: Progression[element] = Progression.of(lazyList.stdlib.tail)
  inline def isEmpty: Boolean = lazyList.stdlib.isEmpty
  inline def nonEmpty: Boolean = lazyList.stdlib.nonEmpty
  inline def iterator: Iterator[element] = lazyList.stdlib.iterator
  inline def take(count: Int): Progression[element] = Progression.of(lazyList.stdlib.take(count))
  inline def drop(count: Int): Progression[element] = Progression.of(lazyList.stdlib.drop(count))

  inline def takeWhile(predicate: element => Boolean): Progression[element] =
    Progression.of(lazyList.stdlib.takeWhile(predicate))

  inline def dropWhile(predicate: element => Boolean): Progression[element] =
    Progression.of(lazyList.stdlib.dropWhile(predicate))

  // By-name suffix keeps the append lazy, mirroring the permanent `#:::` operator.
  inline def lazyAppendedAll[element2 >: element](suffix: => Progression[element2]): Progression[element2] =
    Progression.of(lazyList.stdlib.lazyAppendedAll(suffix.stdlib))

  // `init` stays lazy in its prefix; `find`/`collectFirst`/`forall`/`count`/`foldLeft` force
  // (finite-stream idioms during migration — the drain replaces them with gated/typeclass forms).
  inline def init: Progression[element] = Progression.of(lazyList.stdlib.init)
  inline def find(predicate: element => Boolean): Option[element] = lazyList.stdlib.find(predicate)
  inline def forall(predicate: element => Boolean): Boolean = lazyList.stdlib.forall(predicate)
  inline def count(predicate: element => Boolean): Int = lazyList.stdlib.count(predicate)

  inline def collectFirst[element2](lambda: PartialFunction[element, element2]): Option[element2] =
    lazyList.stdlib.collectFirst(lambda)

  inline def foldLeft[state](initial: state)(lambda: (state, element) => state): state =
    lazyList.stdlib.foldLeft(initial)(lambda)

  inline def filterNot(predicate: element => Boolean): Progression[element] =
    Progression.of(lazyList.stdlib.filterNot(predicate))
