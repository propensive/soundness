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

import scala.annotation.targetName
import scala.caps
import scala.math

import scala.collection.immutable as sci

import proscenium.{Array, Ledger, List, Map, Set, Chain, Sequence}

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

// The `Ledger` twin of the `Map` block above: the same surface, closed over the
// insertion-ordered type, with the same omissions for the same reasons (no `getOrElse` —
// cc boxer crash on by-name defaults in inline extensions — so call sites bridge via
// `stdlib` or `at(...).or(...)`).
extension [key, value](ledger: Ledger[key, value])
  inline def get(key: key): Option[value] = ledger.stdlib.get(key)
  inline def apply(key: key): value = ledger.stdlib(key)
  inline def keySet: Set[key] = Set.of(ledger.stdlib.keySet)
  inline def keys: Iterable[key] = ledger.stdlib.keys
  inline def values: Iterable[value] = ledger.stdlib.values
  inline def isEmpty: Boolean = ledger.stdlib.isEmpty
  inline def nonEmpty: Boolean = ledger.stdlib.nonEmpty
  inline def size: Int = ledger.stdlib.size
  inline def iterator: Iterator[(key, value)] = ledger.stdlib.iterator
  inline def toList: List[(key, value)] = List.of(ledger.stdlib.toList)
  inline def toSeq: Seq[(key, value)] = ledger.stdlib.toSeq
  inline def toMap: Map[key, value] = Map.of(ledger.stdlib)
  inline def find(predicate: ((key, value)) => Boolean): Option[(key, value)] =
    ledger.stdlib.find(predicate)

  inline def forall(predicate: ((key, value)) => Boolean): Boolean = ledger.stdlib.forall(predicate)
  inline def count(predicate: ((key, value)) => Boolean): Int = ledger.stdlib.count(predicate)

  inline def updated[value2 >: value](key: key, value: value2): Ledger[key, value2] =
    Ledger.of(ledger.stdlib.updated(key, value))

  inline def removed(key: key): Ledger[key, value] = Ledger.of(ledger.stdlib.removed(key))

  inline def concat[value2 >: value](other: Ledger[key, value2]): Ledger[key, value2] =
    Ledger.of(ledger.stdlib.concat(other.stdlib))

  inline def filterNot(predicate: ((key, value)) => Boolean): Ledger[key, value] =
    Ledger.of(ledger.stdlib.filterNot(predicate))

  inline def collect[result](lambda: PartialFunction[(key, value), result]): Iterable[result] =
    ledger.stdlib.collect(lambda)

  inline def mkString(sep: String): String = ledger.stdlib.mkString(sep)

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

// MIGRATION SHIMS for the opaque `Chain`. Non-umbrella names only — `map`/`filter`/`flatMap`/
// `fold`/`each` come from the typeclass surface (`Traversable`/`Reshapable`). Forcing operations
// (`length`/`size`) are deliberately NOT shimmed: they must announce themselves via `.stdlib` so
// the `UnboundedSizeComplexity`-gated `Countable.lazyList` is not silently bypassed. Laziness is
// preserved: `tail`,
// `take`, `drop`, `takeWhile`, `dropWhile` and `lazyAppendedAll` do not force the stream's tail.
extension [element](lazyList: Chain[element])
  inline def head: element = lazyList.stdlib.head
  inline def headOption: Option[element] = lazyList.stdlib.headOption
  inline def tail: Chain[element] = Chain.of(lazyList.stdlib.tail)
  inline def isEmpty: Boolean = lazyList.stdlib.isEmpty
  inline def nonEmpty: Boolean = lazyList.stdlib.nonEmpty
  inline def iterator: Iterator[element] = lazyList.stdlib.iterator
  inline def take(count: Int): Chain[element] = Chain.of(lazyList.stdlib.take(count))
  inline def drop(count: Int): Chain[element] = Chain.of(lazyList.stdlib.drop(count))

  inline def takeWhile(predicate: element => Boolean): Chain[element] =
    Chain.of(lazyList.stdlib.takeWhile(predicate))

  inline def dropWhile(predicate: element => Boolean): Chain[element] =
    Chain.of(lazyList.stdlib.dropWhile(predicate))

  // By-name suffix keeps the append lazy, mirroring the permanent `#:::` operator.
  inline def lazyAppendedAll[element2 >: element](suffix: => Chain[element2]): Chain[element2] =
    Chain.of(lazyList.stdlib.lazyAppendedAll(suffix.stdlib))

  // `init` stays lazy in its prefix; `find`/`collectFirst`/`forall`/`count`/`foldLeft` force
  // (finite-stream idioms during migration — the drain replaces them with gated/typeclass forms).
  inline def init: Chain[element] = Chain.of(lazyList.stdlib.init)
  inline def find(predicate: element => Boolean): Option[element] = lazyList.stdlib.find(predicate)
  inline def forall(predicate: element => Boolean): Boolean = lazyList.stdlib.forall(predicate)
  inline def count(predicate: element => Boolean): Int = lazyList.stdlib.count(predicate)

  inline def collectFirst[element2](lambda: PartialFunction[element, element2]): Option[element2] =
    lazyList.stdlib.collectFirst(lambda)

  inline def foldLeft[state](initial: state)(lambda: (state, element) => state): state =
    lazyList.stdlib.foldLeft(initial)(lambda)

  inline def filterNot(predicate: element => Boolean): Chain[element] =
    Chain.of(lazyList.stdlib.filterNot(predicate))

// MIGRATION SHIMS for the opaque `Sequence` (the blessed `Vector`), giving it the same
// transitional surface as `List`, with the same deliberate omissions (`getOrElse`-style
// by-name defaults, `++`/`contains`, `to(...)`); `:::` is the concatenation shim, matching
// the `List` block, and `:+`/`+:` cover the ends a `Vector` amortizes.
extension [element](sequence: Sequence[element])
  inline def filterNot(predicate: element => Boolean): Sequence[element] =
    Sequence.of(sequence.stdlib.filterNot(predicate))

  inline def forall(predicate: element => Boolean): Boolean = sequence.stdlib.forall(predicate)
  inline def count(predicate: element => Boolean): Int = sequence.stdlib.count(predicate)
  inline def find(predicate: element => Boolean): Option[element] = sequence.stdlib.find(predicate)

  inline def collect[element2](lambda: PartialFunction[element, element2]): Sequence[element2] =
    Sequence.of(sequence.stdlib.collect(lambda))

  inline def collectFirst[element2](lambda: PartialFunction[element, element2]): Option[element2] =
    sequence.stdlib.collectFirst(lambda)

  inline def foldLeft[state](initial: state)(lambda: (state, element) => state): state =
    sequence.stdlib.foldLeft(initial)(lambda)

  inline def foldRight[state](initial: state)(lambda: (element, state) => state): state =
    sequence.stdlib.foldRight(initial)(lambda)

  inline def zipWithIndex: Sequence[(element, Int)] = Sequence.of(sequence.stdlib.zipWithIndex)
  inline def head: element = sequence.stdlib.head
  inline def headOption: Option[element] = sequence.stdlib.headOption
  inline def last: element = sequence.stdlib.last
  inline def lastOption: Option[element] = sequence.stdlib.lastOption
  inline def tail: Sequence[element] = Sequence.of(sequence.stdlib.tail)
  inline def init: Sequence[element] = Sequence.of(sequence.stdlib.init)
  inline def take(count: Int): Sequence[element] = Sequence.of(sequence.stdlib.take(count))
  inline def drop(count: Int): Sequence[element] = Sequence.of(sequence.stdlib.drop(count))
  inline def takeRight(count: Int): Sequence[element] = Sequence.of(sequence.stdlib.takeRight(count))
  inline def dropRight(count: Int): Sequence[element] = Sequence.of(sequence.stdlib.dropRight(count))

  inline def takeWhile(predicate: element => Boolean): Sequence[element] =
    Sequence.of(sequence.stdlib.takeWhile(predicate))

  inline def dropWhile(predicate: element => Boolean): Sequence[element] =
    Sequence.of(sequence.stdlib.dropWhile(predicate))

  inline def span(predicate: element => Boolean): (Sequence[element], Sequence[element]) =
    val (left, right) = sequence.stdlib.span(predicate)
    (Sequence.of(left), Sequence.of(right))

  inline def splitAt(index: Int): (Sequence[element], Sequence[element]) =
    val (left, right) = sequence.stdlib.splitAt(index)
    (Sequence.of(left), Sequence.of(right))

  inline def partition(predicate: element => Boolean): (Sequence[element], Sequence[element]) =
    val (left, right) = sequence.stdlib.partition(predicate)
    (Sequence.of(left), Sequence.of(right))

  inline def isEmpty: Boolean = sequence.stdlib.isEmpty
  inline def nonEmpty: Boolean = sequence.stdlib.nonEmpty
  inline def length: Int = sequence.stdlib.length
  inline def size: Int = sequence.stdlib.size
  inline def mkString: String = sequence.stdlib.mkString
  inline def mkString(separator: String): String = sequence.stdlib.mkString(separator)

  inline def mkString(start: String, separator: String, end: String): String =
    sequence.stdlib.mkString(start, separator, end)

  inline def sorted(using math.Ordering[element]): Sequence[element] = Sequence.of(sequence.stdlib.sorted)
  inline def toSet: Set[element] = Set.of(sequence.stdlib.toSet)
  inline def toSeq: Seq[element] = sequence.stdlib
  inline def toList: List[element] = List.of(sequence.stdlib.toList)
  inline def iterator: Iterator[element] = sequence.stdlib.iterator
  inline def indexOf(element: element): Int = sequence.stdlib.indexOf(element)
  inline def indexWhere(predicate: element => Boolean): Int = sequence.stdlib.indexWhere(predicate)
  inline def apply(index: Int): element = sequence.stdlib.apply(index)

  inline def updated(index: Int, element2: element): Sequence[element] =
    Sequence.of(sequence.stdlib.updated(index, element2))

  inline def slice(from: Int, until: Int): Sequence[element] =
    Sequence.of(sequence.stdlib.slice(from, until))

  infix def ::: [element2 >: element](suffix: Sequence[element2]): Sequence[element2] =
    Sequence.of(sequence.stdlib ++ suffix.stdlib)

  inline infix def :+ [element2 >: element](element2Value: element2): Sequence[element2] =
    Sequence.of(sequence.stdlib :+ element2Value)

  inline infix def +: [element2 >: element](element2Value: element2): Sequence[element2] =
    Sequence.of(element2Value +: sequence.stdlib)

// MIGRATION SHIMS for the frozen array, `Array[element]^{}`, following the same drain
// loop as the other blessed types -- anchored at `^{caps.any.rd}` receivers so frozen,
// shared and exclusive references all subsume. The `@targetName`s date from the interim
// period when a parallel `IArray` shim block shared these names at the same erasure; they
// are retained as the shims' stable binary names. Reads
// delegate through the read-only `readable` view, and constructive results come back
// frozen via `Array.frozen`. `length` is deliberately absent: the core companion already
// serves it for every reference.
extension [element](array: Array[element]^{caps.any.rd})
  @targetName("frozenApply")
  inline def apply(index: Int): element = array.readable(index)
  @targetName("frozenSize")
  inline def size: Int = array.readable.size
  @targetName("frozenIsEmpty")
  inline def isEmpty: Boolean = array.readable.isEmpty
  @targetName("frozenNonEmpty")
  inline def nonEmpty: Boolean = array.readable.nonEmpty
  @targetName("frozenHead")
  inline def head: element = array.readable.head
  @targetName("frozenHeadOption")
  inline def headOption: Option[element] = array.readable.headOption
  @targetName("frozenLast")
  inline def last: element = array.readable.last
  @targetName("frozenLastOption")
  inline def lastOption: Option[element] = array.readable.lastOption
  @targetName("frozenIndices")
  inline def indices: Range = array.readable.indices
  @targetName("frozenIterator")
  inline def iterator: Iterator[element] = array.readable.iterator
  @targetName("frozenCount")
  inline def count(predicate: element => Boolean): Int = array.readable.count(predicate)

  @targetName("frozenFind")
  inline def find(predicate: element => Boolean): Option[element] =
    array.readable.find(predicate)

  @targetName("frozenIndexWhere")
  inline def indexWhere(predicate: element => Boolean): Int =
    array.readable.indexWhere(predicate)

  @targetName("frozenFoldLeft")
  inline def foldLeft[state](initial: state)(lambda: (state, element) => state): state =
    array.readable.foldLeft(initial)(lambda)

  @targetName("frozenMkString")
  inline def mkString: String = array.readable.mkString
  @targetName("frozenMkString1")
  inline def mkString(separator: String): String = array.readable.mkString(separator)

  @targetName("frozenMkString2")
  inline def mkString(start: String, separator: String, end: String): String =
    array.readable.mkString(start, separator, end)

  @targetName("frozenToSeq")
  inline def toSeq: Seq[element] = array.readable.toSeq
  @targetName("frozenToList")
  inline def toList: List[element] = List.of(array.readable.toList)
  @targetName("frozenToSet")
  inline def toSet: Set[element] = Set.of(array.readable.toSet)

  @targetName("frozenMap")
  inline def map[element2: scala.reflect.ClassTag](lambda: element => element2)
  :   Array[element2]^{} =
    Array.frozen(array.readable.map(lambda))

  @targetName("frozenTake")
  inline def take(count: Int)(using scala.reflect.ClassTag[element]): Array[element]^{} =
    Array.frozen(array.readable.take(count))

  @targetName("frozenDrop")
  inline def drop(count: Int)(using scala.reflect.ClassTag[element]): Array[element]^{} =
    Array.frozen(array.readable.drop(count))

  @targetName("frozenSlice")
  inline def slice(from: Int, until: Int)(using scala.reflect.ClassTag[element])
  :   Array[element]^{} =
    Array.frozen(array.readable.slice(from, until))

  @targetName("frozenUpdated")
  inline def updated[element2 >: element](index: Int, element2: element2)
     (using scala.reflect.ClassTag[element2])
  :   Array[element2]^{} =
    Array.frozen(array.readable.updated(index, element2))

  @targetName("frozenFilterNot")
  inline def filterNot(predicate: element => Boolean)(using scala.reflect.ClassTag[element])
  :   Array[element]^{} =
    Array.frozen(array.readable.filterNot(predicate))

  @targetName("frozenReverse")
  inline def reverse(using scala.reflect.ClassTag[element]): Array[element]^{} =
    Array.frozen(array.readable.reverse)

  @targetName("frozenConcat")
  inline infix def ++ [element2 >: element: scala.reflect.ClassTag]
     (suffix: Array[element2]^{caps.any.rd})
  :   Array[element2]^{} =
    Array.frozen(array.readable ++ suffix.readable)

  @targetName("frozenSameElements")
  inline def sameElements(that: Array[element]^{caps.any.rd}): Boolean =
    array.readable.sameElements(that.readable)

  @targetName("frozenSum")
  inline def sum(using math.Numeric[element]): element = array.readable.sum

  @targetName("frozenContains")
  inline def contains(element2: element): Boolean =
    array.readable.toSeq.contains(element2)

  @targetName("frozenZipWithIndex")
  inline def zipWithIndex: Array[(element, Int)]^{} =
    Array.frozen(array.readable.zipWithIndex)

  @targetName("frozenCollect")
  inline def collect[element2: scala.reflect.ClassTag]
     (lambda: PartialFunction[element, element2])
  :   Array[element2]^{} =
    Array.frozen(array.readable.collect(lambda))

  @targetName("frozenForall")
  inline def forall(predicate: element => Boolean): Boolean = array.readable.forall(predicate)

  @targetName("frozenLastIndexWhere")
  inline def lastIndexWhere(predicate: element => Boolean): Int =
    array.readable.lastIndexWhere(predicate)

  @targetName("frozenIndexOf")
  inline def indexOf(element2: element): Int = array.readable.indexOf(element2)

  @targetName("frozenAppend")
  inline infix def :+ [element2 >: element: scala.reflect.ClassTag](element3: element2)
  :   Array[element2]^{} =
    Array.frozen(array.readable :+ element3)

  @targetName("frozenPrepend")
  inline infix def +: [element2 >: element: scala.reflect.ClassTag](element3: element2)
  :   Array[element2]^{} =
    Array.frozen(element3 +: array.readable)


