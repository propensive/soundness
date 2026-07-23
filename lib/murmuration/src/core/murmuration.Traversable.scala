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
package murmuration

import anticipation.*
import prepositional.*

// A type that can be traversed as a sequence of its elements (its `Operand`, bound
// with `by` — e.g. `List[Int] is Traversable by Int`). It is the basis for
// element-oriented operations like `where` that need to visit elements in order and
// short-circuit, and for the transforming operations (`map`, `filter`, …) built with
// `Reshapable`. `traverse` returns a fresh, one-shot `Iterator`, which callers
// consume only as far as they need; it is internal currency, never user-facing.
// The blanket `Iterable` instance lives in a parent trait so per-alias instances in the
// object take priority (the compiler otherwise reports an ambiguity for alias receivers).
transparent trait Traversable2:
  given iterable: [element, collection <: Iterable[element]]
  =>  collection is Traversable by element =
    _.iterator

object Traversable extends Traversable2:

  // `Text` (opaque over `String`) is not an `Iterable`, so it needs its own instance;
  // placing it here (the typeclass companion) keeps it in implicit scope for
  // `Text is Traversable` without an explicit `import`, unlike a top-level given. `Self` is
  // subtype-parametric so intersections like `Text & Populated` (from `occupied`) also match.
  given text: [text <: Text] => text is Traversable by Char = _.s.iterator

  // Opaque `List` likewise; subtype-parametric for `List[e] & Populated` receivers.
  given list: [element, list <: List[element]] => list is Traversable by element =
    _.stdlib.iterator

  // Opaque `Progression` likewise; `.stdlib.iterator` is lazy (pulls elements on demand).
  given lazyList: [element, lazyList <: Progression[element]] => lazyList is Traversable by element =
    _.stdlib.iterator

  // Opaque `Series` likewise; subtype-parametric for `Series[e] & Populated` receivers.
  given series: [element, series <: Series[element]] => series is Traversable by element =
    _.stdlib.iterator

  // Opaque `Set` likewise.
  given set: [element, set <: Set[element]] => set is Traversable by element =
    _.stdlib.iterator

  // Opaque `Map` traverses as its pairs.
  given map: [key, value] => Map[key, value] is Traversable by (key, value) =
    _.stdlib.iterator

  // `IArray` is not an `Iterable`, so the blanket instance cannot serve it; the immutable
  // wrapper is allocation-free and the cast never escapes.
  given iarray: [element] => IArray[element] is Traversable by element =
    iarray =>
      scala.collection.immutable.ArraySeq.unsafeWrapArray(iarray.asInstanceOf[Array[element]])
      . iterator

  // Java collections traverse through `.asScala`, so `javaCollection.transmute[List]` (and `[Set]`,
  // `[Map]`, …) work with no explicit conversion — the builder is the standard-library converter.
  // `java.lang.Iterable` covers `java.util.List`/`Set`/`Collection`/…; `java.util.Iterator` and
  // `java.util.Map` (traversed as its pairs) are neither, so they get their own instances.
  given javaIterable: [element, collection <: java.lang.Iterable[element]]
  =>  collection is Traversable by element =
    collection => scala.jdk.CollectionConverters.IterableHasAsScala(collection).asScala.iterator

  given javaIterator: [element] => java.util.Iterator[element] is Traversable by element =
    iterator => scala.jdk.CollectionConverters.IteratorHasAsScala(iterator).asScala

  given javaMap: [key, value] => java.util.Map[key, value] is Traversable by (key, value) =
    map => scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.iterator

  given javaEnumeration: [element] => java.util.Enumeration[element] is Traversable by element =
    enumeration => scala.jdk.CollectionConverters.EnumerationHasAsScala(enumeration).asScala

trait Traversable extends Typeclass.Pure, Operable:
  def traverse(self: Self): Iterator[Operand]
