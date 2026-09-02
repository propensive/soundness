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
  // Opaque `List` likewise; subtype-parametric for `List[e] & Populated` receivers.
  given list: [element, list <: List[element]] => list is Traversable by element =
    List.iterator(_)

  // Opaque `Chain` likewise; its `iterator` is lazy (pulls elements on demand).
  given chain: [element, chain <: Chain[element]] => chain is Traversable by element =
    Chain.iterator(_)

  // Opaque `Sequence` likewise; subtype-parametric for `Sequence[e] & Populated` receivers.
  given sequence: [element, sequence <: Sequence[element]] => sequence is Traversable by element =
    Sequence.iterator(_)

  // Opaque `Set` likewise.
  given set: [element, set <: Set[element]] => set is Traversable by element =
    Set.iterator(_)

  // Opaque `Map` traverses as its pairs.
  given map: [key, value] => Map[key, value] is Traversable by (key, value) =
    Map.iterator(_)

  given ledger: [key, value] => Ledger[key, value] is Traversable by (key, value) =
    Ledger.iterator(_)

  // The frozen array, and any other readable reference: reading through a shared reference
  // is sound under separation checking (live writers are excluded wherever readers alias),
  // and inline re-elaboration freshens even statically-frozen references to `any.rd`, so
  // the receiver admits the whole read-only spectrum. Capture-set polymorphism (rather
  // than a fixed `any.rd` bound) lets boxed capture variables instantiate too.
  given frozenArray: [element, refs^] => (Array[element]^{refs}) is Traversable by element =
    array =>
      scala.collection.immutable.ArraySeq.unsafeWrapArray(array.asInstanceOf[scala.Array[element]])
      . iterator

  // Java collections traverse through `.asScala`, so `javaCollection.to[List]` (and `[Set]`,
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
