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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package proscenium

export scala.collection.immutable.Vector as Trie
export Predef.runtimeChecked as absolve
export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{ListMap, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap
export murmuration.{List, Nil, `::`, `:+`, `+:`, IterableOnce, Set, Series, Map, IArray}

// `scala` was removed from `-Yimports` so the project's `List`/`Iterable`/`Set` win unambiguously
// over the standard library's. Proscenium is now the sole predef and re-exports the scala-package
// prelude the library modules use bare. The collection names `Iterable`/`IterableOnce`/`List`/`Set`
// are intentionally NOT re-exported here — murmuration owns those. (Packages such as `scala.math`
// and `scala.compiletime` cannot be re-exported, so sites using `scala.math.…`/`scala.compiletime.…` etc. get
// an explicit `import scala.…` instead.)
export scala.{Any, AnyRef, AnyVal, Matchable, Nothing, Null, Singleton, Equals, Product,
    Serializable, Cloneable, Int, Long, Short, Byte, Double, Float, Char, Boolean, Option,
    Some, None, Either, Left, Right, Tuple, EmptyTuple, Function, PartialFunction, Conversion,
    Selectable, Dynamic, CanEqual, Specializable, MatchError, ValueOf, CanThrow, AnyKind, NamedTuple, Precise, Seq, IndexedSeq, Iterator, Vector, Array, StringContext, Symbol,
    deprecated, inline, throws, unchecked, specialized, volatile, transient, native,
    SerialVersionUID}

export scala.math.{Ordering, Numeric, Integral, Fractional, BigInt, BigDecimal}
export scala.{`&`, `|`, `<:<`, `=:=`, `*:`}
export scala.unsafeExceptions
export scala.collection.mutable.StringBuilder
export scala.collection.immutable.Range
export scala.collection.immutable.LazyList.`#::`
export scala.collection.StringOps

export Predef
. { nn, identity, summon, charWrapper, $conforms, ArrowAssoc, intWrapper, longWrapper,
    shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper, locally, is,
    refArrayOps, genericArrayOps, byteArrayOps, shortArrayOps, intArrayOps, longArrayOps,
    floatArrayOps, doubleArrayOps, charArrayOps, booleanArrayOps, unitArrayOps,
    augmentString, `???`, classOf, assert }

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export scala.jdk.CollectionConverters
. { IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava, MapHasAsJava,
    EnumerationHasAsScala }

export scala.annotation
. { tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation }

export scala.annotation.unchecked.{uncheckedVariance, uncheckedCaptures, uncheckedStable}

export scala.LazyList as Stream
export scala.LazyList
export scala.DummyImplicit as Void
export scala.DummyImplicit

export Conversion.into

type Unit = scala.Unit
type Nat = Int & Singleton
type Label = String & Singleton

@targetName("partialFn")
infix type ~> [-domain, +range] = PartialFunction[domain, range]

export scala.EmptyTuple as Zero

object Mono:
  inline def apply[value](value: value): Mono[value] = value *: Zero

type Mono[value] = value *: Zero

transparent inline def infer[context]: context = scala.compiletime.summonInline[context]

transparent inline def provide[context](using erased Void)[result]
  ( inline lambda: context ?=> result )
:   result =

  lambda(using infer[context])
