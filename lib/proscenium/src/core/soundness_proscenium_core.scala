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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package soundness

import scala.compiletime

// The collection companions are forwarded as STABLE paths, not exported: an `export` of an
// object makes a `def` forwarder, and a companion member selected through that unstable prefix
// sees the opaque alias's right-hand side — `Sequence.empty[Int].toVector` compiled, exposing
// the whole stdlib API on every value built through the umbrella's companions. The types are
// ordinary parameterised aliases: a bare alias of an opaque constructor (`type List =
// proscenium.List`) is not seen through — `::`, `CanEqual` and the extension methods all
// treat it as a different `List` — whereas the applied alias dealiases as usual, and
// `xs.to[List]` still finds the same constructor through it.
type Sequence[+element] = proscenium.Sequence[element]
val Sequence: proscenium.Sequence.type = proscenium.Sequence
export proscenium.Comparison
export proscenium.Sized
export Predef.runtimeChecked as absolve
export scala.reflect.{ClassTag, Typeable}
type Set[element] = proscenium.Set[element]
val Set: proscenium.Set.type = proscenium.Set
type Map[key, +value] = proscenium.Map[key, value]
val Map: proscenium.Map.type = proscenium.Map
type List[+element] = proscenium.List[element]
val List: proscenium.List.type = proscenium.List
type Chain[+element] = proscenium.Chain[element]
val Chain: proscenium.Chain.type = proscenium.Chain
export proscenium.{Nil, `::`, `#::`}
export scala.collection.immutable.{TreeMap, TreeSet}
export scala.collection.concurrent.TrieMap

export
  Predef
  . { $conforms, ArrowAssoc, assert, augmentString, booleanArrayOps, byteArrayOps,
      byteWrapper, charArrayOps, charWrapper, doubleArrayOps, doubleWrapper,
      floatArrayOps, floatWrapper, genericArrayOps, identity, intArrayOps, intWrapper, is,
      locally, longArrayOps, longWrapper, nn, refArrayOps, shortArrayOps, shortWrapper,
      summon, unitArrayOps, valueOf, `???` }

export proscenium.classOf

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export
  scala.jdk.CollectionConverters
  . { EnumerationHasAsScala, IteratorHasAsScala, ListHasAsScala, MapHasAsJava, MapHasAsScala,
      SeqHasAsJava }

export
  scala.annotation
  . { implicitNotFound as missingContext, StaticAnnotation, switch, tailrec, targetName }

export scala.annotation.unchecked.{uncheckedCaptures, uncheckedStable, uncheckedVariance}

export scala.DummyImplicit as Void

export proscenium.{`~>`, Label, Mono, Nat, prohibits, unexported, Zero}

transparent inline def infer[context]: context = compiletime.summonInline[context]

export proscenium.provide

export Conversion.into
