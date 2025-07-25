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
┃    Soundness, version 0.40.0.                                                                    ┃
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
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap

export Predef
. { nn, identity, summon, charWrapper, $conforms, ArrowAssoc, intWrapper, longWrapper,
    shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper, locally }

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export scala.jdk.CollectionConverters
. { IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava, MapHasAsJava,
    EnumerationHasAsScala }

export scala.annotation
. { tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation }

export scala.annotation.unchecked.{uncheckedVariance, uncheckedCaptures, uncheckedStable}

export scala.LazyList as Stream
export scala.DummyImplicit as Void

export Conversion.into

type Nat = Int & Singleton
type Label = String & Singleton

@targetName("partialFn")
infix type ~> [-domain, +range] = PartialFunction[domain, range]

export scala.EmptyTuple as Zero

type Mono[value] = value *: Zero

object Mono:
  inline def apply[value](value: value): Mono[value] = value *: Zero

transparent inline def infer[context]: context = compiletime.summonInline[context]

type Macro[result] = scala.quoted.Quotes ?=> scala.quoted.Expr[result]

transparent inline def provide[context](using erased Void)[result]
                        (inline lambda: context ?=> result)
: result =

    lambda(using compiletime.summonInline[context])
