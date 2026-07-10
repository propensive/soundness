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
package proscenium

export scala.collection.immutable.Vector as Series
export Predef.runtimeChecked as absolve
export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}
export scala.collection.concurrent.TrieMap

export Predef
. { nn, identity, summon, charWrapper, $conforms, ArrowAssoc, intWrapper, longWrapper,
    shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper, locally, is,
    refArrayOps, genericArrayOps, byteArrayOps, shortArrayOps, intArrayOps, longArrayOps,
    floatArrayOps, doubleArrayOps, charArrayOps, booleanArrayOps, unitArrayOps,
    augmentString, `???`, assert }

// `Predef.classOf` is a compiler intrinsic only when its call carries a concrete class type:
// through an export forwarder, the type argument is the forwarder's own (erased) type parameter,
// so every call yielded `Class[Object]` at runtime. This inline version summons the class token
// at each call site instead (where the type argument is concrete).
inline def classOf[T](using tag: scala.reflect.ClassTag[T]): Class[T] =
  tag.runtimeClass.asInstanceOf[Class[T]]

export scala.util.control.NonFatal

export scala.util.boundary, boundary.break

export scala.jdk.CollectionConverters
. { IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava, MapHasAsJava,
    EnumerationHasAsScala }

export scala.annotation
. { tailrec, implicitNotFound as missingContext, targetName, switch, StaticAnnotation }

export scala.annotation.unchecked.{uncheckedVariance, uncheckedCaptures, uncheckedStable}

export scala.DummyImplicit as Void

export Conversion.into

type Nat = Int & Singleton
type Label = String & Singleton

// Marks a top-level definition (a module, an extension method, …) that is
// deliberately *not* re-exported into the `soundness` umbrella — typically because
// its simple name clashes with another component's in that package, or it is a
// compile-time/internal helper reached via the component's own import. Decorum's
// SN-742/SN-742.1 export rules read this annotation and skip the annotated
// definition.
final class unexported() extends StaticAnnotation

@targetName("partialFn")
infix type ~> [-domain, +range] = PartialFunction[domain, range]

export scala.EmptyTuple as Zero

object Mono:
  inline def apply[value](value: value): Mono[value] = value *: Zero

type Mono[value] = value *: Zero

transparent inline def infer[context]: context = compiletime.summonInline[context]

transparent inline def provide[context](using erased Void)[result]
  ( inline lambda: context ?=> result )
:   result =

  lambda(using infer[context])
