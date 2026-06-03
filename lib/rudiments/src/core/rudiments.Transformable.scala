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
package rudiments

import scala.reflect.ClassTag

import prepositional.*

import murmuration.{Expandable, Fabricable}

object Transformable:
  // One generic instance covers every (unary source) → (unary target) pair: expand the source into
  // elements, then fabricate the target from them. `scala.collection.Factory` appears nowhere.
  given collection: [source[_], target[_], element]
      => (expandable:  source is Expandable)
      => (fabricable:  target is Fabricable)
      => (source[element] is Transformable in target to target[element]) = new Transformable:
        type Self = source[element]
        type Form = target
        type Result = target[element]

        def transform(self: source[element]): target[element] =
          fabricable.fabricate(expandable.expand(self))

  // `Map` is binary, so it is not a `Fabricable` unary target; converting any pair-yielding source
  // into a `Map` is its own instance.
  given mapTarget: [source[_], key, value]
      => (expandable: source is Expandable)
      => (source[(key, value)] is Transformable in Map to Map[key, value]) = new Transformable:
        type Self = source[(key, value)]
        type Form = Map
        type Result = Map[key, value]

        def transform(self: source[(key, value)]): Map[key, value] =
          Map.from(expandable.expand(self))

  // A `Map` source expands to its key/value pairs, which any unary target can fabricate.
  given mapSource: [target[_], key, value]
      => (fabricable: target is Fabricable)
      => (Map[key, value] is Transformable in target to target[(key, value)]) = new Transformable:
        type Self = Map[key, value]
        type Form = target
        type Result = target[(key, value)]

        def transform(self: Map[key, value]): target[(key, value)] = fabricable.fabricate(self.iterator)

  // `IArray`/`Array` targets need a per-element `ClassTag`, which the `Fabricable` interface does not
  // carry, so they are their own instances (the `ClassTag` rides on the given).
  given iarrayTarget: [source[_], element: ClassTag]
      => (expandable: source is Expandable)
      => (source[element] is Transformable in IArray to IArray[element]) = new Transformable:
        type Self = source[element]
        type Form = IArray
        type Result = IArray[element]

        def transform(self: source[element]): IArray[element] = IArray.from(expandable.expand(self))

  given arrayTarget: [source[_], element: ClassTag]
      => (expandable: source is Expandable)
      => (source[element] is Transformable in Array to Array[element]) = new Transformable:
        type Self = source[element]
        type Form = Array
        type Result = Array[element]

        def transform(self: source[element]): Array[element] = Array.from(expandable.expand(self))

trait Transformable extends Typeclass, Resultant:
  type Form <: AnyKind
  def transform(self: Self): Result
