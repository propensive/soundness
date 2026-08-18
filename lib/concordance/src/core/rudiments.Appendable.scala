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
package rudiments

import denominative.*
import prepositional.*

// Single-element growth for the opaque collections: `:+` incorporates one element (at the end,
// for ordered shapes), the counterpart of `Prependable`'s `+:`. Collection-with-collection
// concatenation is symbolism's `Concatenable` (`+`). Instances whose append rebuilds the whole
// collection are gated on the acknowledgement of that linear cost, like `Countable`.
object Appendable:
  // Appending to a `List` copies all of it: O(n), so the instance demands the marker.
  given list: [element] => (complexity: LinearSizeComplexity)
  =>  List[element] is Appendable by element =
    (list, element) => List.of(list.stdlib :+ element)

  given sequence: [element] => Sequence[element] is Appendable by element =
    (sequence, element) => Sequence.of(sequence.stdlib :+ element)

  // Lazily: nothing is forced by the append itself.
  given chain: [element] => Chain[element] is Appendable by element =
    (chain, element) => Chain.of(chain.stdlib.appended(element))

  // A `Set` has no ends, so `:+` is plain membership addition (the stdlib's `set + element`).
  given set: [element] => Set[element] is Appendable by element =
    (set, element) => Set.of(set.stdlib + element)

  // The frozen array is rebuilt in full: O(n), so the instance demands the marker.
  given frozenArray: [element: scala.reflect.ClassTag] => (complexity: LinearSizeComplexity)
  =>  (Array[element]^{}) is Appendable by element =
    (array, element) => Array.frozen(array.readable :+ element)

trait Appendable extends Typeclass.Pure, Operable:
  def append(self: Self, element: Operand): Self

extension [self, operand](value: self)(using appendable: self is Appendable by operand)
  infix def :+ (element: operand): self = appendable.append(value, element)
