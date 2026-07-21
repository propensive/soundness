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

// `collection.has(value)` (value membership) for any `collection` that is `Inclusive`; the queried
// value type is fixed by the instance's `Operand`. Whether a *key/index* is present is `Indexable`'s
// `defines` instead (that lives in `rudiments`, being `Ordinal`-adjacent).
extension [self](self: self)(using inclusive: self is Inclusive)
  def has(value: inclusive.Operand): Boolean = inclusive.has(self, value)

// The shape-preserving `map`, driven by `Mappable` rather than `Traversable`+`Reshapable`, so a
// `Map` maps its *values* (keys preserved) instead of iterating `(key, value)` pairs; `remap` (below)
// covers the pairwise/entry case. `mappable` is summoned at the *extension* level so the lambda's
// parameter type is the concrete `mappable.Operand` (so `xs.map(_.field)` infers the element type);
// the mapped-container constructor is bound as the higher-kinded type parameter `result[_]` so the
// return type `result[element2]` is a plain application, never a path-dependent projection (#1411).
extension [self, result[_]](self: self)
  (using mappable: self is Mappable { type Result[element2] = result[element2] })
  def map[element2](lambda: mappable.Operand => element2): result[element2] =
    mappable.map(self, lambda)

// The pairwise/entry transformation, for the shapes `map` no longer covers uniformly: a `Map`'s
// entries are presented as `(key, value)` pairs and rebuilt through `Reshapable` into whatever shape
// the lambda's result implies (another `Map` for pair results, else a `List`).
extension [self](self: self)(using traversable: self is Traversable)
  def remap[element2, result](lambda: traversable.Operand => element2)
    ( using reshapable: self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).map(lambda))
