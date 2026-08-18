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
package jacinta

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import panopticon.*
import prepositional.*
import vacuous.*

// The panopticon lens and optic instances for `Json`. These are the only reason `jacinta.core`
// needed panopticon, so they live here instead of in `Json`'s companion. That takes them out of
// `Json`'s implicit scope, so a call site using `json.lens(…)` must import them — decisively,
// from the `optics` package (`import jacinta.optics.*`, or `import soundness.optics.*`, where
// every library's optics merge); panopticon's generic `deref` lens only applies to `Product`
// types, and `Json` is not one, so a missing import is a compile error rather than a silent
// change of behaviour.

package optics:
  given jsonLens: [name <: Label: ValueOf] => (erased dynamicJsonEnabler: DynamicJsonEnabler) => (tactic: Tactic[Json.Error])
  =>  ((name is Lens from Json onto Json)^{tactic}) =

    Lens(_.selectField(valueOf[name]), (json, value) => json.modify(valueOf[name], value))

  given jsonOrdinalOptical: [element] => Ordinal is Optical from Json onto Json =
    ordinal =>
      Optic: (origin, lambda) =>
        if origin.root.isArray then
          val n = origin.root.arrayLength

          if n <= ordinal.n0 then origin else Json.ast:
            val updated = Array[Any](n)
            var i = 0

            while i < n do
              updated(i) =
                if i == ordinal.n0
                then lambda(Json.ast(origin.root.arrayElement(i))).root
                else origin.root.arrayElement(i)

              i += 1

            Json.Ast.arr(Array.freeze(updated))
        else
          origin

  // `Each` applies the transform to every array element; `Filter` to those matching
  // its predicate. Both rebuild the array immutably and no-op on non-arrays.
  given jsonEachOptical: Each.type is Optical from Json onto Json = _ =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Json.ast:
          val updated = Array[Any](n)
          var i = 0

          while i < n do
            updated(i) = lambda(Json.ast(origin.root.arrayElement(i))).root
            i += 1

          Json.Ast.arr(Array.freeze(updated))
      else
        origin

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM
  // pipeline — rejects the `Optic`'s capture of `filter.predicate` against the required
  // pure `Optic` type. (Compiler divergence; see #1520 and `caesura`'s `rowFilter`.)
  given jsonFilterOptical: Filter[Json] is Optical from Json onto Json = filter =>
    val predicate: Json -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Json.ast:
          val updated = Array[Any](n)
          var i = 0

          while i < n do
            val element = Json.ast(origin.root.arrayElement(i))
            updated(i) = (if predicate(element) then lambda(element) else element).root
            i += 1

          Json.Ast.arr(Array.freeze(updated))
      else
        origin

