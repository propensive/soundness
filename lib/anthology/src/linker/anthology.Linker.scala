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
package anthology

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import prepositional.*
import serpentine.*

object Linker:
  // An entry point whose `main(args: Array[String])` method runs when the linked module loads.
  // Now `anthology.EntryPoint` in the core module, aliased here for the one-hop API.
  type EntryPoint = anthology.EntryPoint
  val EntryPoint: anthology.EntryPoint.type = anthology.EntryPoint

  object Option:
    // Sound by construction: options are only creatable through the per-family DSLs, each of
    // which types its options at a subset of exactly one link family, and every `Linkage` of
    // that family fixes `Form` to the type the DSL edits; no option is typed across families.
    private[anthology] def apply[artifact <: Artifact, form](edit0: form => form)
    :   Option[artifact] =

      new Option[artifact]:
        private[anthology] def edit(form0: Any): Any = edit0(form0.asInstanceOf[form])

  // Options are constructible only through the per-family DSLs, keeping the underlying linker
  // configuration types out of the public API; contravariance permits an option declared for a
  // union of artifacts in the options list of any of that union's linkers.
  trait Option[-artifact <: Artifact]:
    private[anthology] def edit(form0: Any): Any

case class Linker[artifact <: Artifact]
  ( options: List[Linker.Option[artifact]], entryPoints: List[Linker.EntryPoint] = Nil ):

  def link(using linkage: Linkage[artifact])
    ( compilation: Compilation[linkage.Origin], out: Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError =

    Log.info(LinkEvent.Start)

    val form: linkage.Form =
      options.stdlib.foldLeft(linkage.initial): (form, option) =>
        option.edit(form).asInstanceOf[linkage.Form]

    val result = linkage.link(form, compilation, entryPoints, out)
    Log.info(LinkEvent.Linked(result.encode))
    result
