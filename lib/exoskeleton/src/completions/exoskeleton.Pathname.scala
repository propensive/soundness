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
package exoskeleton

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import galilei.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import vacuous.*

import filesystemOptions.dereferenceSymlinks
import interfaces.paths.pathOnLocal

import filesystemBackends.javaBaseFilesystem

object Pathname:
  // The property is read straight off the `System` capability rather than through
  // `Directories.homeText`, which panics when it is unset. An unknown home leaves `~` alone,
  // to resolve (and fail) like any other name.
  private def home(using System): Optional[Text] = summon[System](t"user.home").let: text =>
    if text.length > 1 && text.ends(t"/") then text.skip(1, Bidi.Rtl) else text

  // A shell expands `~` before exec, but a quoted argument and every partial word handed to
  // the completion script arrive with the tilde intact, so it is expanded here too.
  private def expand(text: Text)(using System): Text = home.lay(text): home =>
    if text == t"~" then home else if text.starts(t"~/") then home+text.skip(1) else text

  // The inverse, so a completion under a tilde stays short and keeps its tilde.
  private def abbreviate(text: Text)(using System): Text = home.lay(text): home =>
    if text == home then t"~"
    else if text.starts(home+t"/") then t"~"+text.skip(home.length)
    else text

  // The pathname completion candidates for the partially-typed `operand` — expanding `~`,
  // resolving against the working directory, and descending into a directory as it is typed —
  // shared by the positional extractor below and the flag-operand `pathnameDiscoverable`.
  // `tab` is the number of tab presses at the position (a repeated press reveals hidden
  // entries). Empty when the operand cannot be resolved.
  def complete(operand: Text, tab: Ordinal)(using WorkingDirectory, System): List[Suggestion] =
    def suggest(path: Text): Suggestion =
      val point = path.s.lastIndexOf('/', path.length - 2) + 1
      val prefix = path.keep(point)
      val core = path.skip(point)
      // Only a directory leaves the completion mid-path; a file candidate is complete,
      // and accepting it should advance to the next argument. Every candidate is an operand
      // value rather than a subcommand, so the help tree does not enumerate the working
      // directory as though it were syntax (see `Suggestion.operand`).
      Suggestion(core, Unset, incomplete = path.ends(t"/"), prefix = prefix, operand = true)

    safely:
      // `children` is a lazy `Chain`; each branch forces it once into a `List` before filtering.
      if operand == t"." then
        val children0: List[Path on Local] = workingDirectory.children.to[List]

        suggest(t"../") ::
          children0.filter(_.name.starts(t".")).map: path =>
            val directory = safely(path.entry() == galilei.Directory).or(false)
            suggest(if directory then path.name+t"/" else path.name)

      else if operand == t".." then
        val children0: List[Path on Local] = workingDirectory.children.to[List]

        suggest(t"../") ::
          children0.filter(_.name.starts(t"..")).map: path =>
            val directory = safely(path.entry() == galilei.Directory).or(false)
            suggest(if directory then path.name+t"/" else path.name)

      else if operand.nil then
        val children0: List[Path on Local] = workingDirectory.children.to[List]
        val showAll = tab > Prim
        val children =
          if !showAll then children0.filter(!_.name.starts(t".")) else children0

        children.map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          suggest(if directory then path.name+t"/" else path.name)

      else
        val tilde = home.present && (operand == t"~" || operand.starts(t"~/"))
        val absolute = operand.starts(t"/")
        // A bare `~` names the home directory itself, so it lists that directory's children,
        // exactly as `~/` does; without this it would list the home directory's siblings.
        val directory = operand.ends(t"/") || operand == t"~"
        // Resolution runs under its own optional tactic; no aliased writer.
        val prototype = scala.caps.unsafe.unsafeAssumeSeparate:
          workingDirectory.resolve(expand(operand))
        val showAll = tab > Prim || prototype.name.starts(t".")
        val base: Optional[Path on Local] = if directory then prototype else prototype.parent
        val children0 = base.let(base => base.children.to[List]).or(List[Path on Local]())

        val children =
          if directory then children0
          else children0.filter(_.name.starts(prototype.name))

        val children2 =
          if !showAll then children.filter(!_.name.starts(t".")) else children

        children2.map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          val slash = if directory then t"/" else t""

          suggest:
            if tilde then abbreviate(path.encode)+slash
            else if absolute then path.encode+slash
            else workingDirectory.toward(path).encode+slash

    . or(List())


  def unapply(argument: Argument)(using WorkingDirectory, Cli, System): Option[Path on Local] =
    // The candidates add to `prior` rather than replacing it: another extractor evaluated
    // against the same argument (typically a `Subcommand`) must keep its suggestions.
    argument.suggest(Pathname.complete(argument(), argument.tab.or(Prim)) + prior)

    scala.caps.unsafe.unsafeAssumeSeparate:
      safely(workingDirectory.resolve(expand(argument()))).option

// The subject `Path on Local` belongs to serpentine, and `Discoverable`'s companion (in the
// platform-neutral `args` module) cannot see galilei, so this given is structurally
// un-anchorable: it lives at the top level with a library-qualified name, to be imported by
// name (issue #1632). With it in scope, a `Flag[Path on Local]`'s operand completes as a
// pathname exactly as a positional `Pathname` argument does.
given pathnameDiscoverable: (WorkingDirectory, System) => (Path on Local) is Discoverable =
  (operand, tab) => Pathname.complete(operand, tab)
