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

import proscenium.compat.*

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

import filesystemOptions.dereferenceSymlinks.enabled
import interfaces.paths.pathOnLocal

import filesystemBackends.virtualMachineFilesystem

object Pathname:
  def unapply(argument: Argument)(using WorkingDirectory, Cli, System): Option[Path on Local] =
    safely:
      def suggest(path: Text): Suggestion =
        val point = path.s.lastIndexOf('/', path.length - 2) + 1
        val prefix = path.keep(point)
        val core = path.skip(point)
        // Only a directory leaves the completion mid-path; a file candidate is complete,
        // and accepting it should advance to the next argument.
        Suggestion(core, Unset, incomplete = path.ends(t"/"), prefix = prefix)

      // Each branch adds to `prior` rather than replacing it: another extractor evaluated
      // against the same argument (typically a `Subcommand`) must keep its suggestions.
      if argument() == t"." then argument.suggest:
        val wd: Path on Local = workingDirectory
        val listing = List.of:
          suggest(t"../") ::
            workingDirectory.children.stdlib.toList.filter(_.name.starts(t".")).map: path =>
              val directory = safely(path.entry() == galilei.Directory).or(false)
              suggest(if directory then path.name+t"/" else path.name)

        listing + prior

      else if argument() == t".." then argument.suggest:
        val listing = List.of:
          suggest(t"../") ::
            workingDirectory.children.stdlib.toList.filter(_.name.starts(t"..")).map: path =>
              val directory = safely(path.entry() == galilei.Directory).or(false)
              suggest(if directory then path.name+t"/" else path.name)

        listing + prior

      else if argument().nil then argument.suggest:
        val children0 = workingDirectory.children.stdlib.toList
        val showAll = argument.tab.or(Prim) > Prim
        val children =
          if !showAll then children0.filter(!_.name.starts(t".")) else children0

        val listing = List.of:
         children.map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          suggest(if directory then path.name+t"/" else path.name)

        listing + prior

      else
        val absolute = argument().starts(t"/")
        val directory = argument().ends(t"/")
        // Resolution runs under its own optional tactic; no aliased writer.
        val prototype = scala.caps.unsafe.unsafeAssumeSeparate:
          workingDirectory.resolve(argument())
        val showAll = argument.tab.or(Prim) > Prim || prototype.name.starts(t".")
        val base: Optional[Path on Local] = if directory then prototype else prototype.parent
        val children0 = base.let(base => List.of(base.children.stdlib.toList)).or(List[Path on Local]())

        val children =
          if directory then children0
          else children0.filter(_.name.starts(prototype.name))

        argument.suggest:
          val children2 =
            if !showAll then children.filter(!_.name.starts(t".")) else children

          val listing = children2.map: path =>
            val directory = safely(path.entry() == galilei.Directory).or(false)
            val slash = if directory then t"/" else t""

            suggest:
              if absolute then path.encode+slash else workingDirectory.toward(path).encode+slash

          listing + prior

    scala.caps.unsafe.unsafeAssumeSeparate:
      safely(workingDirectory.resolve(argument())).option
