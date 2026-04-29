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
package exoskeleton

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import galilei.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import serpentine.*
import symbolism.*
import vacuous.*

import filesystemOptions.dereferenceSymlinks.enabled
import interfaces.paths.pathOnLinux

object Pathname:
  def unapply(argument: Argument)(using WorkingDirectory, Cli): Option[Path on Linux] =
    safely:
      def suggest(path: Text): Suggestion =
        val point = path.s.lastIndexOf('/', path.length - 2) + 1
        val prefix = path.keep(point)
        val core = path.skip(point)
        Suggestion(core, Unset, incomplete = path != argument(), prefix = prefix)

      if argument() == t"." then argument.suggest:
        val wd: Path on Linux = workingDirectory
        suggest(t"../")
        :: workingDirectory.children.to(List).filter(_.name.starts(t".")).map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          suggest(if directory then path.name+t"/" else path.name)

      else if argument() == t".." then argument.suggest:
        suggest(t"../")
        :: workingDirectory.children.to(List).filter(_.name.starts(t"..")).map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          suggest(if directory then path.name+t"/" else path.name)

      else if argument().nil then argument.suggest:
        val children0 = workingDirectory.children.to(List)
        val showAll = argument.tab.or(Prim) > Prim
        val children = if !showAll then children0.filter(!_.name.starts(t".")) else children0

        children.map: path =>
          val directory = safely(path.entry() == galilei.Directory).or(false)
          suggest(if directory then path.name+t"/" else path.name)

      else
        val absolute = argument().starts(t"/")
        val directory = argument().ends(t"/")
        val prototype = workingDirectory.resolve(argument())
        val showAll = argument.tab.or(Prim) > Prim || prototype.name.starts(t".")
        val base: Optional[Path on Linux] = if directory then prototype else prototype.parent
        val children0 = base.lay(Nil)(_.children.to(List))

        val children =
          if directory then children0 else children0.filter(_.name.starts(prototype.name))

        argument.suggest:
          val children2 = if !showAll then children.filter(!_.name.starts(t".")) else children

          children2.map: path =>
            val directory = safely(path.entry() == galilei.Directory).or(false)
            val slash = if directory then t"/" else t""

            suggest:
              if absolute then path.encode+slash else workingDirectory.toward(path).encode+slash

    safely(workingDirectory.resolve(argument())).option
