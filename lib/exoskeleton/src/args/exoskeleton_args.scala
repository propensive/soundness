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

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import vacuous.*

package interpreters:
  given simpleInterpreter: Interpreter:
    type Topic = List[Argument]

    def interpret(arguments: List[Argument]): List[Argument] = arguments
    def focus(arguments: List[Argument]): Optional[Argument] = Unset
    def find(arguments: List[Argument], flag: Flag): List[Argument] = Nil


    def read[operand: Interpretable](arguments: List[Argument], flag: Flag)
      ( using cli: Cli, discoverable: (? <: operand) is Discoverable )
    :   Optional[operand] =

      Unset

  given posixClusteringInterpreter: Interpreter:
    type Topic = Commandline

    def interpret(arguments: List[Argument]): Commandline = interpreter(arguments, true)
    def focus(commandline: Commandline): Optional[Argument] = commandline.focus
    def find(commandline: Commandline, flag: Flag): List[Argument] = commandline.at(flag)


    def read[operand: Interpretable](commandline: Commandline, flag: Flag)
      ( using cli: Cli, discoverable: (? <: operand) is Discoverable )
    :   Optional[operand] =

      commandline.read(flag)

  given posixInterpreter: Interpreter:
    type Topic = Commandline

    def interpret(arguments: List[Argument]): Commandline = interpreter(arguments, false)
    def focus(commandline: Commandline): Optional[Argument] = commandline.focus
    def find(commandline: Commandline, flag: Flag): List[Argument] = commandline.at(flag)


    def read[operand: Interpretable](commandline: Commandline, flag: Flag)
      ( using cli: Cli, discoverable: (? <: operand) is Discoverable )
    :   Optional[operand] =

      commandline.read(flag)

  private def interpreter(arguments: List[Argument], clustering: Boolean): Commandline =
    def recur
      ( todo:        List[Argument],
        arguments:   List[Argument],
        current:     Optional[Argument],
        commandline: Commandline )
    :   Commandline =

      def push(): Commandline = current.lay(Commandline(List.of(arguments.stdlib.reverse))): current =>
        commandline.copy
          ( parameters = commandline.parameters.updated(current, List.of(arguments.stdlib.reverse)) )

      def postprocess(commandline: Commandline): Commandline =
        val parameters2: Map[Argument, List[Argument]] = Map.from:
          commandline.parameters.stdlib.toList.flatMap: (key, values) =>
            val flag = key.value

            if flag.starts(t"--") && flag.contains(t"=")
            then
              val key2 = key.copy(format = Argument.Format.EqualityPrefix)
              val value = key.copy(format = Argument.Format.EqualitySuffix)
              scala.collection.immutable.List(key2 -> List.of(value :: values.stdlib))
            else if flag.starts(t"-") && !flag.starts(t"--") && flag.length > 2
            then
              if clustering then
                val init =
                  (0 until (flag.length - 2)).toList.map: index =>
                    key.copy(format = Argument.Format.CharFlag(index.z)) -> Nil

                init :+ (key.copy(format = Argument.Format.CharFlag((flag.length - 2).z)), values)
              else
                scala.collection.immutable.List:
                  key.copy(format = Argument.Format.CharFlag(Prim)) ->
                    List.of(key.copy(format = Argument.Format.FlagSuffix) :: values.stdlib)

            else
              scala.collection.immutable.List(key -> values)

        val focus2 = current.let: current =>
          val focusCursor: Ordinal = current.cursor.or(current.value.length).z

          (parameters2.stdlib.keySet ++ parameters2.stdlib.values.flatMap(_.stdlib)).find: argument =>
            current.position == argument.position && argument.contains(focusCursor)

          . optional

        commandline.copy(parameters = parameters2, focus = focus2)

      todo match
        case head :: tail =>
          if head.value == t"--" then push().copy(postpositional = tail)
          else if head.value.starts(t"-") then recur(tail, Nil, head, push())
          else
            val commandline2 =
              if head.cursor.present then commandline.copy(focus = current) else commandline

            recur(tail, head :: arguments, current, commandline2)

        case Nil =>
          postprocess(push())

    recur(arguments, Nil, Unset, Commandline())

def arguments(using cli: Cli): List[Argument] = cli.arguments
