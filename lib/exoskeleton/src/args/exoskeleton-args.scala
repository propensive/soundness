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
┃    Soundness, version 0.42.0.                                                                    ┃
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

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

given realm: Realm = realm"exoskeleton"

package interpreters:
  given simple: Interpreter:
    type Topic = Arguments
    def interpret(arguments: List[Argument]): Arguments = Arguments(arguments*)

  given posixClustering: Interpreter:
    type Topic = Commandline
    def interpret(arguments: List[Argument]): Commandline = interpreter(arguments, true)

  given posix: Interpreter:
    type Topic = Commandline
    def interpret(arguments: List[Argument]): Commandline = interpreter(arguments, false)

  private def interpreter(arguments: List[Argument], clustering: Boolean): Commandline =
    def recur
         (todo:        List[Argument],
          arguments:   List[Argument],
          current:     Optional[Argument],
          commandline: Commandline)
    : Commandline =

        def push(): Commandline = current.lay(Commandline(arguments.reverse)): current =>
          commandline.copy
           (parameters = commandline.parameters.updated(current, arguments.reverse))

        def postprocess(commandline: Commandline): Commandline =
          val parameters2: Map[Argument, List[Argument]] =
            commandline.parameters.to(List).flatMap: (key, values) =>
              val flag = key.value
              if flag.starts(t"--") && flag.contains(t"=")
              then
                val key2 = key.copy(format = Argument.Format.EqualityPrefix)
                val value = key.copy(format = Argument.Format.EqualitySuffix)
                List(key2 -> (value :: values))
              else if flag.starts(t"-") && !flag.starts(t"--") && flag.length > 2
              then
                if clustering then
                  val init =
                    (0 until (flag.length - 2)).to(List).map: index =>
                      key.copy(format = Argument.Format.CharFlag(index.z)) -> Nil

                  init :+ (key.copy(format = Argument.Format.CharFlag((flag.length - 2).z)), values)
                else
                  List:
                    key.copy(format = Argument.Format.CharFlag(Prim))
                    -> (key.copy(format = Argument.Format.FlagSuffix) :: values)

              else List(key -> values)
            . to(Map)

          val focus2 = current.let: current =>
            val focusCursor: Ordinal = current.cursor.or(current.value.length).z

            (parameters2.keySet ++ parameters2.values.flatten).find: argument =>
              current.position == argument.position && argument.contains(focusCursor)

            . optional

          commandline.copy(parameters = parameters2, focus = focus2)

        todo match
          case head :: tail =>
            if head.value == t"--" then push().copy(postpositional = tail)
            else if head.value.starts(t"-") then recur(tail, Nil, head, push())
            //else if head.value == t"=" && arguments == Nil
            //then recur(tail, Nil, current, commandline)
            else
              val commandline2 =
                if head.cursor.present then commandline.copy(focus = current) else commandline
              recur(tail, head :: arguments, current, commandline2)

          case Nil =>
            postprocess(push())

    recur(arguments, Nil, Unset, Commandline())

def arguments(using cli: Cli): List[Argument] = cli.arguments
