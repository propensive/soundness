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
package sedentary

import ambience.*
import anticipation.*
import contingency.*
import eucalyptus.*
import galilei.*
import gossamer.*
import guillotine.*
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import logging.silentLogging
import workingDirectories.javaWorkingDirectory
import beneficence.*

trait BenchmarkDevice extends Findable:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError

  // `heap` overrides the measurement JVM's fixed heap size (both `-Xms` and `-Xmx`), in
  // `-Xmx` syntax, e.g. `t"256m"`; when unset, the default of `1g` applies. Constrained-heap
  // stress tests use this to demonstrate what a workload sustains in a small, pinned heap.
  def invoke(path: Path on Linux, input: Text, heap: Optional[Text] = Unset)
  :   Text raises BenchError

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError

class NetworkDevice(user: Text, host: Hostname) extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError =
    safely(sh"scp $path $user@$host:$uuid.jar".exec[Exit]()).lest(BenchError())

  def invoke(path: Path on Linux, input: Text, heap: Optional[Text] = Unset)
  :   Text raises BenchError =
    val size = heap.or(t"1g")

    val command =
      sh"""
        java
          -XX:+AlwaysPreTouch
          -Xms$size
          -Xmx$size
          -XX:CICompilerCount=2
          -XX:+UseSerialGC
          -jar ${path.name}
          '$input'
          2> /dev/null
      """

    safely(sh"""ssh $user@$host ${command.escape}""".exec[Text]()).lest(BenchError())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError =
    safely(sh"ssh $user@$host rm $path".exec[Text]()).lest(BenchError())

object LocalhostDevice extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError = ()

  def invoke(path: Path on Linux, input: Text, heap: Optional[Text] = Unset)
  :   Text raises BenchError =
    val size = heap.or(t"1g")
    val opts = sh"-XX:+AlwaysPreTouch -Xms$size -Xmx$size -XX:CICompilerCount=2 -XX:+UseSerialGC"
    val cmd = sh"java $opts -jar $path $input"
    safely(cmd.exec[Text]()).lest(BenchError())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError = ()
