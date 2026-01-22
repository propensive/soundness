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
┃    Soundness, version 0.53.0.                                                                    ┃
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
import fulminate.*
import gossamer.*
import guillotine.*
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*

import workingDirectories.java
import errorDiagnostics.stackTraces
import logging.silent

trait BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError
  def invoke(path: Path on Linux, input: Text): Text raises BenchError
  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError

class NetworkDevice(user: Text, host: Hostname) extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError =
    safely(sh"scp $path $user@$host:$uuid.jar".exec[Exit]()).lest(BenchError())

  def invoke(path: Path on Linux, input: Text): Text raises BenchError =
    //val command = sh"sudo taskset -c 2 chrt -b 0 nice -n -20 ionice -c1 -n0 java -XX:+AlwaysPreTouch -Xms1g -Xmx1g -XX:CICompilerCount=2 -XX:+UseSerialGC -jar ${path.name} '$input' 2> /dev/null"
    val command = sh"java -XX:+AlwaysPreTouch -Xms1g -Xmx1g -XX:CICompilerCount=2 -XX:+UseSerialGC -jar ${path.name} '$input' 2> /dev/null"
    safely(sh"""ssh $user@$host ${command.escape}""".exec[Text]()).lest(BenchError())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError =
    safely(sh"ssh $user@$host rm $path".exec[Text]()).lest(BenchError())

object LocalhostDevice extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError = ()

  def invoke(path: Path on Linux, input: Text): Text raises BenchError =
    safely(sh"java -jar $path $input".exec[Text]()).lest(BenchError())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises BenchError = ()
