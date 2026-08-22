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

import soundness.*

import strategies.throwUnsafely
import workingDirectories.defaultWorkingDirectory

object SettingTests extends Suite(m"Setting tests"):
  import interpreters.posixInterpreter

  given prefix: Configurator.Prefix = Configurator.Prefix(t"myapp")

  def fixedEnvironment(entries: (Text, Text)*): Environment =
    val map = Map.from(entries)
    name => map.at(name)

  def fixedSystem(entries: (Text, Text)*): System =
    val map = Map.from(entries)
    name => map.at(name)

  def invocation(arguments: Text*)(using environment: Environment): Invocation =
    Invocation
      ( Cli.arguments(arguments),
        environment,
        summon[WorkingDirectory],
        stdios.muteStdio,
        true,
        Login(t"tester", Unset) )

  def run(): Unit =
    val Port = Setting[Int](t"port", description = t"the port to listen on")
    val LogLevel = Setting[Text](t"logLevel", description = t"logging verbosity")

    test(m"A CLI parameter beats a property and an environment variable"):
      given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"1")
      given system: System = fixedSystem(t"myapp.port" -> t"2")
      given cli: Cli = invocation(t"--port", t"3")
      Port()
    . assert(_ == 3)

    test(m"A system property beats an environment variable"):
      given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"1")
      given system: System = fixedSystem(t"myapp.port" -> t"2")
      given cli: Cli = invocation()
      Port()
    . assert(_ == 2)

    test(m"An environment variable applies when nothing else is set"):
      given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"1")
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      Port()
    . assert(_ == 1)

    test(m"A setting absent from every source reads as Unset"):
      given environment: Environment = fixedEnvironment()
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      Port()
    . assert(_ == Unset)

    test(m"The vacuous default applies below the whole cascade"):
      given environment: Environment = fixedEnvironment()
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      Port().or(8080)
    . assert(_ == 8080)

    test(m"A multi-word name maps to a SCREAMING_SNAKE_CASE environment variable"):
      given environment: Environment = fixedEnvironment(t"MYAPP_LOG_LEVEL" -> t"info")
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      LogLevel()
    . assert(_ == t"info")

    test(m"A multi-word name maps to a dotted system property"):
      given environment: Environment = fixedEnvironment()
      given system: System = fixedSystem(t"myapp.log.level" -> t"warn")
      given cli: Cli = invocation()
      LogLevel()
    . assert(_ == t"warn")

    test(m"A multi-word name maps to a kebab-case command-line flag"):
      given environment: Environment = fixedEnvironment()
      given system: System = fixedSystem()
      given cli: Cli = invocation(t"--log-level", t"debug")
      LogLevel()
    . assert(_ == t"debug")

    test(m"A malformed value in the winning source does not fall through"):
      given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"invalid")
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      safely(Port())
    . assert(_ == Unset)

    test(m"An explicit variable override is consulted verbatim"):
      val Home = Setting[Text](t"home", variable = t"MYTOOL_HOME")
      given environment: Environment = fixedEnvironment(t"MYTOOL_HOME" -> t"/opt/mytool")
      given system: System = fixedSystem()
      given cli: Cli = invocation()
      Home()
    . assert(_ == t"/opt/mytool")

    test(m"A user-defined configurator overrides the default cascade"):
      given environment: Environment = fixedEnvironment(t"MYAPP_LOG_LEVEL" -> t"info")
      given system: System = fixedSystem()
      given configurator: Configurator = name => t"custom"
      given cli: Cli = invocation()
      LogLevel()
    . assert(_ == t"custom")

    test(m"Reading a setting registers its flag for completion"):
      given environment: Environment = fixedEnvironment()
      val completion =
        Completion
          ( Cli.arguments(List(t"")), Cli.arguments(List(t""), 0, Unset, Prim),
            summon[Environment], summon[WorkingDirectory], Shell.Zsh, 0, Unset,
            stdios.muteStdio, t"", Prim, Login(t"tester", Unset) )

      given system: System = fixedSystem()
      Port()(using completion)
      completion.flags.keySet.to(List).map(_.name)
    . assert(_ == List(t"port"))

    test(m"Reading a setting records its environment variable for help"):
      val variables: scala.collection.mutable.LinkedHashSet[Text] =
        scala.collection.mutable.LinkedHashSet()

      given environment: Environment = name =>
        variables += name
        Unset

      given system: System = fixedSystem()
      given cli: Cli = invocation()
      Port()
      variables.to(scala.List)
    . assert(_ == scala.List(t"MYAPP_PORT"))
