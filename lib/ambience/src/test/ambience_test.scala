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
package ambience

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

case class FakePath(text: Text)

given fakePath: FakePath is Instantiable across Paths from Text = FakePath(_)

def fixedEnvironment(entries: (Text, Text)*): Environment =
  val map = entries.to(Map)
  name => map.at(name)

def fixedSystem(entries: (Text, Text)*): System =
  val map = entries.to(Map)
  name => map.at(name)

object Tests extends Suite(m"Ambience Tests"):
  def run(): Unit =
    suite(m"Architecture tests"):
      test(m"Decode a 32-bit x86 architecture"):
        t"x86".as[Architecture]
      . assert(_ == Architecture.X86(32))

      test(m"Decode the alternative name for 32-bit x86"):
        t"i386".as[Architecture]
      . assert(_ == Architecture.X86(32))

      test(m"Decode a 64-bit x86 architecture"):
        t"x86_64".as[Architecture]
      . assert(_ == Architecture.X86(64))

      test(m"Decode the alternative name for 64-bit x86"):
        t"amd64".as[Architecture]
      . assert(_ == Architecture.X86(64))

      test(m"Decode a 32-bit ARM architecture"):
        t"arm".as[Architecture]
      . assert(_ == Architecture.Arm(32))

      test(m"Decode a 64-bit ARM architecture"):
        t"aarch64".as[Architecture]
      . assert(_ == Architecture.Arm(64))

      test(m"Decode a big-endian PowerPC architecture"):
        t"ppc64".as[Architecture]
      . assert(_ == Architecture.Ppc(64, false))

      test(m"Decode a little-endian PowerPC architecture"):
        t"ppc64le".as[Architecture]
      . assert(_ == Architecture.Ppc(64, true))

      test(m"Decode a SPARC architecture"):
        t"sparcv9".as[Architecture]
      . assert(_ == Architecture.Sparc(64))

      test(m"Decode a MIPS architecture"):
        t"mips64".as[Architecture]
      . assert(_ == Architecture.Mips(64))

      test(m"Decode a RISC-V architecture"):
        t"riscv64".as[Architecture]
      . assert(_ == Architecture.RiscV)

      test(m"Decode a 31-bit S/390 architecture"):
        t"s390".as[Architecture]
      . assert(_ == Architecture.S390(31))

      test(m"An unrecognized architecture keeps its name"):
        t"vax".as[Architecture]
      . assert(_ == Architecture.Other(t"vax"))

    suite(m"Environment tests"):
      test(m"Read a defined environment variable"):
        given environment: Environment = fixedEnvironment(t"HOME" -> t"/home/jack")
        unsafely(Environment[Text](t"HOME"))
      . assert(_ == t"/home/jack")

      test(m"An undefined environment variable raises an error"):
        given environment: Environment = fixedEnvironment()
        unsafely(capture[Environment.Error](Environment[Text](t"HOME"))).variable
      . assert(_ == t"HOME")

      test(m"The empty environment defines nothing"):
        environments.emptyEnvironment.variable(t"HOME")
      . assert(_ == Unset)

      test(m"Select an environment variable by name"):
        given environment: Environment = fixedEnvironment(t"LANG" -> t"en_GB.UTF-8")
        unsafely(Environment.lang[Text])
      . assert(_ == t"en_GB.UTF-8")

      test(m"A camel-case selector maps to a screaming-snake-case name"):
        given environment: Environment = fixedEnvironment(t"XDG_CONFIG_HOME" -> t"/cfg")
        unsafely(Environment.xdgConfigHome[Text])
      . assert(_ == t"/cfg")

      test(m"An environment variable can be decoded"):
        given environment: Environment = fixedEnvironment(t"COLUMNS" -> t"80")
        unsafely(Environment.columns[Int])
      . assert(_ == 80)

    suite(m"Variable naming tests"):
      test(m"A single-word variable name is upper-cased"):
        summon[Variable["term", Text]].defaultName
      . assert(_ == t"TERM")

      test(m"A multi-word variable name is snake-cased"):
        summon[Variable["xdgDataHome", Text]].defaultName
      . assert(_ == t"XDG_DATA_HOME")

      test(m"A variable's default name is unset until derived"):
        summon[Variable["term", Text]].name
      . assert(_ == Unset)

    suite(m"Variable override tests"):
      test(m"An overridden variable takes precedence"):
        given environment: Environment = fixedEnvironment(t"HOME" -> t"/home/jack")

        variables(home = t"/home/jill"):
          unsafely(Environment[Text](t"HOME"))
      . assert(_ == t"/home/jill")

      test(m"A variable not overridden falls back to the environment"):
        given environment: Environment = fixedEnvironment(t"HOME" -> t"/home/jack")

        variables(term = t"xterm"):
          unsafely(Environment[Text](t"HOME"))
      . assert(_ == t"/home/jack")

      test(m"An overridden name is snake-cased"):
        given environment: Environment = fixedEnvironment()

        variables(xdgDataHome = t"/data"):
          unsafely(Environment[Text](t"XDG_DATA_HOME"))
      . assert(_ == t"/data")

      test(m"Several variables can be overridden at once"):
        given environment: Environment = fixedEnvironment()

        variables(home = t"/home/jill", term = t"xterm"):
          (unsafely(Environment[Text](t"HOME")), unsafely(Environment[Text](t"TERM")))
      . assert(_ == (t"/home/jill", t"xterm"))

    suite(m"System property tests"):
      test(m"Read a defined system property"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        unsafely(System.properties.user.home[Text]())
      . assert(_ == t"/home/jack")

      test(m"An undefined system property raises an error"):
        given system: System = fixedSystem()
        unsafely(capture[Property.Error](System.properties.user.home[Text]())).property
      . assert(_ == t"user.home")

      test(m"The empty system defines no properties"):
        systems.emptySystem(t"user.home")
      . assert(_ == Unset)

      test(m"The Java system reads a real property"):
        systems.javaSystem(t"java.version") == Unset
      . assert(_ == false)

    suite(m"Configurator tests"):
      given prefix: Configurator.Prefix = Configurator.Prefix(t"myapp")

      test(m"The environment configurator maps a name to SCREAMING_SNAKE_CASE"):
        given environment: Environment = fixedEnvironment(t"MYAPP_LOG_LEVEL" -> t"info")
        Configurator.environment.read(t"logLevel")
      . assert(_ == t"info")

      test(m"The properties configurator maps a name to dotted lower case"):
        given system: System = fixedSystem(t"myapp.log.level" -> t"warn")
        Configurator.properties.read(t"logLevel")
      . assert(_ == t"warn")

      test(m"An absent setting reads as Unset"):
        given environment: Environment = fixedEnvironment()
        Configurator.environment.read(t"logLevel")
      . assert(_ == Unset)

      test(m"Composition prefers the left configurator"):
        given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"80")
        given system: System = fixedSystem(t"myapp.port" -> t"8080")
        (Configurator.properties ++ Configurator.environment).read(t"port")
      . assert(_ == t"8080")

      test(m"Composition falls through to the right configurator"):
        given environment: Environment = fixedEnvironment(t"MYAPP_PORT" -> t"80")
        given system: System = fixedSystem()
        (Configurator.properties ++ Configurator.environment).read(t"port")
      . assert(_ == t"80")

      test(m"A composed cascade falls through to a custom source"):
        given environment: Environment = fixedEnvironment()
        given system: System = fixedSystem()
        val file: Configurator = name => if name == t"port" then t"7070" else Unset
        (Configurator.properties ++ Configurator.environment ++ file).read(t"port")
      . assert(_ == t"7070")

    suite(m"Directory tests"):
      test(m"The home directory comes from the `user.home` property"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        Directories.homeText
      . assert(_ == t"/home/jack")

      test(m"The XDG data home defaults below the home directory"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        given environment: Environment = fixedEnvironment()
        Xdg.dataHome[FakePath]
      . assert(_ == FakePath(t"/home/jack/.local/share"))

      test(m"The XDG config home defaults below the home directory"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        given environment: Environment = fixedEnvironment()
        Xdg.configHome[FakePath]
      . assert(_ == FakePath(t"/home/jack/.config"))

      test(m"The XDG data home is taken from the environment when set"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        given environment: Environment = fixedEnvironment(t"XDG_DATA_HOME" -> t"/data")
        Xdg.dataHome[FakePath]
      . assert(_ == FakePath(t"/data"))

      test(m"The XDG runtime directory is unset when the environment omits it"):
        given environment: Environment = fixedEnvironment()
        Xdg.runtimeDir[FakePath]
      . assert(_ == Unset)

      test(m"The XDG data directories have standard defaults"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        given environment: Environment = fixedEnvironment()
        Xdg.dataDirs[FakePath]
      . assert(_ == List(FakePath(t"/usr/local/share"), FakePath(t"/usr/share")))

      test(m"The XDG config directories have a standard default"):
        given system: System = fixedSystem(t"user.home" -> t"/home/jack")
        given environment: Environment = fixedEnvironment()
        Xdg.configDirs[FakePath]
      . assert(_ == List(FakePath(t"/etc/xdg")))

      test(m"On Windows the config home is the roaming app data directory"):
        given system: System =
          fixedSystem(t"user.home" -> t"C:\\Users\\Jack", t"os.name" -> t"Windows 11")

        given environment: Environment = fixedEnvironment()
        Directories.configHome[FakePath]
      . assert(_ == FakePath(t"C:\\Users\\Jack\\AppData\\Roaming"))

      test(m"On Windows the cache home is the local app data directory"):
        given system: System =
          fixedSystem(t"user.home" -> t"C:\\Users\\Jack", t"os.name" -> t"Windows 11")

        given environment: Environment = fixedEnvironment()
        Directories.cacheHome[FakePath]
      . assert(_ == FakePath(t"C:\\Users\\Jack\\AppData\\Local"))

      test(m"On Windows the data directories come from `PROGRAMDATA`"):
        given system: System =
          fixedSystem(t"user.home" -> t"C:\\Users\\Jack", t"os.name" -> t"Windows 11")

        given environment: Environment = fixedEnvironment()
        Directories.dataDirs[FakePath]
      . assert(_ == List(FakePath(t"C:\\ProgramData")))

    suite(m"Working directory tests"):
      test(m"The working directory comes from the `user.dir` property"):
        given system: System = fixedSystem(t"user.dir" -> t"/tmp/work")
        workingDirectories.systemWorkingDirectory.directory()
      . assert(_ == t"/tmp/work")

      test(m"The default working directory is absolute"):
        workingDirectories.defaultWorkingDirectory.directory().starts(t"/")
      . assert(_ == true)

    suite(m"Error message tests"):
      test(m"An environment error names the missing variable"):
        val error = unsafely(Environment.Error(t"HOME"))
        error.message.text
      . assert(_ == m"the environment variable HOME was not defined".text)

      test(m"A property error names the missing property"):
        val error = unsafely(Property.Error(t"user.home"))
        error.message.text
      . assert(_ == m"the system property user.home was not defined".text)
