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
┃    Soundness, version 0.40.0.                                                                    ┃
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

import language.dynamics

import scala.compiletime.ops.string.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import vacuous.*

trait SystemProperty[name <: String, property]:
  def read(value: Optional[Text], property: Text): property

object SystemProperty:
  def apply[name <: String, property](lambda: Text => property): SystemProperty[name, property] =
    new:
      def read(value: Optional[Text], property: Text): property =
        lambda(value.or(panic(m"the system property $property was unavailable")))

  given generic: [unknown <: String & Singleton] => Tactic[SystemPropertyError]
        => SystemProperty[unknown, Text] =
    (value, property) => value.lest(SystemPropertyError(property))


  given javaHome: [path: Instantiable across Paths from Text] => SystemProperty["java.home", path] =
    SystemProperty(path(_))


  // given javaLibraryPath: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.library.path", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  // given javaClassPath: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.class.path", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  given javaVersion: SystemProperty["java.version", Text] = SystemProperty(identity)
  given javaVendor: SystemProperty["java.vendor", Text] = SystemProperty(identity)
  given javaVendorUrl: SystemProperty["java.vendor.url", Text] = SystemProperty(identity)


  given javaRuntimeVersion: Tactic[SystemPropertyError]
        => SystemProperty["java.runtime.version", Text] =
    (value, name) => value.lest(SystemPropertyError(name))


  given javaClassVersion: Tactic[NumberError] => SystemProperty["java.runtime.version", Int] =
    SystemProperty(_.decode[Int])


  // given javaExtDirs: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.ext.dirs", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  given fileSeparator: SystemProperty["file.separator", Char] = SystemProperty(_.decode[Char])
  given pathSeparator: SystemProperty["path.separator", Char] = SystemProperty(_.decode[Char])
  given lineSeparator: SystemProperty["line.separator", Text] = SystemProperty(identity)
  given userName: SystemProperty["user.name", Text] = SystemProperty(identity)


  given userHome: [path: Instantiable across Paths from Text] => SystemProperty["user.home", path] =
    SystemProperty(path(_))


  given userDir: [path: Instantiable across Paths from Text] => SystemProperty["user.dir", path] =
    SystemProperty(path(_))


  given osName: SystemProperty["os.name", Text] = SystemProperty(identity)
  given osVersion: SystemProperty["os.version", Text] = SystemProperty(identity)
  given osArch: SystemProperty["os.arch", Architecture] = SystemProperty(_.decode[Architecture])


  given decoder: [unknown <: Label, property] => (decoder: property is Decodable in Text)
        =>  Tactic[SystemPropertyError]
        =>  SystemProperty[unknown, property] =

    (value, name) =>
      decoder.decoded(value.lest(SystemPropertyError(name)))
