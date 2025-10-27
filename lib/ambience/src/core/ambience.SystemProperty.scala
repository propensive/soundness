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
┃    Soundness, version 0.45.0.                                                                    ┃
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

trait SystemProperty extends Typeclass, Topical:
  type Self <: String
  def read(value: Optional[Text], property: Text): Topic

object SystemProperty:
  def apply[name <: String, property](lambda: Text => property)
  : name is SystemProperty of property =
      (value, property) =>
        lambda(value.or(panic(m"the system property $property was unavailable")))

  given generic: [label <: String & Singleton] => Tactic[SystemPropertyError]
        => label is SystemProperty of Text =
    (value, property) => value.lest(SystemPropertyError(property))


  given javaHome: [path: Instantiable across Paths from Text]
        => ("java.home" is SystemProperty of path) =

      SystemProperty(path(_))


  // given javaLibraryPath: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.library.path", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  // given javaClassPath: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.class.path", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  given javaVersion: ("java.version" is SystemProperty of Text) = SystemProperty(identity)
  given javaVendor: ("java.vendor" is SystemProperty of Text) = SystemProperty(identity)
  given javaVendorUrl: ("java.vendor.url" is SystemProperty of Text) = SystemProperty(identity)


  given javaRuntimeVersion: Tactic[SystemPropertyError]
        => ("java.runtime.version" is SystemProperty of Text) =
    (value, name) => value.lest(SystemPropertyError(name))


  given javaClassVersion: ("java.runtime.version" is SystemProperty of Int) =
    given Tactic[NumberError] = strategies.throwUnsafely
    SystemProperty(_.decode[Int])


  // given javaExtDirs: [path: Instantiable across Paths from Text]
  //       => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  //       =>  SystemProperty["java.ext.dirs", List[path]] =

  //   _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))


  given fileSeparator: ("file.separator" is SystemProperty of Char) = SystemProperty(_.decode[Char])
  given pathSeparator: ("path.separator" is SystemProperty of Char) = SystemProperty(_.decode[Char])
  given lineSeparator: ("line.separator" is SystemProperty of Text) = SystemProperty(identity)
  given userName: ("user.name" is SystemProperty of Text) = SystemProperty(identity)


  given userHome: [path: Instantiable across Paths from Text]
        => ("user.home" is SystemProperty of path) =
    SystemProperty(path(_))


  given userDir: [path: Instantiable across Paths from Text]
        => ("user.dir" is SystemProperty of path) =

      SystemProperty(path(_))


  given osName: ("os.name" is SystemProperty of Text) = SystemProperty(identity)
  given osVersion: ("os.version" is SystemProperty of Text) = SystemProperty(identity)

  given osArch: ("os.arch" is SystemProperty of Architecture) =
    SystemProperty(_.decode[Architecture])


  given decoder: [label <: Label, property] => (decoder: property is Decodable in Text)
        =>  Tactic[SystemPropertyError]
        =>  label is SystemProperty of property =

    (value, name) =>
      decoder.decoded(value.lest(SystemPropertyError(name)))
