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

trait Property extends Typeclass, Topical:
  type Self <: String
  def read(value: Optional[Text], property: Text): Topic

object Property:
  case class Access[name <: String](property: String) extends Dynamic:
    def selectDynamic(key: String): Access[name+"."+key.type] =
      Access[name+"."+key.type](property+"."+key)


    def applyDynamic[property](key: String)()
      ( using properties: System, reader: (name+"."+key.type) is Property of property )
    : property =

        val name = property+"."+key
        reader.read(properties(name.tt), name.tt)


    inline def apply[property]()
      ( using properties: System, reader: name is Property of property )
    : property =

        val name = valueOf[name]
        reader.read(properties(name.tt), name.tt)

  def apply[name <: String, property](lambda: Text => property)
  : name is Property of property =
      (value, property) =>
        lambda(value.or(panic(m"the system property $property was unavailable")))


  given generic: [label <: String & Singleton] => Tactic[PropertyError]
  =>  label is Property of Text =

      (value, property) => value.lest(PropertyError(property))


  given javaHome: [path: Instantiable across Paths from Text]
  =>  ( "java.home" is Property of path ) =

      Property(path(_))


  // given javaLibraryPath: [path: Instantiable across Paths from Text]
  //       => (system: System, property: Tactic[PropertyError])
  //       =>  Property["java.library.path", List[path]] =

  //   _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))


  // given javaClassPath: [path: Instantiable across Paths from Text]
  //       => (system: System, property: Tactic[PropertyError])
  //       =>  Property["java.class.path", List[path]] =

  //   _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))


  given javaVersion: ("java.version" is Property of Text) = Property(identity)
  given javaVendor: ("java.vendor" is Property of Text) = Property(identity)
  given javaVendorUrl: ("java.vendor.url" is Property of Text) = Property(identity)


  given javaRuntimeVersion: Tactic[PropertyError]
  =>  ( "java.runtime.version" is Property of Text ) =

      (value, name) => value.lest(PropertyError(name))


  given javaClassVersion: ("java.runtime.version" is Property of Int) =
    given Tactic[NumberError] = strategies.throwUnsafely
    Property(_.decode[Int])


  // given javaExtDirs: [path: Instantiable across Paths from Text]
  // =>  ( system: System, property: Tactic[PropertyError] )
  // =>  Property["java.ext.dirs", List[path]] =

  //   _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))


  given fileSeparator: ("file.separator" is Property of Char) = Property(_.decode[Char])
  given pathSeparator: ("path.separator" is Property of Char) = Property(_.decode[Char])
  given lineSeparator: ("line.separator" is Property of Text) = Property(identity)
  given userName: ("user.name" is Property of Text) = Property(identity)


  given userHome: [path: Instantiable across Paths from Text]
  =>  ( "user.home" is Property of path ) =

      Property(path(_))


  given userDir: [path: Instantiable across Paths from Text]
  =>  ( "user.dir" is Property of path ) =

      Property(path(_))


  given osName: ("os.name" is Property of Text) = Property(identity)
  given osVersion: ("os.version" is Property of Text) = Property(identity)

  given osArch: ("os.arch" is Property of Architecture) =
    Property(_.decode[Architecture])


  given decoder: [label <: Label, property] => (decoder: property is Decodable in Text)
  =>  Tactic[PropertyError]
  =>  label is Property of property =

    (value, name) =>
      decoder.decoded(value.lest(PropertyError(name)))
