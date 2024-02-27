/*
    Ambience, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ambience

import anticipation.*
import spectacular.*
import contingency.*
import rudiments.*
import vacuous.*
import fulminate.*
import gossamer.*

import scala.compiletime.ops.string.*

import language.experimental.captureChecking
import language.dynamics

@capability
trait SystemProperties:
  def apply(name: Text): Optional[Text]

object Properties extends Dynamic:
  given default(using Quickstart): SystemProperties = systemProperties.virtualMachine
  
  def apply[PropertyType](property: Text)
      (using properties: SystemProperties, reader: SystemProperty[String, PropertyType],
          systemProperty: Raises[SystemPropertyError])
          : PropertyType^{properties, reader, systemProperty} =

    properties(property).let(reader.read).or(abort(SystemPropertyError(property)))
    
  def selectDynamic(key: String): PropertyAccess[key.type] = PropertyAccess[key.type](key)

@capability
trait SystemProperty[NameType <: String, PropertyType]:
  def read(value: Text): PropertyType

object SystemProperty:
  given generic[UnknownType <: String & Singleton](using DummyImplicit): SystemProperty[UnknownType, Text] =
    identity(_)
  
  given javaHome[PathType: SpecificPath]: SystemProperty["java.home", PathType] = SpecificPath(_)
  
  given javaLibraryPath[PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
          : SystemProperty["java.library.path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given javaClassPath[PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
          : SystemProperty["java.class.path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given javaVersion: SystemProperty["java.version", Text] = identity(_)
  given javaRuntimeVersion: SystemProperty["java.runtime.version", Text] = identity(_)
  
  given javaExtDirs[PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
          : SystemProperty["java.ext.dirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given fileSeparator: SystemProperty["file.separator", Char] = _.decodeAs[Char]
  given pathSeparator: SystemProperty["path.separator", Char] = _.decodeAs[Char]
  given lineSeparator: SystemProperty["line.separator", Text] = identity(_)

  given userName: SystemProperty["user.name", Text] = identity(_)
  given userHome[PathType: SpecificPath]: SystemProperty["user.home", PathType] = SpecificPath(_)
  given userDir[PathType: SpecificPath]: SystemProperty["user.dir", PathType] = SpecificPath(_)

  given osName: SystemProperty["os.name", Text] = identity(_)
  given osVersion: SystemProperty["os.version", Text] = identity(_)
  given osArch: SystemProperty["os.arch", Text] = identity(_)

  given decoder[UnknownType <: String & Singleton, PropertyType](using decoder: Decoder[PropertyType])
          : SystemProperty[UnknownType, PropertyType] =

    decoder.decode(_)

case class PropertyAccess[NameType <: String](property: String) extends Dynamic:
  def selectDynamic(key: String): PropertyAccess[NameType+"."+key.type] =
    PropertyAccess[NameType+"."+key.type](property+"."+key)
  
  def applyDynamic[PropertyType](key: String)()
      (using properties: SystemProperties,
          reader: SystemProperty[NameType+"."+key.type, PropertyType],
          systemProperty: Raises[SystemPropertyError])
          : PropertyType^{properties, reader, systemProperty} =

    properties((property+"."+key).tt).let(reader.read(_)).or:
      abort(SystemPropertyError((property+"."+key).tt))
  
  inline def apply[PropertyType]()
      (using properties: SystemProperties, reader: SystemProperty[NameType, PropertyType],
          systemProperty: Raises[SystemPropertyError])
          : PropertyType^{properties, reader, systemProperty} =

    properties(valueOf[NameType].tt).let(reader.read(_)).or:
      abort(SystemPropertyError(valueOf[NameType].tt))

case class SystemPropertyError(property: Text)
extends Error(msg"the system property $property was not defined")

package systemProperties:
  given empty: SystemProperties with
    def apply(name: Text): Unset.type = Unset

  given virtualMachine: SystemProperties with
    def apply(name: Text): Optional[Text] = Optional(System.getProperty(name.s)).let(_.tt)
