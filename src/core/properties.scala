/*
    Ambience, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import perforate.*
import rudiments.*
import fulminate.*
import gossamer.*

import scala.compiletime.ops.string.*

import language.experimental.captureChecking
import language.dynamics

@capability
trait SystemProperties:
  def apply(name: Text): Maybe[Text]

object Properties extends Dynamic:
  given default(using Quickstart): SystemProperties = systemProperties.jvm
  
  def apply
      [PropertyType]
      (property: Text)
      (using properties: SystemProperties, reader: SystemPropertyReader[String, PropertyType],
          systemProperty: Raises[SystemPropertyError])
      : PropertyType^{properties, reader, systemProperty} =
    properties(property).mm(reader.read).or(abort(SystemPropertyError(property)))
    
  def selectDynamic(key: String): SystemProperty[key.type] = SystemProperty[key.type](key)

@capability
trait SystemPropertyReader[NameType <: String, PropertyType]:
  def read(value: Text): PropertyType

object SystemPropertyReader:
  given generic[UnknownType <: String & Singleton]: SystemPropertyReader[UnknownType, Text] =
    identity(_)
  
  given javaHome[PathType: SpecificPath]: SystemPropertyReader["java.home", PathType] =
    SpecificPath(_)
  
  given javaLibraryPath
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
      : SystemPropertyReader["java.library.path", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given javaClassPath
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
      : SystemPropertyReader["java.class.path", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given javaVersion: SystemPropertyReader["java.version", Text] = identity(_)
  given javaRuntimeVersion: SystemPropertyReader["java.runtime.version", Text] = identity(_)
  
  given javaExtDirs
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties, systemProperty: Raises[SystemPropertyError])
      : SystemPropertyReader["java.ext.dirs", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given fileSeparator: SystemPropertyReader["file.separator", Char] = _.decodeAs[Char]
  given pathSeparator: SystemPropertyReader["path.separator", Char] = _.decodeAs[Char]
  given lineSeparator: SystemPropertyReader["line.separator", Text] = identity(_)

  given userName: SystemPropertyReader["user.name", Text] = identity(_)
  
  given userHome[PathType: SpecificPath]: SystemPropertyReader["user.home", PathType] =
    SpecificPath(_)
  
  given userDir[PathType: SpecificPath]: SystemPropertyReader["user.dir", PathType] =
    SpecificPath(_)

  given osName: SystemPropertyReader["os.name", Text] = identity(_)
  given osVersion: SystemPropertyReader["os.version", Text] = identity(_)
  given osArch: SystemPropertyReader["os.arch", Text] = identity(_)

  given decoder
      [UnknownType <: String & Singleton, PropertyType]
      (using decoder: Decoder[PropertyType])
      : SystemPropertyReader[UnknownType, PropertyType] =
    decoder.decode(_)

case class SystemProperty[NameType <: String](property: String) extends Dynamic:
  def selectDynamic(key: String): SystemProperty[NameType+"."+key.type] =
    SystemProperty[NameType+"."+key.type](property+"."+key)
  
  def applyDynamic
      [PropertyType]
      (key: String)()
      (using properties: SystemProperties,
          reader: SystemPropertyReader[NameType+"."+key.type, PropertyType],
          systemProperty: Raises[SystemPropertyError])
      : PropertyType^{properties, reader, systemProperty} =
    properties((property+"."+key).tt).mm(reader.read(_)).or:
      abort(SystemPropertyError((property+"."+key).tt))
  
  inline def apply
      [PropertyType]
      ()(using properties: SystemProperties, reader: SystemPropertyReader[NameType, PropertyType],
          systemProperty: Raises[SystemPropertyError])
      : PropertyType^{properties, reader, systemProperty} =
    properties(valueOf[NameType].tt).mm(reader.read(_)).or:
      abort(SystemPropertyError(valueOf[NameType].tt))

case class SystemPropertyError(property: Text)
extends Error(msg"the system property $property was not defined")

package systemProperties:
  given empty: SystemProperties with
    def apply(name: Text): Unset.type = Unset

  given jvm: SystemProperties with
    def apply(name: Text): Maybe[Text] = Maybe(System.getProperty(name.s)).mm(_.tt)
