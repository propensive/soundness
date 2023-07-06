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
import rudiments.*

import language.experimental.captureChecking

@missingContext(contextMessage(module = "ambience", typeclass = "Environment")(
  "environments.empty"      -> "no environment variables",
  "environments.restricted" -> "access to system properties, but no environment variables",
  "environments.system"     -> "full access to the JVM's environment"
))

trait Environment:
  def apply(variable: Text): Maybe[Text]
  def property(variable: Text): Text
  def fileSeparator: ('/' | '\\')
  def pathSeparator: (':' | ';')
  def javaClassPath[PathType](using GenericPathMaker[PathType]): List[PathType]
  def javaHome[PathType](using GenericPathMaker[PathType]): PathType
  def javaVendor: Text
  def javaVendorUrl: Text
  def javaVersion: Text
  def javaSpecificationVersion: Int
  def lineSeparator: Text
  def osArch: Text
  def osVersion: Text
  def userDir[PathType](using GenericPathMaker[PathType]): PathType
  def userHome[PathType](using GenericPathMaker[PathType]): PathType
  def userName: Text
  def pwd[PathType](using GenericPathMaker[PathType]): PathType
  

@capability
class StandardEnvironment
    (getEnv: Text -> Maybe[Text], getProperty: Text -> Maybe[Text])
    (using CanThrow[EnvironmentError])
extends Environment:
  def apply(variable: Text): Maybe[Text] = getEnv(variable)

  def property(variable: Text): Text =
    getProperty(variable).option.getOrElse(throw EnvironmentError(variable, true))

  def fileSeparator: ('/' | '\\') = property(Text("file.separator")).s match
    case "/"  => '/'
    case "\\" => '\\'
    case _    => throw EnvironmentError(Text("file.separator"), true)

  def pathSeparator: (':' | ';') = property(Text("path.separator")).s match
    case ";" => ';'
    case ":" => ':'
    case _    => throw EnvironmentError(Text("path.separator"), true)

  def javaClassPath[PathType](using GenericPathMaker[PathType]): List[PathType] =
    property(Text("java.class.path")).s.split(pathSeparator).to(List).map(makeGenericPath(_))

  def javaHome[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property(Text("java.home")).s)

  def javaVendor: Text = property(Text("java.vendor"))
  def javaVendorUrl: Text = property(Text("java.vendor.url"))
  def javaVersion: Text = property(Text("java.version"))

  def javaSpecificationVersion: Int =
    property(Text("java.specification.version")) match
      case As[Int](version) => version
      case other            => throw EnvironmentError(Text("java.specification.version"), true)

  def lineSeparator: Text = property(Text("line.separator"))
  def osArch: Text = property(Text("os.arch"))
  def osVersion: Text = property(Text("os.version"))

  def userDir[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property(Text("user.dir")).s)

  def userHome[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property(Text("user.home")).s)

  def userName: Text = property(Text("user.name"))

  def pwd[PathType](using GenericPathMaker[PathType]): PathType =
    getProperty(Text("user.dir")) match
      case path: Text => makeGenericPath(path.s)
      case _          => getEnv(Text("PWD")) match
        case path: Text => makeGenericPath(path.s)
        case _          => throw EnvironmentError(Text("user.dir"), true)

case class EnvironmentError(variable: Text, property: Boolean)
extends Error(msg"the ${Text(if property then "system property" else "environment variable")} $variable was not found")

package environments:
  given system(using CanThrow[EnvironmentError]): Environment = StandardEnvironment(
    v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)).maybe,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe
  )

  given restricted(using CanThrow[EnvironmentError]): Environment =
    StandardEnvironment(v => Unset, v =>
        Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe)

  given empty(using CanThrow[EnvironmentError]): Environment =
    StandardEnvironment(v => Unset, v => Unset)

inline def env(using env: Environment): Environment = env
