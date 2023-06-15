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
@capability
class Environment(getEnv: Text -> Maybe[Text], getProperty: Text -> Maybe[Text]):
  def apply(variable: Text): Maybe[Text] = getEnv(variable)

  def property(variable: Text): Text throws EnvironmentError =
    getProperty(variable).option.getOrElse(throw EnvironmentError(variable, true))

  def fileSeparator: ('/' | '\\') throws EnvironmentError = property(Text("file.separator")).s match
    case "/"  => '/'
    case "\\" => '\\'
    case _    => throw EnvironmentError(Text("file.separator"), true)

  def pathSeparator: (':' | ';') throws EnvironmentError = property(Text("path.separator")).s match
    case ";" => ';'
    case ":" => ':'
    case _    => throw EnvironmentError(Text("path.separator"), true)

  def javaClassPath[PathType](using GenericPathMaker[PathType]): List[PathType] throws EnvironmentError =
    property(Text("java.class.path")).s.split(pathSeparator).to(List).flatMap(makeGenericPath(_))

  def javaHome[PathType](using GenericPathMaker[PathType]): PathType throws EnvironmentError =
    makeGenericPath(property(Text("java.home")).s).getOrElse:
      throw EnvironmentError(Text("java.home"), true)

  def javaVendor: Text throws EnvironmentError = property(Text("java.vendor"))
  def javaVendorUrl: Text throws EnvironmentError = property(Text("java.vendor.url"))
  def javaVersion: Text throws EnvironmentError = property(Text("java.version"))

  def javaSpecificationVersion: Int throws EnvironmentError =
    property(Text("java.specification.version")) match
      case As[Int](version) => version
      case other            => throw EnvironmentError(Text("java.specification.version"), true)

  def lineSeparator: Text throws EnvironmentError = property(Text("line.separator"))
  def osArch: Text throws EnvironmentError = property(Text("os.arch"))
  def osVersion: Text throws EnvironmentError = property(Text("os.version"))

  def userDir[PathType](using GenericPathMaker[PathType]): PathType throws EnvironmentError =
    makeGenericPath(property(Text("user.dir")).s).getOrElse(throw EnvironmentError(Text("user.dir"), true))

  def userHome[PathType](using GenericPathMaker[PathType]): PathType throws EnvironmentError =
    makeGenericPath(property(Text("user.home")).s).getOrElse:
      throw EnvironmentError(Text("user.home"), true)

  def userName: Text throws EnvironmentError = property(Text("user.name"))

  def pwd[PathType](using GenericPathMaker[PathType]): PathType throws EnvironmentError =
    val dirOption = getProperty(Text("user.dir")) match
      case path: Text => makeGenericPath(path.s)
      case _          => getEnv(Text("PWD")) match
        case path: Text => makeGenericPath(path.s)
        case _          => throw EnvironmentError(Text("user.dir"), true)
    
    dirOption match
      case Some(dir) => dir
      case None      => throw EnvironmentError(Text("user.dir"), true)

case class EnvironmentError(variable: Text, property: Boolean)
extends Error(ErrorMessage(
  List(
    Text(if property then "the system property " else "the environment variable "),
    Text(" was not found")
  ),
  List(variable)
))

package environments:
  given system(using CanThrow[EnvironmentError]): Environment = Environment(
    v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)).maybe,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe
  )

  given restricted(using CanThrow[EnvironmentError]): Environment =
    Environment(v => Unset, v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe)

  given empty(using CanThrow[EnvironmentError]): Environment =
    Environment(v => Unset, v => Unset)

inline def env(using env: Environment): Environment = env
