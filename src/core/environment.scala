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

  def property(variable: Text): Text throws EnvError =
    getProperty(variable).option.getOrElse(throw EnvError(variable, true))

  def fileSeparator: ('/' | '\\') throws EnvError = property(Text("file.separator")).s match
    case "/"  => '/'
    case "\\" => '\\'
    case _    => throw EnvError(Text("file.separator"), true)

  def pathSeparator: (':' | ';') throws EnvError = property(Text("path.separator")).s match
    case ";" => ';'
    case ":" => ':'
    case _    => throw EnvError(Text("path.separator"), true)

  def javaClassPath[PathType](using GenericPathMaker[PathType]): List[PathType] throws EnvError =
    property(Text("java.class.path")).s.split(pathSeparator).to(List).flatMap(makeGenericPath(_))

  def javaHome[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    makeGenericPath(property(Text("java.home")).s).getOrElse:
      throw EnvError(Text("java.home"), true)

  def javaVendor: Text throws EnvError = property(Text("java.vendor"))
  def javaVendorUrl: Text throws EnvError = property(Text("java.vendor.url"))
  def javaVersion: Text throws EnvError = property(Text("java.version"))

  def javaSpecificationVersion: Int throws EnvError =
    property(Text("java.specification.version")) match
      case As[Int](version) => version
      case other            => throw EnvError(Text("java.specification.version"), true)

  def lineSeparator: Text throws EnvError = property(Text("line.separator"))
  def osArch: Text throws EnvError = property(Text("os.arch"))
  def osVersion: Text throws EnvError = property(Text("os.version"))

  def userDir[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    makeGenericPath(property(Text("user.dir")).s).getOrElse(throw EnvError(Text("user.dir"), true))

  def userHome[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    makeGenericPath(property(Text("user.home")).s).getOrElse:
      throw EnvError(Text("user.home"), true)

  def userName: Text throws EnvError = property(Text("user.name"))

  def pwd[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    val dirOption = getProperty(Text("user.dir")) match
      case path: Text => makeGenericPath(path.s)
      case _          => getEnv(Text("PWD")) match
        case path: Text => makeGenericPath(path.s)
        case _          => throw EnvError(Text("user.dir"), true)
    
    dirOption match
      case Some(dir) => dir
      case None      => throw EnvError(Text("user.dir"), true)

case class EnvError(variable: Text, property: Boolean)
extends Error(ErrorMessage(
  List(
    Text(if property then "the system property " else "the environment variable "),
    Text(" was not found")
  ),
  List(variable)
))

package environments:
  given system: Environment(
    v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)).maybe,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe
  )

  given restricted: Environment(
    v => Unset,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_)).maybe
  )

  given empty: Environment(v => Unset, v => Unset)

inline def env(using env: Environment): Environment = env
