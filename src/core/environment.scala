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

  def fileSeparator: ('/' | '\\') = property("file.separator".tt).s match
    case "/"  => '/'
    case "\\" => '\\'
    case _    => throw EnvironmentError("file.separator".tt, true)

  def pathSeparator: (':' | ';') = property("path.separator".tt).s match
    case ";" => ';'
    case ":" => ':'
    case _    => throw EnvironmentError("path.separator".tt, true)

  def javaClassPath[PathType](using GenericPathMaker[PathType]): List[PathType] =
    property("java.class.path".tt).s.split(pathSeparator).to(List).map(makeGenericPath(_))

  def javaHome[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property("java.home".tt).s)

  def javaVendor: Text = property("java.vendor".tt)
  def javaVendorUrl: Text = property("java.vendor.url".tt)
  def javaVersion: Text = property("java.version".tt)

  def javaSpecificationVersion: Int =
    property("java.specification.version".tt) match
      case As[Int](version) => version
      case other            => throw EnvironmentError("java.specification.version".tt, true)

  def lineSeparator: Text = property("line.separator".tt)
  def osArch: Text = property("os.arch".tt)
  def osVersion: Text = property("os.version".tt)

  def userDir[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property("user.dir".tt).s)

  def userHome[PathType](using GenericPathMaker[PathType]): PathType =
    makeGenericPath(property("user.home".tt).s)

  def userName: Text = property("user.name".tt)

  def pwd[PathType](using GenericPathMaker[PathType]): PathType =
    getProperty("user.dir".tt) match
      case path: Text => makeGenericPath(path.s)
      case _          => getEnv("PWD".tt) match
        case path: Text => makeGenericPath(path.s)
        case _          => throw EnvironmentError("user.dir".tt, true)

case class EnvironmentError(variable: Text, property: Boolean)
extends Error(msg"the ${if property then "system property".tt else "environment variable".tt} $variable was not found")

package environments:
  given system(using CanThrow[EnvironmentError]): Environment = StandardEnvironment(
    v => Option(System.getenv(v.s)).map(_.nn).map(_.tt).maybe,
    v => Option(System.getProperty(v.s)).map(_.nn).map(_.tt).maybe
  )

  given restricted(using CanThrow[EnvironmentError]): Environment =
    StandardEnvironment(v => Unset, v =>
        Option(System.getProperty(v.s)).map(_.nn).map(_.tt).maybe)

  given empty(using CanThrow[EnvironmentError]): Environment =
    StandardEnvironment(v => Unset, v => Unset)

inline def env(using env: Environment): Environment = env
