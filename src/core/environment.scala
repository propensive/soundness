/*
    Ambience, version 0.4.0. Copyright 2023-23 Jon Pretty, Propensive OÜ.

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

@implicitNotFound("rudiments: a contextual Environment instance is required, for example one of:\n"+
                  "    given Environment = environments.empty       // no environment variables or system properties\n"+
                  "    given Environment = environments.restricted  // access to system properties, but no environment variables\n"+
                  "    given Environment = environments.system      // full access to the JVM's environment")
class Environment(getEnv: Text => Option[Text], getProperty: Text => Option[Text]):
  def apply(variable: Text): Maybe[Text] = getEnv(variable) match
    case None        => Unset
    case Some(value) => value

  def property(variable: Text): Text throws EnvError =
    getProperty(variable).getOrElse(throw EnvError(variable, true))

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
    makeGenericPath(property(Text("java.home")).s).getOrElse(throw EnvError(Text("java.home"), true))

  def javaVendor: Text throws EnvError = property(Text("java.vendor"))
  def javaVendorUrl: Text throws EnvError = property(Text("java.vendor.url"))
  def javaVersion: Text throws EnvError = property(Text("java.version"))

  def javaSpecificationVersion: Int throws EnvError = property(Text("java.specification.version")) match
    case As[Int](version) => version
    case other            => throw EnvError(Text("java.specification.version"), true)

  def lineSeparator: Text throws EnvError = property(Text("line.separator"))
  def osArch: Text throws EnvError = property(Text("os.arch"))
  def osVersion: Text throws EnvError = property(Text("os.version"))

  def userDir[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    makeGenericPath(property(Text("user.dir")).s).getOrElse(throw EnvError(Text("user.dir"), true))

  def userHome[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    makeGenericPath(property(Text("user.home")).s).getOrElse(throw EnvError(Text("user.home"), true))

  def userName: Text throws EnvError = property(Text("user.name"))

  def pwd[PathType](using GenericPathMaker[PathType]): PathType throws EnvError =
    val userDir = try property(Text("user.dir")) catch case err: Exception => Unset
    apply(Text("PWD")).or(userDir).fm(throw EnvError(Text("user.dir"), true)): path =>
      makeGenericPath(path.s).getOrElse(throw EnvError(Text("user.dir"), true))

case class EnvError(variable: Text, property: Boolean)
extends Error(ErrorMessage[Text *: EmptyTuple](
  List(Text(if property then "the system property " else "the environment variable "), Text(" was not found")),
  variable *: EmptyTuple
))

package environments:
  given system: Environment(
    v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)),
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_))
  )

  given restricted: Environment(
    v => None,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_))
  )

  given empty: Environment(v => None, v => None)

inline def env(using env: Environment): Environment = env