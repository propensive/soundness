/*
    Hellenism, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import java.io as ji
import java.net as jn

import anticipation.*
import contingency.*
import rudiments.*
import serpentine.*
import vacuous.*

object Classloader:
  def threadContext: Classloader = new Classloader(Thread.currentThread.nn.getContextClassLoader.nn)
  inline def apply[ClassType <: AnyKind]: Classloader = ClassRef[ClassType].classloader

class Classloader(val java: ClassLoader) extends Root("/".tt, "/".tt, Case.Sensitive):
  type Platform = Classpath
  override def parent: Optional[Classloader] = Optional(java.getParent).let(new Classloader(_))

  protected def urlClassloader: Optional[jn.URLClassLoader] = java match
    case java: jn.URLClassLoader => java
    case _                       => parent.let(_.urlClassloader)

  def classpath: Optional[Classpath] = urlClassloader.let(Classpath(_))

  private[hellenism] def inputStream(path: Text)(using notFound: Tactic[ClasspathError])
  :     ji.InputStream =
    Optional(java.getResourceAsStream(path.s)).or(abort(ClasspathError(path)))
