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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package hellenism

import java.io as ji
import java.net as jn

import anticipation.*
import beneficence.*
import contingency.*
import rudiments.*
import vacuous.*

object Classloader:
  def threadContext: Classloader = new Classloader(Thread.currentThread.nn.getContextClassLoader.nn)
  inline def apply[template <: AnyKind]: Classloader = ClassRef[template].classloader

class Classloader(val java: ClassLoader) extends Findable:
  type Plane = Classpath

  def parent: Optional[Classloader] = Optional(java.getParent).let(new Classloader(_))

  def use[result](block: => result): result =
    val classloader0 = Thread.currentThread.nn.getContextClassLoader().nn
    val thread = Thread.currentThread.nn

    try thread.setContextClassLoader(java) yet block
    finally thread.setContextClassLoader(classloader0)

  // Building a `Classpath` from a `URLClassLoader` produces a galilei-backed `LocalClasspath`, so
  // `classpath` lives in `hellenism.jvm` (as an extension); `urlClassloader` is exposed to it.
  private[hellenism] def urlClassloader: Optional[jn.URLClassLoader] = java match
    case java: jn.URLClassLoader => java
    case _                       => parent.let(_.urlClassloader)

  def on(name: Text): Optional[Class[?]] = Optional(Class.forName(name.s, true, java))

  def apply(path: Text): Optional[Data] logs ClasspathEvent =
    Optional(java.getResourceAsStream(path.s)).let: stream =>
      Log.fine(ClasspathEvent.ResourceLoaded(path))
      stream.readAllBytes().nn.immutable(using Unsafe)

  private[hellenism] def inputStream(path: Text)(using Tactic[ClasspathError])
  :   ji.InputStream logs ClasspathEvent =

    Optional(java.getResourceAsStream(path.s)).lest:
      Log.warn(ClasspathEvent.ResourceMissing(path))
      ClasspathError(path)
