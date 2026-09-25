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

  // How a classloader built over a classpath resolves a name which both it and its parent
  // offer: the JVM's own order, deferring to the parent, or the reverse, preferring its own
  // entries, so that a plugin's versions of shared libraries are isolated from its host's.
  enum Delegation:
    case Deferential, Preferential

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

  // The bytes of a resource, or `Unset` if the classloader does not have it, read by a method
  // which takes no capability: a frozen array in a union result freshens to an `any.rd` as
  // soon as the method has a `^` parameter such as a logger, and that fresh capability then
  // cannot enter an enclosing `safely` block. The logging `apply` below is therefore an
  // `inline` shell over this (which stays public: a private helper would be reached through
  // an inline accessor, reintroducing the fresh root).
  def resource(path: Text): Optional[Data] =
    Optional(java.getResourceAsStream(path.s)).let: stream =>
      try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  // Logged `if data.present`, not `data.let(…)`: a lambda over the frozen bytes freshens
  // them too.
  inline def apply(path: Text)(using (Classpath.Event is Loggable)^): Optional[Data] =
    val data = resource(path)
    if data.present then Log.fine(Classpath.Event.ResourceLoaded(path))
    data

  // A real `using` clause rather than the `logs` sugar: a context-function result would
  // hide the tactic parameter, which the separation checker rejects.
  private[hellenism] def inputStream(path: Text)
    ( using Tactic[Classpath.Error], (Classpath.Event is Loggable)^ )
  :   ji.InputStream =

    Optional(java.getResourceAsStream(path.s)).lest:
      Log.warn(Classpath.Event.ResourceMissing(path))
      Classpath.Error(path)
