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
package probably

import java.nio.file as jnf
import java.util.concurrent as juc

import soundness.*

// `Casket`, recompiled with probes from the TASTy this test classpath holds for it, and loaded
// beside the uninstrumented one: by a classloader which finds that one class itself, and leaves
// every other — `Coverage` among them, so that there is only one — to the suite's.
object Probed:
  private val name: String = "probably.Casket"
  private val parent: ClassLoader = getClass.getClassLoader.nn

  private val classpath: scala.List[String] = Instrumenter.classpath(parent)
  private val cache: String = jnf.Files.createTempDirectory("probably-coverage").nn.toString

  // The plan for the class, named as a class and as the source file it was compiled from.
  val plan: Instrumenter.Plan = Instrumenter.plan(classpath, scala.List(name), scala.Nil, cache)

  def sourced: Instrumenter.Plan =
    Instrumenter.plan(classpath, scala.List("test/probably.Casket.scala"), scala.Nil, cache)

  def absent: Instrumenter.Plan =
    Instrumenter.plan(classpath, scala.List("probably.Nerissa"), scala.Nil, cache)

  val probes: scala.List[Instrumenter.Probe] = plan.units.flatMap(_.probes)
  def kinds: scala.List[String] = probes.map(_.kind).distinct.sorted

  private val loader: ClassLoader =
    val classes: java.net.URL = jnf.Path.of(plan.classes).nn.toUri.nn.toURL.nn

    new java.net.URLClassLoader(scala.Array[java.net.URL | Null](classes), parent):
      override def loadClass(name: String | Null, resolve: Boolean): Class[?] | Null =
        if name != Probed.name then super.loadClass(name, resolve) else
          val loaded: Class[?] | Null = findLoadedClass(name)
          if loaded == null then findClass(name) else loaded

  // A casket of the recompiled class, as the interface the two classes share.
  def make(threshold: Int): Lottery =
    val casket: Class[?] = loader.loadClass(name).nn
    val constructor: java.lang.reflect.Constructor[?] = casket.getConstructor(Integer.TYPE).nn
    constructor.newInstance(Int.box(threshold)).asInstanceOf[Lottery]

  // What each name is credited with since the last time this was asked: every probe reached,
  // as its kind and the method it is in.
  def drained(): scala.collection.immutable.Map[String, scala.List[String]] =
    Instrumenter.drain(parent).map: reached =>
      val described: scala.List[String] = reached.probes.flatMap: (unit, index) =>
        probes.filter { probe => unit == 0 && probe.index == index }.map: probe =>
          probe.kind+":"+probe.scope.substring(probe.scope.lastIndexOf('.') + 1).nn

      reached.test -> described.sorted

    . toMap

// A suite whose one test does whatever it is given to do, run through `invoke` by the test
// which checks that the runner brackets a test's body.
class Chooser(action: () => Unit) extends Suite(m"chooser"):
  def run(): Unit = test(m"lead")(action()).check(_ => true)
