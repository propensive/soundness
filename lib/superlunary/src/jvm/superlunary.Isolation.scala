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
┃    Soundness, version 0.34.0.                                                                    ┃
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
package superlunary

import scala.reflect.Selectable.reflectiveSelectable

import ambience.*, systemProperties.jre
import anthology.*
import anticipation.*
import austronesian.*
import contingency.*
import distillate.*
import eucalyptus.*
import gossamer.*
import guillotine.*
import hellenism.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

import charDecoders.utf8
import textSanitizers.skip
import systemProperties.jre
import classloaders.system

object Isolation extends Rig:
  type Result[output] = output
  type Form = Array[Pojo]
  type Target = Classloader
  type Transport = Pojo

  def deploy(out: Path on Linux): Classloader = classloaders.threadContext.classpath match
    case classpath: LocalClasspath =>
      LocalClasspath(classpath.entries :+ Classpath.Directory(out)).classloader()

    case _ =>
      val systemClasspath = unsafely(Properties.java.`class`.path().decode[LocalClasspath])
      LocalClasspath(Classpath.Directory(out) :: systemClasspath.entries).classloader()


  val scalac: Scalac[3.6] = Scalac[3.6](List(scalacOptions.experimental))

  protected def invoke[output](dispatch: Dispatch[output, Form, Target]): output =
    import workingDirectories.systemProperties
    import logging.silent

    dispatch.remote: input =>
      val classloader: Classloader = dispatch.target
      val cls = classloader.on(t"Generated$$Code$$From$$Quoted").or(???)
      val instance = cls.getDeclaredConstructor().nn.newInstance().nn
      val method = cls.getMethod("apply").nn
      val function = method.invoke(instance).nn
      val cls2 = function.getClass.nn
      val method2 = function.getClass.nn.getMethod("apply", classOf[Object]).nn
      method2.setAccessible(true)
      method2.invoke(function, input).asInstanceOf[Array[Pojo]]
