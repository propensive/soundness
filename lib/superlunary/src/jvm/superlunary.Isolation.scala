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
package superlunary

import anthology.*
import anticipation.*
import austronesian.*
import galilei.*
import gossamer.*
import hellenism.*
import prepositional.*
import serpentine.*
import vacuous.*

// The rig's own classloader rather than the system classloader: they coincide in a plain
// `java -cp` process, but under a test-running host such as fume the rig (with the code it
// stages) lives in an isolating `URLClassLoader`, and the system loader knows only the host.

object Isolation extends Rig(using Classloader[Isolation.type]):
  type Result[output] = output
  type Form = scala.Array[Pojo]
  type Target = Classloader
  type Transport = Pojo

  def stage(out: Path on Linux): Classloader = classpath(out).classloader()

  val scalac: Scalac[3.6, Universe.Classfile] = Scalac[3.6](List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Form, Target]): output =
    // The bridge crosses through an `AnyRef` rim (the kernel-module-sep idiom): a directly
    // typed lambda mints fresh read capabilities that cannot match the stage's own.
    val bridge: AnyRef = ((input: scala.Array[Pojo]) =>
      val classloader: Classloader = stage.target
      val cls = classloader.on(t"Generated$$Code$$From$$Quoted").or(???)
      val instance = cls.getDeclaredConstructor().nn.newInstance().nn
      val method = cls.getMethod("apply").nn
      val function = method.invoke(instance).nn
      val cls2 = function.getClass
      val method2 = function.getClass.getMethod("apply", classOf[Object]).nn
      method2.setAccessible(true)
      val result =
        method2.invoke(function, input.asInstanceOf[scala.Array[AnyRef | Null]])
        . asInstanceOf[scala.Array[AnyRef | Null]]

      // A pure-typed copy via the Java API, so the bridge's result carries no capability.
      java.util.Arrays.copyOf(result, result.length).nn.asInstanceOf[scala.Array[Pojo]]
    ).asInstanceOf[AnyRef]

    stage.remote.asInstanceOf[AnyRef => AnyRef](bridge).asInstanceOf[output]
