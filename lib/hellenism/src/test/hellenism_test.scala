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

import soundness.*

import classloaders.threadContextClassloader

trait TestService:
  def name: Text

class TestServiceA extends TestService:
  def name: Text = t"A"

class TestServiceB extends TestService:
  def name: Text = t"B"

object Tests extends Suite(m"Hellenism Tests"):
  def run(): Unit =
    test(m"check that a classpath file is accessible"):
      cp"/scala/Option.class"
    . assert()

    test(m"Decode a classpath"):
      unsafely:
        t"/scala/Option.class".as[Path on Classpath]
    . assert(_ == Classpath / "scala" / "Option.class")

    test(m"check that a classpath file is streamable"):
      cp"/scala/Option.class".read[Data]
    . assert(_.readable.length > 0)

    test(m"check that a nonexistent classpath file is an error"):
      demilitarize(cp"/missing.txt").map(_.message)
    . assert(_ == List(t"hellenism: the path /missing.txt is not on the classpath"))

    test(m"check that an invalid classpath path is an error"):
      demilitarize(cp"foobar").map(_.message)
    . assert(_ == List(t"hellenism: the path foobar is not a valid classpath path"))

    test(m"load services from META-INF/services"):
      import systems.javaSystem
      val classpath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      classpath.services[TestService].stdlib.map(_.name).to(Set)
    . assert(_ == Set(t"A", t"B"))

    suite(m"Native-rendering coverage"):
      val classpath = LocalClasspath(Classpath.Entry.Jar(t"/x.jar"),
                                     Classpath.Entry.Directory(t"/a/b/"))

      test(m"hellenism's types inspect natively"):
        Inspectable.fallbacks(classpath.inspect, ClassRef(classOf[String]).inspect)
      . assert(_ == Nil)

      test(m"A classpath shows its entries, separated by colons"):
        classpath.inspect
      . assert(_ == t"classpath⟨/x.jar:/a/b/⟩")

      test(m"A class reference shows the source which produces it"):
        ClassRef(classOf[String]).inspect
      . assert(_ == t"classOf[java.lang.String]")
