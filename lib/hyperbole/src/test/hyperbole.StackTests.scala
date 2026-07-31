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
package hyperbole

import soundness.*
import proscenium.compat.*

import classloaders.threadContextClassloader
import unsafeExceptions.canThrowAny

import StackTrace.Frame.Kind

object StackFixture:
  def method(): Unit = throw Exception("method")

  def lambda(): Unit = List(1).foreach: value =>
    throw Exception("lambda")

  def defaulted(value: Int = throw Exception("default")): Int = value

  lazy val lazily: Int = throw Exception("lazily")

extension (value: Int) def extended: Int = throw Exception("extension")

object StackTests extends Suite(m"Stack-trace resolution tests"):
  def run(): Unit =
    def capture(block: => Unit): StackTrace =
      try
        block
        panic(m"the fixture did not throw")
      catch case error: Throwable => error.stackTrace.resolved

    // Captures a trace with no resolver in scope at all, which is what every caller gets by
    // default, and what every platform without a classpath gets always.
    def capture0(block: => Unit): StackTrace =
      try
        block
        panic(m"the fixture did not throw")
      catch case error: Throwable => error.stackTrace

    // Captures a trace which resolves as it is built, rather than afterwards, by importing the
    // resolver into scope; and does so through the `soundness` re-export, since a given whose
    // type is a context function is delicate to re-export.
    def captured(block: => Unit): StackTrace =
      import stackResolutions.tastyStackResolution

      try
        block
        panic(m"the fixture did not throw")
      catch case error: Throwable => error.stackTrace

    // The frames below the fixture belong to the test framework and the JDK, so every test looks
    // at the topmost frame in the fixture's own file.
    def frame(stackTrace: StackTrace): StackTrace.Frame =
      stackTrace.frames.find(_.jvmClass.starts(t"hyperbole.")).optional.vouch

    suite(m"Resolving as a trace is captured"):
      test(m"An imported resolver resolves frames at capture time"):
        frame(captured(StackFixture.lambda())).source.let(_.definition)

      . assert(_ == t"hyperbole.StackFixture.lambda.λ")

      test(m"Without an imported resolver, frames are left alone"):
        frame(capture0(StackFixture.lambda())).source

      . assert(_ == Unset)

    suite(m"Locating the TASTy for a class"):
      test(m"A module class resolves through its top-level class"):
        frame(capture(StackFixture.method())).source.let(_.path)

      . assert(_.let(_.ends(t"hyperbole.StackTests.scala")) == true)

      test(m"An unresolvable class leaves the frame untouched"):
        val frame = StackTrace.Frame(StackTrace.Method(t"java.lang.Thread", t"run()"), t"", 1, false)
        StackResolver().resolve(frame).source

      . assert(_ == Unset)

    suite(m"Naming the definition a frame was compiled from"):
      test(m"A method frame names the method"):
        frame(capture(StackFixture.method())).source.let(_.definition)

      . assert(_ == t"hyperbole.StackFixture.method")

      test(m"A lambda frame names the method containing it"):
        frame(capture(StackFixture.lambda())).source.let(_.definition)

      . assert(_ == t"hyperbole.StackFixture.lambda.λ")

      test(m"An extension method frame names the extension"):
        frame(capture(1.extended)).source.let(_.definition)

      . assert(_ == t"hyperbole.hyperbole.StackTests⁆.extended")

      test(m"A default getter is named as `rewrite` would name it"):
        frame(capture(StackFixture.defaulted())).source.let(_.definition)

      . assert(_ == t"hyperbole.StackFixture.defaultedδ₁")

      test(m"A lazy value's initializer names the value"):
        frame(capture(StackFixture.lazily)).source.let(_.definition)

      . assert(_ == t"hyperbole.StackFixture.lazily")

    suite(m"Classifying frames"):
      test(m"A lambda is classified as a lambda"):
        frame(capture(StackFixture.lambda())).source.let(_.kind)

      . assert(_ == Kind.Lambda)

      test(m"An extension method is classified as an extension"):
        frame(capture(1.extended)).source.let(_.kind)

      . assert(_ == Kind.Extension)

      test(m"A default argument is classified as a default"):
        frame(capture(StackFixture.defaulted())).source.let(_.kind)

      . assert(_ == Kind.Default)

      test(m"A lazy value's initializer is classified as an initializer"):
        frame(capture(StackFixture.lazily)).source.let(_.kind)

      . assert(_ == Kind.Initializer)

    suite(m"Quoting the source"):
      test(m"A frame carries the line of source it was compiled from"):
        frame(capture(StackFixture.method())).source.let(_.code)

      . assert(_ == t"""def method(): Unit = throw Exception("method")""")

      test(m"The line quoted for a lambda is the line inside it"):
        frame(capture(StackFixture.lambda())).source.let(_.code)

      . assert(_ == t"""throw Exception("lambda")""")
