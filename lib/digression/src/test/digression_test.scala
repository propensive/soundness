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
package digression

import soundness.*

import StackTrace.Frame.Kind

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Digression Tests"):
  def run(): Unit =
    suite(m"Demangling compiled names"):
      test(m"An operator name is decoded"):
        StackTrace.rewrite("$plus$plus", method = true)

      . assert(_ == t"++()")

      test(m"An anonymous function is marked as a lambda"):
        StackTrace.rewrite("Foo$$anonfun$3", method = true)

      . assert(_ == t"Foo.λ₃()")

      test(m"A default getter is marked as a default"):
        StackTrace.rewrite("defaulted$default$1")

      . assert(_ == t"defaultedδ₁")

      test(m"A constructor is marked as an initializer"):
        StackTrace.rewrite("<init>", method = true)

      . assert(_ == t"ⲛ()")

      test(m"A module class keeps its marker"):
        StackTrace.rewrite("pkg.Foo$")

      . assert(_ == t"pkg.ΞFoo")

      test(m"A name with no `.` to split on is left alone"):
        StackTrace.rewrite("Foo$")

      . assert(_ == t"Foo#")

    suite(m"Capturing a stack trace"):
      test(m"A captured frame keeps the compiled names it was built from"):
        Exception("boom").stackTrace.frames.prim.let(_.jvmClass)

      . assert(_.let(_.starts(t"digression.Tests")) == true)

      test(m"Frames are unresolved unless a resolver is imported"):
        Exception("boom").stackTrace.frames.map(_.source).to[Set]

      . assert(_ == Set(Unset))

      test(m"An unresolved frame displays its demangled names"):
        Exception("boom").stackTrace.frames.prim.let: frame =>
          (frame.displayClass, frame.displayMethod)

      . assert: value =>
          value.let((cls, method) => cls.starts(t"digression.ΞTests") && method.ends(t"()"))
          == true

    suite(m"Resolved frames"):
      val source = StackTrace.Frame.Source(t"/src/Foo.scala", t"pkg.Foo.bar", t"λ", Kind.Lambda)

      val frame =
        StackTrace.Frame
         ( StackTrace.Method(t"pkg.ΞFoo", t"bar.λ₁()"), t"Foo.scala", 12, false,
           t"pkg.Foo$$", t"bar$$anonfun$$1", source )

      test(m"A resolved frame displays the source definition"):
        (frame.displayClass, frame.displayMethod)

      . assert(_ == (t"pkg.Foo.bar", t"λ"))

      test(m"The displayed owner splits at its last segment"):
        (frame.displayPrefix, frame.displaySegment)

      . assert(_ == (t"pkg.Foo", t"bar"))

      test(m"A definition joins its owner and name"):
        source.definition

      . assert(_ == t"pkg.Foo.bar.λ")

      test(m"A definition with no owner is just its name"):
        source.copy(owner = t"").definition

      . assert(_ == t"λ")

      test(m"Compiler-generated frames are marked as plumbing"):
        List(Kind.Bridge, Kind.Forwarder, Kind.Initializer, Kind.Specialized, Kind.Synthetic)
        . map(_.plumbing)

      . assert(_.all(identity))

      test(m"Frames from source are not marked as plumbing"):
        List(Kind.Method, Kind.Lambda, Kind.Value, Kind.Class, Kind.Constructor, Kind.Extension)
        . map(_.plumbing)

      . assert(_.all(!_))
