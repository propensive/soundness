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

    val smap: Optional[Smap] =
      Smap.parse:
        Text:
          List
            ( "SMAP", "Main.scala", "Scala",
              "*S Scala",
              "*F", "+ 1 Main.scala", "Main.scala", "+ 2 Util.scala", "foo/Util.scala",
              "*L", "1#1,120:1", "3#2,2:121",
              "*S ScalaDebug",
              "*F", "+ 1 Main.scala", "Main.scala",
              "*L", "3#1:121", "3#1:122",
              "*E" )
          . mkString("\n")

    val nested: Optional[Smap] =
      Smap.parse:
        Text:
          List
            ( "SMAP", "Main.scala", "Scala",
              "*S Scala",
              "*F", "+ 1 Main.scala", "Main.scala", "+ 2 B.scala", "B.scala",
              "+ 3 A.scala", "A.scala",
              "*L", "1#1,10:1", "3#2:11", "3#3:12",
              "*S ScalaDebug",
              "*F", "+ 1 Main.scala", "Main.scala", "+ 2 B.scala", "B.scala",
              "*L", "3#1:11", "11#2:12",
              "*E" )
          . mkString("\n")

    suite(m"SMAP parsing and expansion"):
      test(m"Unparsable text is not an SMAP"):
        Smap.parse(t"not an SMAP")

      . assert(_ == Unset)

      test(m"A real line expands to nothing"):
        smap.let(_.expand(50))

      . assert(_ == Unset)

      test(m"A line beyond every mapping expands to nothing"):
        smap.let(_.expand(200))

      . assert(_ == Unset)

      test(m"A synthetic line recovers its origin and its call site"):
        smap.let(_.expand(121))

      . assert(_ == Smap.Expansion(List(Smap.Origin(t"Util.scala", t"foo/Util.scala", 3)), 3))

      test(m"A coalesced run maps each of its lines"):
        smap.let(_.expand(122))

      . assert(_ == Smap.Expansion(List(Smap.Origin(t"Util.scala", t"foo/Util.scala", 4)), 3))

      test(m"Nested inlining expands innermost first, back to a real line"):
        nested.let(_.expand(12))

      . assert:
          _ == Smap.Expansion
                 ( List
                     ( Smap.Origin(t"A.scala", t"A.scala", 3),
                       Smap.Origin(t"B.scala", t"B.scala", 3) ),
                   3 )

      test(m"A chain with no call-site information keeps its origins but no line"):
        val bare =
          List
            ( "SMAP", "Main.scala", "Scala",
              "*S Scala",
              "*F", "+ 1 Main.scala", "Main.scala", "+ 2 Util.scala", "Util.scala",
              "*L", "1#1,120:1", "3#2:121",
              "*E" )
          . mkString("\n")

        Smap.parse(Text(bare)).let(_.expand(121))

      . assert(_ == Smap.Expansion(List(Smap.Origin(t"Util.scala", t"Util.scala", 3)), Unset))

    suite(m"Rendering inlined frames"):
      val method = StackTrace.Method(t"Main", t"run()")

      val inlined =
        List
          ( StackTrace.Frame.Inlined(t"A.scala", t"A.scala", 3),
            StackTrace.Frame.Inlined(t"B.scala", t"B.scala", 3) )

      val frame = StackTrace.Frame(method, t"Main.scala", 3, false, inlined = inlined)
      val stack = StackTrace(t"scala", t"Exception", Message(t"boom"), List(frame), Unset)

      test(m"An inlined origin is rendered beneath its frame"):
        stack.show.contains(t"↳ inlined from A.scala:3")

      . assert(_ == true)

      test(m"Inlined origins are rendered innermost first"):
        val text = stack.show
        text.s.indexOf("A.scala:3") < text.s.indexOf("B.scala:3")

      . assert(_ == true)

      test(m"A frame with no inline information renders as before"):
        StackTrace(t"scala", t"Exception", Message(t"boom"),
            List(frame.copy(inlined = Nil)), Unset)
        . show.contains(t"inlined")

      . assert(_ == false)
