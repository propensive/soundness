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
package ypsiloid


import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import denominative.dysasymptotics.linearSize

// The serializer, emitter and codec-mode suites, split from `Tests` so that neither object
// exceeds the JVM's constant-pool limit.
object SerializerTests extends Suite("yaml-serializer", m"Ypsiloid serializer tests"):
  def run(): Unit =
    suite(m"Serializer roundtrip"):
      import formatting.blockYamlFormatting

      // Encode a value to `Yaml`, render it with the printer, parse the text
      // back, and decode: the full encode → print → parse → decode loop.
      def roundtrip[value: {Encodable in Yaml, Decodable in Yaml}](value: value): value =
        value.in[Yaml].show.read[Yaml].as[value]

      // Print a parsed AST and re-parse it: print ∘ parse must be the identity
      // on the structural AST (compared by `deepEquals`).
      def astStable(source: Text): Boolean =
        val original = source.read[Yaml]
        Yaml.Ast.deepEquals(Yaml.unseal(original), Yaml.unseal(original.show.read[Yaml]))

      test(m"Roundtrip a positive integer"):
        roundtrip(42)
      . assert(_ == 42)

      test(m"Roundtrip a negative integer"):
        roundtrip(-99)
      . assert(_ == -99)

      test(m"Roundtrip a long"):
        roundtrip(1234567890123L)
      . assert(_ == 1234567890123L)

      test(m"Roundtrip a double"):
        roundtrip(3.1415926)
      . assert(_ == 3.1415926)

      test(m"Roundtrip positive infinity"):
        roundtrip(Double.PositiveInfinity)
      . assert(_ == Double.PositiveInfinity)

      test(m"Roundtrip negative infinity"):
        roundtrip(Double.NegativeInfinity)
      . assert(_ == Double.NegativeInfinity)

      test(m"Roundtrip NaN"):
        roundtrip(Double.NaN)
      . assert(_.isNaN)

      test(m"Roundtrip true"):
        roundtrip(true)
      . assert(identity)

      test(m"Roundtrip false"):
        roundtrip(false)
      . assert(!_)

      test(m"Roundtrip a simple string"):
        roundtrip(t"hello")
      . assert(_ == t"hello")

      test(m"Roundtrip a string with a space"):
        roundtrip(t"hello world")
      . assert(_ == t"hello world")

      test(m"Roundtrip a string that looks like an integer"):
        roundtrip(t"42")
      . assert(_ == t"42")

      test(m"Roundtrip a string that looks like a boolean"):
        roundtrip(t"true")
      . assert(_ == t"true")

      test(m"Roundtrip a string that looks like null"):
        roundtrip(t"null")
      . assert(_ == t"null")

      test(m"Roundtrip an empty string"):
        roundtrip(t"")
      . assert(_ == t"")

      test(m"Roundtrip a string with a colon"):
        roundtrip(t"key: value")
      . assert(_ == t"key: value")

      test(m"Roundtrip a string with a newline"):
        roundtrip(t"line1\nline2")
      . assert(_ == t"line1\nline2")

      test(m"Roundtrip a string with a quote and backslash"):
        roundtrip(t"a\"b\\c")
      . assert(_ == t"a\"b\\c")

      test(m"Roundtrip a string with leading indicator"):
        roundtrip(t"- not a list")
      . assert(_ == t"- not a list")

      test(m"Roundtrip a list of integers"):
        roundtrip[List[Int]](List(1, 2, 3))
      . assert(_ == List(1, 2, 3))

      test(m"Roundtrip a list of strings"):
        roundtrip[List[Text]](List(t"alice", t"bob"))
      . assert(_ == List(t"alice", t"bob"))

      test(m"Roundtrip an empty list"):
        roundtrip(List[Int]())
      . assert(_ == Nil)

      test(m"Roundtrip a map"):
        roundtrip(Map(t"a" -> 1, t"b" -> 2))
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

      test(m"Roundtrip a case class"):
        roundtrip(Person(t"Jon", 42))
      . assert(_ == Person(t"Jon", 42))

      test(m"Roundtrip a nested case class"):
        roundtrip(NamedOuter(t"a", Inner(7)))
      . assert(_ == NamedOuter(t"a", Inner(7)))

      test(m"Roundtrip a list of case classes"):
        roundtrip[List[Person]](List(Person(t"a", 1), Person(t"b", 2)))
      . assert(_ == List(Person(t"a", 1), Person(t"b", 2)))

      test(m"Roundtrip a list of lists"):
        roundtrip[List[List[Int]]](List(List(1, 2), List(3, 4)))
      . assert(_ == List(List(1, 2), List(3, 4)))

      test(m"AST stable: nested mapping"):
        astStable(t"outer:\n  inner: 1\n  other: two")
      . assert(identity)

      test(m"AST stable: sequence of mappings"):
        astStable(t"- a: 1\n  b: 2\n- a: 3\n  b: 4")
      . assert(identity)

      test(m"AST stable: empty flow mapping"):
        astStable(t"{}")
      . assert(identity)

      test(m"AST stable: empty flow sequence"):
        astStable(t"[]")
      . assert(identity)

    suite(m"Emitting a tree a block at a time"):
      import formatting.blockYamlFormatting

      // Eight-char blocks, so the document is split across many.
      given Buffering:
        def capacity(substrate: Substrate): Int = 8
        def depth: Int = 2

      val document =
        t"name: \"quoted \\\"text\\\"\"\nitems:\n  - 1\n  - nested:\n      deep: [1, 2]\n  - {}\n"
        . read[Yaml]

      test(m"the pulled blocks are the rendered text"):
        Yaml.emit(document).to[List].join
      . assert(_ == document.show)

      test(m"a tree is emitted in several blocks"):
        Yaml.emit(document).size
      . assert(_ > 1)

      test(m"the pushed blocks are the rendered text"):
        val blocks = scala.collection.mutable.ListBuffer[Text]()
        Yaml.emit(document, blocks += _)
        blocks.toList.join
      . assert(_ == document.show)

      test(m"a single scalar is emitted whole"):
        Yaml.emit(t"just text".read[Yaml]).to[List].join
      . assert(_ == t"just text".read[Yaml].show)

      test(m"a streamed value reads back"):
        import strategies.throwUnsafely
        val stream = summon[Yaml is Streamable by Text over Credit].stream(Person(t"Zoë", 30).in[Yaml])
        summon[Text is Aggregable by Text].accept(stream).read[Person in Yaml]
      . assert(_ == Person(t"Zoë", 30))

    suite(m"HTTP content-type integration"):
      import codepages.utf8Codepage
      import formatting.blockYamlFormatting

      test(m"serialises with an application/yaml media type"):
        Person(t"Jon", 42).in[Yaml].generic(0)
      . assert(_.starts(t"application/yaml"))

      test(m"request body parses back via Instantiable"):
        val instantiable = summon[Yaml is Instantiable across HttpRequests from Text]
        instantiable(t"name: Jon\nage: 42").as[Person]
      . assert(_ == Person(t"Jon", 42))

    suite(m"Codec modes"):
      import formatting.blockYamlFormatting

      val person = Person(t"Zoë \"q\"", 30)
      val outer = NamedOuter(t"n", Inner(7))
      val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))
      val renamed = YRenamed(t"Ada Lovelace", 1815)
      val present = WithOptional(t"Ada", 36)
      val absent = WithOptional(t"Bob", Unset)
      val option = WithOption(t"Cy", None)
      val shape: Shape = Shape.Circle(1.5)

      val team =
        YTeam(t"crew", List(Person(t"al", 1), Person(t"bo", 2)), Set(t"x"), Map(t"k" -> 1),
            Nil, Unset, Some(t"hi"))

      test(m"`codecs.direct` writes each record as `show` renders it"):
        import codecs.direct

        ( person.in[Text over Yaml], outer.in[Text over Yaml], tree.in[Text over Yaml],
          renamed.in[Text over Yaml], present.in[Text over Yaml], absent.in[Text over Yaml],
          option.in[Text over Yaml], team.in[Text over Yaml] )
      . assert(_ == (person.in[Yaml].show, outer.in[Yaml].show, tree.in[Yaml].show,
          renamed.in[Yaml].show, present.in[Yaml].show, absent.in[Yaml].show,
          option.in[Yaml].show, team.in[Yaml].show))

      test(m"`codecs.direct` reads each record back"):
        import codecs.direct

        ( person.in[Text over Yaml].read[Person in Yaml],
          outer.in[Text over Yaml].read[NamedOuter in Yaml],
          tree.in[Text over Yaml].read[Tree in Yaml],
          renamed.in[Text over Yaml].read[YRenamed in Yaml],
          absent.in[Text over Yaml].read[WithOptional in Yaml],
          team.in[Text over Yaml].read[YTeam in Yaml] )
      . assert(_ == (person, outer, tree, renamed, absent, team))

      test(m"`codecs.direct` reads flow style too"):
        import codecs.direct
        t"{name: Alice, age: 30}".read[Person in Yaml]
      . assert(_ == Person(t"Alice", 30))

      test(m"`codecs.direct` writes a record with no instance of its own directly"):
        import codecs.direct

        given innerSeam: (Inner is Encodable in Yaml) =
          _ => panic(m"a direct printer encoded a nested record through its Encodable")

        outer.in[Text over Yaml]
      . assert(_ == t"name: n\ninner:\n  n: 7\n")

      test(m"`codecs.direct` reads a record with no instance of its own directly"):
        import codecs.direct

        given innerSeam: (Inner is Decodable in Yaml) =
          _ => panic(m"a direct parser decoded a nested record through its Decodable")

        given outerSeam: (NamedOuter is Decodable in Yaml) =
          _ => panic(m"the read decoded the record through its Decodable")

        t"name: n\ninner:\n  n: 7\n".read[NamedOuter in Yaml]
      . assert(_ == outer)

      test(m"`codecs.direct` prints a generic record directly too"):
        import codecs.direct

        given innerSeam: (Boxed[Int] is Encodable in Yaml) =
          _ => panic(m"the write encoded the record through its Encodable")

        Boxed(7).in[Text over Yaml]
      . assert(_ == t"value: 7\n")

      test(m"`codecs.direct` leaves an enumeration on the tree path"):
        import codecs.direct
        import discriminables.yamlByTypeDiscriminable
        (shape.in[Text over Yaml], shape.in[Text over Yaml].read[Shape in Yaml])
      . assert: (text, back) =>
          import discriminables.yamlByTypeDiscriminable
          text == shape.in[Yaml].show && back == shape

      test(m"`codecs.direct` reads an absent optional as `Unset`, and a null too"):
        import codecs.direct
        ( t"name: Eve".read[WithOptional in Yaml],
          t"name: Eve\nage: null".read[WithOptional in Yaml] )
      . assert(_ == (WithOptional(t"Eve", Unset), WithOptional(t"Eve", Unset)))

      test(m"`codecs.direct` reads a strict null as an error"):
        import codecs.direct
        import optionalityOptions.strictYamlNulls
        capture[Yaml.Error](t"name: Eve\nage: null".read[WithOptional in Yaml]).reason
      . assert(_ == Yaml.Error.Reason.NotType(Yaml.Primitive.Null, Yaml.Primitive.Integer))

      test(m"`codecs.direct` reports a missing field as absent"):
        import codecs.direct
        capture[Yaml.Error](t"name: Eve".read[Person in Yaml]).reason
      . assert(_ == Yaml.Error.Reason.Absent)

      test(m"`codecs.direct` takes a declared default for a missing field"):
        import codecs.direct
        t"name: Eve".read[WithDefault in Yaml]
      . assert(_ == WithDefault(t"Eve", 18))

      test(m"`codecs.derived` encodes even where a direct printer is in scope"):
        import codecs.derived

        given (Person is Yaml.Printable) =
          Yaml.Printable.fromEncodable[Person](_ => panic(m"printed directly"))

        Person(t"Ada", 36).in[Text over Yaml]
      . assert(_ == t"name: Ada\nage: 36\n")

      test(m"`codecs.derived` decodes even where a direct parser is in scope"):
        import codecs.derived
        given (Person is Yaml.Parsable) = Yaml.Parsable.derived[Person]

        // A sentinel decoder: only the tree path can return it.
        given (Person is Decodable in Yaml) = _ => Person(t"derived", 0)

        t"name: Ada\nage: 36".read[Person in Yaml]
      . assert(_ == Person(t"derived", 0))

      test(m"`codecs.strict` writes and reads a record directly"):
        import codecs.strict

        given innerSeam: (Inner is Encodable in Yaml) =
          _ => panic(m"a direct printer encoded a nested record through its Encodable")

        outer.in[Text over Yaml].read[NamedOuter in Yaml]
      . assert(_ == outer)

      test(m"`derives Yaml.Direct` writes and reads without an import"):
        given (YDeclared is Decodable in Yaml) =
          _ => panic(m"the read decoded the record through its Decodable")

        YDeclared(5, t"five").in[Text over Yaml].read[YDeclared in Yaml]
      . assert(_ == YDeclared(5, t"five"))

      test(m"`derives Yaml.Direct` takes precedence over `codecs.direct`"):
        import codecs.direct
        summon[YDeclared is Yaml.Parsable] == summon[Yaml.Direct[YDeclared]].parsable
      . assert(_ == true)

      test(m"a printed record is written in blocks as it is printed"):
        given Buffering:
          def capacity(substrate: Substrate): Int = 8
          def depth: Int = 2

        given (Tree is Yaml.Printable) = Yaml.Printable.derived[Tree]
        val blocks = scala.collection.mutable.ListBuffer[Text]()
        Yaml.Printable.emit(tree, blocks += _)
        (blocks.length > 1, blocks.toList.join)
      . assert(_ == (true, tree.in[Yaml].show))
