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
package delicious

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.IteratorHasAsScala

import soundness.*

import galilei.Linux.pathOnLinux
import logging.silentLogging
import probates.cancelProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading
import workingDirectories.javaWorkingDirectory

object Tests extends Suite(m"Delicious Tests"):
  given palette: ScalaSyntaxPalette = new Palette:
    type Form = Srgb
    def background: Color in Srgb       = WebColors.Black
    def foreground: Color in Srgb       = WebColors.White
    def scalaError: Color in Srgb       = WebColors.Red
    def scalaNumber: Color in Srgb      = WebColors.Gold
    def scalaString: Color in Srgb      = WebColors.LimeGreen
    def scalaTerm: Color in Srgb        = WebColors.Silver
    def scalaType: Color in Srgb        = WebColors.Turquoise
    def scalaKeyword: Color in Srgb     = WebColors.Orange
    def scalaSymbol: Color in Srgb      = WebColors.Gray
    def scalaParenthesis: Color in Srgb = WebColors.Gray
    def scalaModifier: Color in Srgb    = WebColors.Orchid
    def scalaComment: Color in Srgb     = WebColors.DimGray
    def subdued: Color in Srgb          = WebColors.DimGray
    def accented: Color in Srgb         = WebColors.White
    def margin: Color in Srgb           = WebColors.DimGray

  val Esc: Char      = '\u001B'
  val Start: Char    = '\uE000'
  val End: Char      = '\uE001'
  val AttrsSep: Char = '\uE002'
  val TextSep: Char  = '\uE003'
  val AttrSep: Char  = '\u001F'

  def mark(kind: Text, attrs: List[(Text, Text)], text: Text): Text =
    val attributes = attrs.map { (key, value) => t"$key=$value" }.join(t"${AttrSep}")
    t"${Start}$kind${AttrsSep}$attributes${TextSep}$text${End}"

  def run(): Unit =
    test(m"Unmarked text parses as a single text node"):
      Markup.parse(t"type mismatch")
    . assert(_ == List(Markup.Textual(t"type mismatch")))

    test(m"A sym marker parses with its attributes"):
      Markup.parse(t"method ${mark(t"sym", List(t"name" -> t"foo", t"full" -> t"example.foo"), t"foo")} here")
    . assert(_ == List
        ( Markup.Textual(t"method "),
          Markup.Symbolic(t"foo", t"example.foo", Style.Default, List(Markup.Textual(t"foo"))),
          Markup.Textual(t" here") ))

    test(m"A name marker records whether it is a type name"):
      Markup.parse(mark(t"name", List(t"isType" -> t"true"), t"Elem"))
    . assert(_ == List(Markup.Named(true, Style.Default, List(Markup.Textual(t"Elem")))))

    test(m"An unknown marker kind falls back to a spanned node"):
      Markup.parse(mark(t"mystery", Nil, t"???"))
    . assert(_ == List(Markup.Spanned(t"mystery", Style.Default, List(Markup.Textual(t"???")))))

    test(m"Styles decode from their wire names"):
      Markup.parse(mark(t"sym", List(t"name" -> t"foo", t"style" -> t"dcl"), t"def foo: Int"))
    . assert(_ == List
        ( Markup.Symbolic(t"foo", t"", Style.Declaration, List(Markup.Textual(t"def foo: Int"))) ))

    test(m"Markers nest, and children keep their order"):
      val inner = mark(t"sym", List(t"name" -> t"foo"), t"foo")
      Markup.parse(mark(t"name", Nil, t"method $inner"))
    . assert(_ == List
        ( Markup.Named
            ( false,
              Style.Default,
              List
                ( Markup.Textual(t"method "),
                  Markup.Symbolic(t"foo", t"", Style.Default, List(Markup.Textual(t"foo"))) )) ))

    test(m"Percent-encoded attribute values decode"):
      Markup.parse(mark(t"sym", List(t"name" -> t"%003ainit%003a"), t"constructor"))
    . assert(_ == List
        ( Markup.Symbolic(t":init:", t"", Style.Default, List(Markup.Textual(t"constructor"))) ))

    test(m"A stray start marker is dropped"):
      Markup.parse(t"broken ${Start} text")
    . assert(_ == List(Markup.Textual(t"broken  text")))

    test(m"A stray end marker is dropped"):
      Markup.parse(t"broken ${End} text")
    . assert(_ == List(Markup.Textual(t"broken  text")))

    test(m"An unterminated marker splices its children into the parent"):
      Markup.parse(t"a ${Start}sym${AttrsSep}name=foo${TextSep}bar")
    . assert(_ == List(Markup.Textual(t"a "), Markup.Textual(t"bar")))

    test(m"A truncated header is treated as text"):
      Markup.parse(t"a ${Start}sym no header end")
    . assert(_ == List(Markup.Textual(t"a sym no header end")))

    test(m"Plain text strips all markers"):
      val inner = mark(t"sym", List(t"name" -> t"foo"), t"foo")
      Markup.plain(t"method ${mark(t"name", Nil, t"call of $inner")} failed")
    . assert(_ == t"method call of foo failed")

    test(m"A type marker carries its TASTy payload and placeholders"):
      val placeholder = t"0|local-type|Foo|1||Foo[Int]"
      Markup.parse(mark(t"type", List(t"tasty" -> t"QUJD", t"p" -> placeholder), t"Foo[Int]"))
    . assert(_ == List
        ( Markup.Typed
            ( t"QUJD",
              List(Placeholder(0, PlaceholderKind.LocalType, t"Foo", 1, Unset, t"Foo[Int]")),
              Style.Default,
              List(Markup.Textual(t"Foo[Int]")) ) ))

    test(m"A placeholder decodes its six fields"):
      Placeholder.decode(t"3|skolem|x|0|Test.scala:5|x.type")
    . assert(_ == Placeholder(3, PlaceholderKind.Skolem, t"x", 0, t"Test.scala:5", t"x.type"))

    test(m"A placeholder with an escaped pipe decodes"):
      Placeholder.decode(t"1|error|a%007cb|0||a|b")
    . assert(_ == Unset)

    test(m"An escaped pipe within a field decodes to a literal pipe"):
      Placeholder.decode(t"1|error|a%007cb|0||printed")
    . assert(_ == Placeholder(1, PlaceholderKind.Error, t"a|b", 0, Unset, t"printed"))

    test(m"A non-numeric placeholder id is rejected"):
      Placeholder.decode(t"x|error|a|0||printed")
    . assert(_ == Unset)

    test(m"A placeholder with too few fields is rejected"):
      Placeholder.decode(t"1|error|a|0|")
    . assert(_ == Unset)

    test(m"An unknown placeholder kind is preserved"):
      Placeholder.decode(t"1|quantum|a|0||printed")
    . assert(_ == Placeholder(1, PlaceholderKind.Other(t"quantum"), t"a", 0, Unset, t"printed"))

    test(m"A placeholder reference is recognized"):
      Placeholder.reference(t"⟨scala-diag:42⟩")
    . assert(_ == 42)

    test(m"A non-placeholder literal is not a reference"):
      Placeholder.reference(t"⟨scala-diag:esc:7⟩")
    . assert(_ == Unset)

    test(m"An escaped literal unescapes"):
      Placeholder.escaped(t"⟨scala-diag:esc:real text⟩")
    . assert(_ == t"real text")

    test(m"A semantic message finds nested type markers"):
      val typed = mark(t"type", List(t"tasty" -> t"QUJD"), t"List[Int]")
      SemanticMessage.parse(t"Found: ${mark(t"name", Nil, t"value of $typed")}").types.length
    . assert(_ == 1)

    test(m"A message without markers is not marked"):
      SemanticMessage.marked(t"ordinary message")
    . assert(_ == false)

    test(m"A message with markers is marked"):
      SemanticMessage.marked(mark(t"sym", Nil, t"foo"))
    . assert(_ == true)

    test(m"Substitution replaces a placeholder sentinel"):
      val placeholder =
        Placeholder(0, PlaceholderKind.LocalType, t"Local", 0, Unset, t"Bad.Local")

      val list = Syntax.Simple(Typename(t"scala.collection.immutable.List"))
      val syntax =
        Syntax.Application(list, List(Syntax.Primitive(t"\"⟨scala-diag:0⟩\"")), false)

      Reifier.substitute(syntax, List(placeholder))
    . assert(_ == Syntax.Application
        ( Syntax.Simple(Typename(t"scala.collection.immutable.List")),
          List(Syntax.Symbolic(t"Bad.Local")),
          false ))

    test(m"Substitution unescapes an escaped genuine literal"):
      Reifier.substitute(Syntax.Primitive(t"\"⟨scala-diag:esc:x⟩\""), Nil)
    . assert(_ == Syntax.Primitive(t"\"x\""))

    test(m"Substitution leaves other primitives alone"):
      Reifier.substitute(Syntax.Primitive(t"42"), Nil)
    . assert(_ == Syntax.Primitive(t"42"))

    // Payloads captured from a semdiag compiler run over:
    //   object Bad:
    //     class Local
    //     val xs: List[String] = List(1.5)
    //     def f: Option[Int] = List(new Local)
    // TASTy is version-locked (28.9-1), so these decode under any 3.9-stream compiler.
    val stringPayload: Text =
      t"XKGrH5yJgZpTY2FsYSAzLjkuMC1SQzEtcHJvcGVuc2l2ZQAadmNlJOIHAAAAAAAAAACeAYRBU1RzAYZTdHJpbmcBhGphdmEBhGxhbmcCgoKDgIR1gUCE"

    val placeholderPayload: Text =
      t"XKGrH5yJgZpTY2FsYSAzLjkuMC1SQzEtcHJvcGVuc2l2ZQDQKNr7HmsuAAAAAAAAAADGAYRBU1RzAYRMaXN0AYVzY2FsYQGKY29sbGVjdGlvbgKCgoMBiWltbXV0YWJsZQKChIUBkuKfqHNjYWxhLWRpYWc6MOKfqYCIoYZ1gUCGSoc="

    proscalaLibrary().let: lib =>
      val jars = List("scala-library.jar", "scala3-library.jar").map(lib.resolve(_).nn)
      val classpath = LocalClasspath(jars.map { jar => ClasspathEntry.Jar(jar.toString.tt) }*)
      val reifier = Reifier(classpath)
      given Imports = Imports.empty

      test(m"A pickled type payload reifies to a stenography rendering"):
        reifier.syntax(Markup.Typed(stringPayload, Nil, Style.Default, Nil)).let(_.text)
      . assert(_ == t"java.lang.String")

      test(m"A placeholder payload reifies with its placeholder substituted"):
        val placeholder =
          Placeholder(0, PlaceholderKind.LocalType, t"Local", 0, t"Bad.scala:2", t"Bad.Local")

        reifier.syntax(Markup.Typed(placeholderPayload, List(placeholder), Style.Default, Nil))
        . let(_.text)
      . assert(_ == t"scala.collection.immutable.List[Bad.Local]")

      test(m"An unpicklable payload degrades to Unset"):
        reifier.syntax(Markup.Typed(t"bm90IHRhc3R5", Nil, Style.Default, Nil))
      . assert(_ == Unset)

      test(m"A marked message renders types through stenography"):
        val typed = mark(t"type", List(t"tasty" -> stringPayload), t"printed")
        SemanticMessage.parse(t"Required: $typed").render(reifier)
      . assert(_ == t"Required: java.lang.String")

      test(m"A styled rendering preserves the visible text"):
        val code = mark(t"code", Nil, t"List(1.5)")
        val typed = mark(t"type", List(t"tasty" -> stringPayload), t"printed")
        SemanticMessage.parse(t"Tree: $code has type $typed").teletype(reifier).plain
      . assert(_ == t"Tree: List(1.5) has type java.lang.String")

      test(m"A code sample is syntax-highlighted, not plain"):
        val code = mark(t"code", Nil, t"val x = 42")
        SemanticMessage.parse(t"code: $code").teletype(reifier)
      . assert(_ != e"code: val x = 42")

      test(m"Compiler styling is stripped before highlighting"):
        val code = mark(t"code", Nil, t"${Esc}[33m1.5d${Esc}[0m")
        SemanticMessage.parse(t"Tree: $code").teletype(reifier).plain
      . assert(_ == t"Tree: 1.5d")

      // End-to-end through the embedded compiler. Feature-detecting: a compiler
      // without semdiag (releases up to p5) produces no markup, and only the
      // fallback invariants are asserted.
      supervise:
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val source: Text =
          t"""|object Bad:
              |  class Local
              |  val xs: List[String] = List(new Local)
              |""".s.stripMargin.tt

        val process =
          Scalac[3.9](List(scalacOptions.semanticDiagnostics))
            (classpath)(Map(t"bad.scala" -> source), out)

        process.complete()
        val notices = process.notices.to(List)

        test(m"A failed compilation produces at least one error notice"):
          notices.count(_.importance == Importance.Error)
        . assert(_ > 0)

        val marked = notices.filter(_.markup.present)

        if marked.isEmpty then
          test(m"Without semdiag support, messages are plain and unmarked"):
            notices.forall { notice => !SemanticMessage.marked(notice.message) }
          . assert(_ == true)
        else
          test(m"Semantic notices strip markers from the plain message"):
            marked.forall { notice => !SemanticMessage.marked(notice.message) }
          . assert(_ == true)

          test(m"A semantic notice renders its types through stenography"):
            marked.map { notice => notice.semantic.let(_.render(reifier)).or(t"") }
            . join(t"\n")
          . assert { rendered =>
              // `java.lang.String` (not the compiler-printed `String`) proves the type
              // came through stenography; `Bad.Local` proves placeholder substitution.
              rendered.contains(t"java.lang.String") && rendered.contains(t"Bad.Local")
            }

  def proscalaLibrary(): Optional[java.nio.file.Path] =
    val home = java.lang.System.getProperty("user.home").nn
    val root = Paths.get(home, ".cache", "soundness", "proscala").nn

    if !Files.isDirectory(root) then Unset else
      Files.list(root).nn.iterator.nn.asScala.to(List).sortBy(_.toString).reverse
      . map(_.resolve("lib").nn)
      . find { lib => Files.isDirectory(lib) && Files.exists(lib.resolve("scala3-library.jar")) }
      . getOrElse(Unset)
