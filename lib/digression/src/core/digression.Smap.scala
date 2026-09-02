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

import scala.collection.immutable as sci

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Smap:
  // One file in a stratum's `*F` table: the name a stack trace records, and the full path, when
  // the SMAP's writer supplied the two-line form (otherwise the path repeats the name).
  case class File(name: Text, path: Text)

  // One `*L` entry: `repeat` input lines starting at `inputLine` of file `fileId` map to output
  // lines starting at `outputLine`, each input line covering `increment` output lines.
  case class Entry(inputLine: Int, fileId: Int, repeat: Int, outputLine: Int, increment: Int):
    // The input line an output line falls at, if this entry covers it.
    def apply(line: Int): Optional[Int] =
      if increment < 1 || repeat < 1 then Unset
      else if line < outputLine || line >= outputLine + repeat*increment then Unset
      else inputLine + (line - outputLine)/increment

  case class Stratum(files: Map[Int, File], entries: List[Entry]):
    // The (file id, input line) an output line maps to, from the first entry which covers it.
    def apply(line: Int): Optional[(Int, Int)] =
      @tailrec
      def recur(todo: List[Entry]): Optional[(Int, Int)] =
        todo match
          case head :: tail => head(line) match
            case input: Int => (head.fileId, input)
            case _          => recur(tail)

          case _ => Unset

      recur(entries)

    // The source position an output line maps to, when the file table knows the entry's file.
    def origin(line: Int): Optional[(File, Int)] =
      apply(line).let: (fileId, input) => files.at(fileId).let(file => (file, input))

  // Where a line of inlined code was written: the file's recorded name, its full path, the
  // 1-based line number, and — when the SMAP's writer emitted a `<default>Class` stratum, as the
  // Scala compiler does — the binary name of the top-level class whose compilation unit the
  // position lies in, which is what lets a resolver find the class's TASTy without guessing.
  case class Origin(file: Text, path: Text, line: Int, cls: Optional[Text] = Unset)

  // What a synthetic output line expands to: the chain of inline origins, innermost first, and
  // the real line of the frame's own file that the chain of calls started from—absent when the
  // SMAP's call-site information ran out before reaching one.
  case class Expansion(inlined: List[Origin], line: Optional[Int])

  // A number, or `Unset` for anything else: the parser's only failure mode for a field.
  private def number(text: String): Optional[Int] =
    try text.toInt catch case _: NumberFormatException => Unset

  // An `*L` line: `InputStartLine[#FileId][,RepeatCount]:OutputStartLine[,OutputLineIncrement]`,
  // where an omitted file id inherits the previous entry's.
  private def entry(line: String, lastFile: Int): Optional[Entry] =
    val colon = line.indexOf(':')

    if colon < 0 then Unset else
      val left = line.substring(0, colon).nn
      val right = line.substring(colon + 1).nn

      def split(text: String, char: Char, absent: String): (String, String) =
        text.indexOf(char.toInt) match
          case -1    => (text, absent)
          case index => (text.substring(0, index).nn, text.substring(index + 1).nn)

      val (input, fileAndRepeat) = split(left, '#', "")

      val (fileId, repeat) =
        if fileAndRepeat.isEmpty then (lastFile.toString, split(input, ',', "1")(1))
        else split(fileAndRepeat, ',', "1")

      val input2 = split(input, ',', "1")(0)
      val (output, increment) = split(right, ',', "1")

      number(input2).let: input =>
        number(fileId).let: fileId =>
          number(repeat).let: repeat =>
            number(output).let: output =>
              number(increment).let(Entry(input, fileId, repeat, output, _))

  // Parses the text of a JSR-45 `SourceDebugExtension` attribute. Anything malformed enough to
  // make the header unreadable is `Unset`; anything malformed within a section is skipped, since
  // an SMAP is only ever a source of extra detail.
  def parse(text: Text): Optional[Smap] =
    val lines: sci.Vector[String] = text.s.split("\r?\n").nn.iterator.map(_.nn).to(sci.Vector)

    if lines.length < 3 || lines(0) != "SMAP" then Unset else
      val generated = lines(1)
      val default = lines(2)
      var strata: Map[Text, Stratum] = Map()
      var stratum: String = ""
      var files: Map[Int, File] = Map()
      var entries: List[Entry] = Nil
      var section: Char = ' '
      var index = 3

      def close(): Unit =
        if stratum != "" then strata = strata.define(stratum.tt, Stratum(files, entries.reverse))
        files = Map()
        entries = Nil

      while index < lines.length do
        val line = lines(index)

        if line.startsWith("*S ") then
          close()
          stratum = line.substring(3).nn.trim.nn
          section = ' '
        else if line == "*F" || line == "*L" then
          section = line.charAt(1)
        else if line.startsWith("*") then
          section = ' '
        else if section == 'F' then
          // `+ id name` announces a path on the following line; `id name` stands alone.
          val plus = line.startsWith("+ ")
          val fields = (if plus then line.substring(2).nn else line).trim.nn.split(" ", 2).nn

          if fields.length == 2 then number(fields(0).nn).let: id =>
            val name = fields(1).nn.trim.nn.tt

            val path =
              if plus && index + 1 < lines.length then
                index += 1
                lines(index).trim.nn.tt
              else
                name

            files = files.define(id, File(name, path))
        else if section == 'L' then
          entry(line.trim.nn, entries.prim.lay(0)(_.fileId)).let: entry =>
            entries = entry :: entries

        index += 1

      close()
      Smap(generated.tt, default.tt, strata)

// A parsed JSR-45 SMAP: the mapping from the synthetic line numbers a classfile records to the
// source positions they stand for. The default stratum maps a line to where the code was written;
// a `<default>Debug` stratum, when the writer emitted one (as the Scala and Kotlin compilers do),
// maps it to its inline call site in output numbering, so that following it repeatedly peels one
// level of inlining at a time until it reaches a real line.
case class Smap(generated: Text, default: Text, strata: Map[Text, Smap.Stratum]):
  private lazy val stratum: Optional[Smap.Stratum] = strata.at(default)
  private lazy val debug: Optional[Smap.Stratum] = strata.at((default.s+"Debug").tt)
  private lazy val classes: Optional[Smap.Stratum] = strata.at((default.s+"Class").tt)

  // The file id the generated file itself appears under in the default stratum, against which
  // identity mappings are recognized.
  private lazy val generatedId: Optional[Int] =
    stratum.let: stratum =>
      stratum.files.seek { (_, file) => file.name == generated }.let(_(0))

  // The generated file's fuller path, when its file table records one.
  lazy val path: Optional[Text] =
    stratum.let: stratum =>
      stratum.files.seek { (_, file) => file.name == generated }.let(_(1).path)

  // A line which maps to itself in the generated file—or which the SMAP says nothing about—is a
  // real line, not a synthetic one.
  private def real(line: Int): Boolean =
    stratum.lay(true): stratum =>
      stratum(line).lay(true): (fileId, input) =>
        input == line && generatedId.lay(false)(_ == fileId)

  // The generated-line ranges standing for a source position: one `[start, end)` range per site
  // the line was inlined at, excluding the identity run (a line's natural occurrence in the
  // generated file itself, which the class's own line table already locates). Empty when the
  // file is foreign to this SMAP. One line inlined at many call sites yields many ranges — a
  // breakpoint on the line belongs at every one. This is `expand`'s inverse, for resolution:
  // `expand` asks what a generated line stands for; `sites` asks where a source line went.
  def sites(file: Text, line: Int): List[(Int, Int)] =
    stratum.lay(List()): stratum =>
      val ids = stratum.files.sweep { case (id, entry) if entry.name == file => id }.to[Set]

      stratum.entries.bind: entry =>
        val identity = generatedId.lay(false)(_ == entry.fileId)
        val covers = line >= entry.inputLine && line < entry.inputLine + entry.repeat
        val sound = entry.increment >= 1 && entry.repeat >= 1

        if !sound || identity || !ids.has(entry.fileId) || !covers then List() else
          val start = entry.outputLine + (line - entry.inputLine)*entry.increment
          List((start, start + entry.increment))

  // Expands a synthetic line into the inline origins it stands for, innermost first, and the
  // real call-site line the chain leads back to. A real line—or a line in a classfile whose SMAP
  // this is not—expands to nothing.
  def expand(line: Int): Optional[Smap.Expansion] =
    if real(line) then Unset else
      @tailrec
      def recur(line: Int, seen: Set[Int], origins: List[Smap.Origin]): Smap.Expansion =
        if real(line) then Smap.Expansion(origins.reverse, line)
        else
          val origins2 = stratum.let(_.origin(line)).lay(origins):
            case (file, input) =>
              val cls = classes.let(_.origin(line)).let: (clsFile, _) => clsFile.name
              Smap.Origin(file.name, file.path, input, cls) :: origins

          debug.let(_(line)) match
            case (_, next: Int) if !seen.has(next) => recur(next, seen :+ next, origins2)

            case _ =>
              Smap.Expansion(origins2.reverse, Unset)

      recur(line, Set(line), List()) match
        case expansion if expansion.inlined.nil => Unset
        case expansion                          => expansion
