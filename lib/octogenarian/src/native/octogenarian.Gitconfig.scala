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
package octogenarian

import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import sortingAlgorithms.timsort
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.overwritePreexisting
import filesystemOptions.moveAtomically
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.deleteRecursively

// Git's configuration dialect: `[section]` and `[section "subsection"]` headers, `key = value`
// entries (a bare key meaning `true`), `#` and `;` comments, double-quoted values with escapes, and
// backslash continuation. A file is kept as its lines, so that a write changes only the line it
// concerns and preserves everything else as written.
object Gitconfig:
  enum Line:
    case Section(name: Text, subsection: Optional[Text], raw: Text)
    case Entry(key: Git.Config.Key, value: Text, raw: Text)
    case Other(raw: Text)

    def raw: Text

  def parse(text: Text): List[Line] =
    def recur(lines: List[Text], section: Optional[(Text, Optional[Text])], acc: List[Line]): List[Line] =
      lines match
        case line :: rest =>
          val trimmed = line.trim

          if trimmed.starts(t"[") then
            val header = parseHeader(trimmed)
            recur(rest, header, Line.Section(header.lay(t"")(_(0)), header.let(_(1)), line) :: acc)
          else if trimmed.length == 0 || trimmed.starts(t"#") || trimmed.starts(t";") then
            recur(rest, section, Line.Other(line) :: acc)
          else
            // A value ending in a backslash continues on the next line.
            val (whole, remaining) = continuation(line, rest)

            section.lay(recur(remaining, section, Line.Other(whole) :: acc)): (name, subsection) =>
              val (key, value) = parseEntry(whole.trim)
              val entry = Line.Entry(Git.Config.Key(name, subsection, key.lower), value, whole)
              recur(remaining, section, entry :: acc)

        case _ => acc.reverse

    recur(text.cut(t"\n"), Unset, Nil)

  private def continuation(line: Text, rest: List[Text]): (Text, List[Text]) =
    if !line.ends(t"\\") then (line, rest) else rest match
      case next :: remaining => continuation(t"${line.skip(1, Rtl)}\n$next", remaining)
      case _                 => (line, Nil)

  private def parseHeader(line: Text): Optional[(Text, Optional[Text])] =
    val close = line.s.indexOf(']')
    if close < 0 then Unset else
      val inner = line.keep(close).skip(1).trim
      val quote = inner.s.indexOf('"')

      if quote < 0 then
        // The deprecated `[section.subsection]` form keeps the subsection's case.
        val dot = inner.s.indexOf('.')
        if dot < 0 then (inner.lower, Unset) else (inner.keep(dot).lower, inner.skip(dot + 1))
      else
        val name = inner.keep(quote).trim.lower
        val subsection = unquote(inner.skip(quote))
        (name, subsection)

  private def parseEntry(text: Text): (Text, Text) =
    val equals = text.s.indexOf('=')
    if equals < 0 then (stripComment(text).trim, t"true")
    else (text.keep(equals).trim, unquoteValue(text.skip(equals + 1).trim))

  private def stripComment(text: Text): Text =
    val hash = text.s.indexOf('#')
    val semicolon = text.s.indexOf(';')
    val cut = if hash < 0 then semicolon else if semicolon < 0 then hash else hash.min(semicolon)
    if cut < 0 then text else text.keep(cut)

  // Removes the outer quotes of a quoted value and resolves its escapes; an unquoted value is
  // trimmed and loses any trailing comment.
  private def unquoteValue(text: Text): Text =
    val builder = StringBuilder()
    val string = text.s

    def recur(index: Int, quoted: Boolean): Unit = if index < string.length then
      string.charAt(index) match
        case '"'  => recur(index + 1, !quoted)
        case '\\' if index + 1 < string.length =>
          string.charAt(index + 1) match
            case 'n'   => builder.append('\n')
            case 't'   => builder.append('\t')
            case 'b'   => builder.append('\b')
            case other => builder.append(other)
          recur(index + 2, quoted)
        case ('#' | ';') if !quoted => ()
        case other =>
          builder.append(other)
          recur(index + 1, quoted)

    recur(0, false)
    builder.toString.tt.trim

  private def unquote(text: Text): Text = unquoteValue(text)

  private def quote(value: Text): Text =
    val needs = value.contains(t"#") || value.contains(t";") || value.starts(t" ") || value.ends(t" ")
    val escaped = value.s.replace("\\", "\\\\").nn.replace("\"", "\\\"").nn.tt
    if needs || value.contains(t"\"") || value.contains(t"\\") then t"\"$escaped\"" else value

  def entries(lines: List[Line]): List[(Git.Config.Key, Text)] =
    lines.sweep { case Line.Entry(key, value, _) => (key, value) }

  private def header(key: Git.Config.Key): Text =
    key.subsection.lay(t"[${key.section}]") { subsection => t"[${key.section} \"$subsection\"]" }

  private def sameSection(line: Line, key: Git.Config.Key): Boolean = line match
    case Line.Section(name, subsection, _) => name == key.section && subsection == key.subsection
    case _                                 => false

  // Sets `key` to `value`, replacing its last occurrence (or every occurrence, when `replaceAll`),
  // or adds it: at the end of its section if the section exists, else in a new section at the end.
  def set(lines: List[Line], key: Git.Config.Key, value: Text, replaceAll: Boolean): List[Line] =
    val entry = Line.Entry(key, value, t"\t${key.name} = ${quote(value)}")
    val occurrences = lines.filter { case Line.Entry(k, _, _) => k == key; case _ => false }

    if occurrences != Nil && !replaceAll then
      val last = occurrences.reverse.prim
      lines.map { line => if Optional(line) == last then entry else line }
    else if occurrences != Nil then
      lines.filter { case Line.Entry(k, _, _) => k != key; case _ => true }.pipe(add(_, key, value))
    else add(lines, key, value)

  // Adds an entry, keeping any existing ones.
  def add(lines: List[Line], key: Git.Config.Key, value: Text): List[Line] =
    val entry = Line.Entry(key, value, t"\t${key.name} = ${quote(value)}")

    def recur(remaining: List[Line], inSection: Boolean, acc: List[Line]): List[Line] = remaining match
      case line :: rest =>
        val starts = sameSection(line, key)

        if inSection && line.isInstanceOf[Line.Section] then
          recur(rest, false, line :: entry :: acc)
        else recur(rest, inSection || starts, line :: acc)

      case _ =>
        if inSection then (entry :: acc).reverse
        else (entry :: Line.Section(key.section, key.subsection, header(key)) :: acc).reverse

    recur(lines, false, Nil)

  def unset(lines: List[Line], key: Git.Config.Key, all: Boolean): List[Line] =
    if all then lines.filter { case Line.Entry(k, _, _) => k != key; case _ => true }
    else
      val last = lines.filter { case Line.Entry(k, _, _) => k == key; case _ => false }.reverse.prim
      lines.filter { line => Optional(line) != last }

  def render(lines: List[Line]): Text = lines.map(_.raw).join(t"", t"\n", t"\n")

  def readFile(file: Path on Linux)(using Tactic[Io.Error]): List[Line] =
    if file.existent() then parse(Objects.text(file.read[Data])) else Nil

  // Writes the file through a lock, as git does, so that two writers never interleave.
  def writeFile(file: Path on Linux, lines: List[Line])(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val parent: Path on Linux = file.parent.or(abort(Git.Error(Git.Error.Reason.Unexpected)))
    Refstore.ensureDirectory(parent)
    val lockFile: Path on Linux = unsafely(t"${file.encode}.lock".as[Path on Linux])

    try lockFile.open[File](Write, OpenFlag.Create, OpenFlag.Exclusive) { handle ?=> () }
    catch case error: Io.Error => abort(Git.Error(Git.Error.Reason.Locked(file.encode)))

    try
      lockFile.write(Objects.bytes(render(lines)))
      lockFile.moveTo(file)
    finally if lockFile.existent() then lockFile.delete()

  // The files git consults, from least to most specific: system, global, local, worktree.
  def files(gitDir: Path on Linux, commonDir: Path on Linux)(using Tactic[Io.Error]): List[Path on Linux] =
    val home: Optional[Text] = Optional(System.getProperty("user.home")).let(_.tt)
    val xdgHome: Optional[Text] = Optional(System.getenv("XDG_CONFIG_HOME")).let(_.tt)

    val xdgFile: Optional[Path on Linux] = xdgHome match
      case directory: Text => path(t"$directory/git/config")
      case _ => home match
        case directory: Text => path(t"$directory/.config/git/config")
        case _               => Unset

    val globalFile: Optional[Path on Linux] = home match
      case directory: Text => path(t"$directory/.gitconfig")
      case _               => Unset

    val local = commonDir / "config"
    val worktree = gitDir / "config.worktree"
    val worktreeKey = Git.Config.Key(t"extensions", Unset, t"worktreeconfig")
    val enabled = t"true"

    val worktreeEnabled =
      entries(readFile(local)).exists: (key, value) =>
        key == worktreeKey && value.lower == enabled

    List(path(t"/etc/gitconfig")) + xdgFile.let(List(_)).or(Nil) + globalFile.let(List(_)).or(Nil) +
      List(local) + (if worktreeEnabled then List(worktree) else Nil)

  private def path(text: Text): Path on Linux = unsafely(text.as[Path on Linux])

  // Every entry in force, in the order git lists them, with `include.path` files spliced in.
  def effective(gitDir: Path on Linux, commonDir: Path on Linux)(using Tactic[Io.Error]): List[(Git.Config.Key, Text)] =
    files(gitDir, commonDir).bind(withIncludes(_, 0))

  private val includeKey: Git.Config.Key = Git.Config.Key(t"include", Unset, t"path")

  def withIncludes(file: Path on Linux, depth: Int)(using Tactic[Io.Error]): List[(Git.Config.Key, Text)] =
    if depth > 10 || !file.existent() then Nil else
      def recur(remaining: List[(Git.Config.Key, Text)], acc: List[(Git.Config.Key, Text)])
      :   List[(Git.Config.Key, Text)] =

        remaining match
          case (`includeKey`, included) :: rest =>
            val target =
              if included.starts(t"/") then included
              else if included.starts(t"~/") then t"${System.getProperty("user.home").nn}${included.skip(1)}"
              else t"${file.parent.lay(t"")(_.encode)}/$included"

            recur(rest, acc + withIncludes(path(target), depth + 1))

          case entry :: rest => recur(rest, acc + List(entry))
          case _             => acc

      recur(entries(readFile(file)), Nil)
