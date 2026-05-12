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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package decorum

import scala.collection.mutable

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

case class ImportInfo
  ( startLine:        Int,
    endLine:          Int,
    path:             String,
    hasTopLevelAlias: Boolean )

object Imports:
  // Extract every top-level `import` (or `export`) statement from the
  // package body. Imports inside templates / blocks are intentionally
  // ignored — Decorum's R9 only governs file-top imports.
  def extract(tree: untpd.Tree, source: SourceFile): List[ImportInfo] =
    val out = mutable.ListBuffer[ImportInfo]()
    val content = String(source.content)

    def visitTopLevel(t: untpd.Tree): Unit = t match
      case pkg: untpd.PackageDef =>
        pkg.stats.foreach(visitTopLevel)

      case imp: untpd.Import =>
        recordImport(imp, content, source, out)

      case _ =>
        ()

    visitTopLevel(tree)
    out.toList

  private def recordImport
    ( imp:     untpd.Import,
      content: String,
      source:  SourceFile,
      out:     mutable.ListBuffer[ImportInfo] )
  :   Unit =

    val span = imp.span
    if !span.exists then ()
    else
      val start = span.start.max(0).min(content.length)
      val end   = span.end.max(start).min(content.length)
      val text  = content.substring(start, end).nn
      val (path, hasAlias) = analyse(text)
      out += ImportInfo
        ( startLine        = source.offsetToLine(start) + 1,
          endLine          = source.offsetToLine((end - 1).max(start)) + 1,
          path             = path,
          hasTopLevelAlias = hasAlias )

  // Reduce an import statement's source text to:
  //   - the path string (concatenation of the path tokens and any selector
  //     braces, with whitespace stripped, stopping at a top-level `as`); and
  //   - whether the statement contains a top-level rename (`as` or `=>`
  //     outside any `{…}`).
  // This mirrors the rules the original token-based checker applied; using
  // a span-bounded substring keeps its semantics intact.
  private def analyse(text: String): (String, Boolean) =
    val keyword = stripKeyword(text)
    val sb       = StringBuilder()
    var depth    = 0
    var hasAlias = false
    var i        = 0
    var done     = false
    val n        = keyword.length
    while i < n do
      val ch = keyword.charAt(i)
      ch match
        case '{' => depth += 1; if !done then sb.append(ch); i += 1
        case '}' => depth -= 1; if !done then sb.append(ch); i += 1

        case '/' if i + 1 < n && keyword.charAt(i + 1) == '/' =>
          while i < n && keyword.charAt(i) != '\n' do i += 1

        case '/' if i + 1 < n && keyword.charAt(i + 1) == '*' =>
          i += 2
          while i + 1 < n && !(keyword.charAt(i) == '*' && keyword.charAt(i + 1) == '/') do i += 1
          if i + 1 < n then i += 2

        case c if c.isWhitespace =>
          // Look ahead for top-level `as` keyword.
          if
            depth == 0 && i + 3 <= n && keyword.regionMatches(i + 1, "as", 0, 2)
            && (i + 3 == n || !isWordChar(keyword.charAt(i + 3)))
          then
            hasAlias = true
            done = true
            i += 3
          else
            i += 1

        case '=' if depth == 0 && i + 1 < n && keyword.charAt(i + 1) == '>' =>
          hasAlias = true
          done = true
          i += 2

        case _ =>
          if !done then sb.append(ch)
          i += 1
    (sb.toString, hasAlias)

  // Skip leading whitespace and the `import` (or `export`) keyword from
  // `text` (the source slice of the import statement). Returns the text
  // following the keyword.
  private def stripKeyword(text: String): String =
    var i = 0
    while i < text.length && text.charAt(i).isWhitespace do i += 1
    val start = i
    while i < text.length && isWordChar(text.charAt(i)) do i += 1
    text.substring(i).nn

  private def isWordChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'
