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
import contingency.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks

// `.gitignore` semantics: a pattern with a slash (other than a trailing one) is anchored to the
// directory of the file it came from, a pattern without one matches a path's final component
// anywhere beneath; a trailing slash matches directories only; `!` negates; the last matching
// pattern wins; and an excluded directory excludes everything within it.
private[octogenarian] object Ignore:
  case class Rule
    ( regex:         java.util.regex.Pattern,
      negated:       Boolean,
      directoryOnly: Boolean,
      anchored:      Boolean,
      base:          Text )

  // A set of rules, from the repository's `info/exclude`, its root `.gitignore`, and the
  // `.gitignore` of every directory, each applying beneath its own directory.
  case class Rules(rules: List[Rule]):
    // Whether `path` (worktree-relative, no leading slash) is ignored; `directory` says whether it
    // names a directory.
    def ignored(path: Text, directory: Boolean): Boolean =
      def parentIgnored: Boolean =
        val slash = path.s.lastIndexOf('/')
        slash > 0 && ignored(path.keep(slash), true)

      def decide
        ( remaining: List[Rule], verdict: Optional[Boolean] )
      :   Optional[Boolean] =

        remaining match
          case rule :: rest =>
            val applies =
              (rule.base.length == 0 || path.starts(t"${rule.base}/")) &&
                (!rule.directoryOnly || directory) &&
                Ignore.matches(rule, path)

            decide(rest, if applies then !rule.negated else verdict)

          case _ => verdict

      decide(rules, Unset).or(parentIgnored)

  private def matches(rule: Rule, path: Text): Boolean =
    val relative = if rule.base.length == 0 then path else path.skip(rule.base.length + 1)
    val subject = if rule.anchored then relative else relative.skip(relative.s.lastIndexOf('/') + 1)
    rule.regex.matcher(subject.s).nn.matches

  // Translates a gitignore glob to a regular expression over a slash-separated path.
  def regex(glob: Text): java.util.regex.Pattern =
    val string = glob.s
    val builder = StringBuilder()

    def recur(index: Int): Unit = if index < string.length then
      val char = string.charAt(index)

      if string.startsWith("**/", index) then
        builder.append("(?:.*/)?")
        recur(index + 3)
      else if string.startsWith("/**", index) && index + 3 == string.length then
        builder.append("/.*")
        recur(index + 3)
      else if string.startsWith("**", index) then
        builder.append(".*")
        recur(index + 2)
      else
        char match
          case '*' => builder.append("[^/]*")
          case '?' => builder.append("[^/]")

          case '[' =>
            val close = string.indexOf(']', index + 1)

            if close < 0 then builder.append("\\[") else
              val body = string.substring(index + 1, close).nn
              val negated = body.startsWith("!")
              builder.append('[')
              if negated then builder.append('^')
              builder.append((if negated then body.substring(1).nn else body).replace("\\", "\\\\"))
              builder.append(']')
              return recur(close + 1)

          case '\\' if index + 1 < string.length =>
            builder.append(java.util.regex.Pattern.quote(string.charAt(index + 1).toString))
            return recur(index + 2)

          case other if "\\.^$+{}()|".indexOf(other) >= 0 => builder.append('\\').append(other)

          case other                                      => builder.append(other)

        recur(index + 1)

    recur(0)
    java.util.regex.Pattern.compile(builder.toString).nn

  def parse(text: Text, base: Text): List[Rule] =
    text.cut(t"\n").bind: line =>
      val trimmed = if line.ends(t"\\ ") then line else line.s.stripTrailing.nn.tt

      if trimmed.length == 0 || trimmed.starts(t"#") then Nil else
        val negated = trimmed.starts(t"!")
        val body0 = if negated then trimmed.skip(1) else trimmed
        val body1 = if body0.starts(t"\\!") || body0.starts(t"\\#") then body0.skip(1) else body0
        val directoryOnly = body1.ends(t"/")
        val body2 = if directoryOnly then body1.skip(1, Rtl) else body1
        val anchored = body2.contains(t"/")
        val body = if body2.starts(t"/") then body2.skip(1) else body2
        List(Rule(regex(body), negated, directoryOnly, anchored, base))

  // Every rule in force for a worktree: `info/exclude`, then each `.gitignore` found, deeper
  // files later so that they take precedence.
  def load(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Io.Error]): Rules =
    val exclude = gitDir / "info" / "exclude"
    val global = if exclude.existent() then parse(Objects.text(exclude.read[Data]), t"") else Nil

    def recur(directory: Path on Linux, base: Text): List[Rule] =
      val file: Path on Linux = Refstore.descend(directory, List(t".gitignore"))
      val own = if file.existent() then parse(Objects.text(file.read[Data]), base) else Nil

      def visit(children: List[Path on Linux], acc: List[Rule]): List[Rule] = children match
        case child :: rest =>
          if child.name == t".git" then visit(rest, acc)
          else if unsafely(child.entry()) == Directory then
            val childBase = if base.length == 0 then child.name else t"$base/${child.name}"
            visit(rest, acc + recur(child, childBase))
          else
            visit(rest, acc)

        case _ => acc

      visit(directory.children.to[List], own)

    Rules(global + recur(worktree, t""))
