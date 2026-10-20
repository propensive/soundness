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
package susurration

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The subset of `~/.ssh/config` (ssh_config(5)) a client needs to reach a host as the user's
// `ssh` would: `Host` blocks of patterns (with `*`, `?` and `!`), and within them `HostName`,
// `User`, `Port` and `IdentityFile`. As in OpenSSH, the first obtained value of a keyword wins,
// so more specific blocks go before `Host *`, except that every `IdentityFile` accumulates. The
// `%h` token in `HostName` and `IdentityFile` expands to the host as given; `~` is left for the
// caller, who knows the home directory.
//
// `Match` blocks, `Include` and the other keywords are not interpreted: a `Match` block's
// contents are skipped, and unknown keywords are ignored.
object SshConfig:
  case class Resolution
    ( hostName: Text, user: Optional[Text], port: Int, identityFiles: List[Text] )

  // A `Host` block: the patterns it applies to and its keyword settings, lower-cased, in order.
  case class Block(patterns: List[Text], settings: List[(Text, Text)]):
    def matches(host: Text): Boolean =
      def negates(pattern: Text): Boolean =
        pattern.starts(t"!") && KnownHosts.glob(pattern.skip(1), host)

      def names(pattern: Text): Boolean = !pattern.starts(t"!") && KnownHosts.glob(pattern, host)

      !patterns.exists(negates) && patterns.exists(names)

  private enum Line:
    case Block(patterns: List[Text])
    case Skip
    case Keyword(name: Text, value: Text)

  def apply(text: Text): SshConfig =
    val lines = text.cut(t"\n").map(parse)

    // Group each `Host` block's keywords with its patterns; keywords before any `Host` apply
    // everywhere.
    def recur
      ( lines:    List[Line],
        patterns: List[Text],
        settings: List[(Text, Text)],
        blocks:   List[Block] )
    :   List[Block] =

      lines match
        case Nil =>
          (Block(patterns, settings.reverse) :: blocks).reverse

        case Line.Block(next) :: rest =>
          recur(rest, next, Nil, Block(patterns, settings.reverse) :: blocks)

        case Line.Skip :: rest =>
          recur(rest, patterns, settings, blocks)

        case Line.Keyword(name, value) :: rest =>
          recur(rest, patterns, (name, value) :: settings, blocks)

        case _ =>
          blocks.reverse

    SshConfig(recur(lines, List(t"*"), Nil, Nil))

  private def parse(line: Text): Line =
    val trimmed = line.trim

    if trimmed.length == 0 || trimmed.starts(t"#") then Line.Skip else
      // `Keyword value`, `Keyword=value` or `Keyword = value`
      val separated = trimmed.s.replaceFirst("^([A-Za-z]+)\\s*=\\s*", "$1 ").nn.tt

      separated.cut(t" ").filter(_.length > 0) match
        case keyword :: values if keyword.lower == t"host"  => Line.Block(values)
        case keyword :: values if keyword.lower == t"match" => Line.Block(Nil)
        case keyword :: value :: _                          => Line.Keyword(keyword.lower, value)
        case _                                              => Line.Skip

case class SshConfig(blocks: List[SshConfig.Block]):
  import SshConfig.*

  // Every value of `keyword` for `host`, in the order obtained.
  def values(host: Text, keyword: Text): List[Text] =
    blocks.filter(_.matches(host)).bind(_.settings).filter(_(0) == keyword.lower).map(_(1))

  def resolve(host: Text): Resolution =
    def expand(value: Text): Text = value.sub(t"%h", host)
    val hostName = values(host, t"hostname").prim.let(expand).or(host)
    val user = values(host, t"user").prim
    val port = values(host, t"port").prim.let { port => safely(port.s.toInt) }.or(22)

    Resolution(hostName, user, port, values(host, t"identityfile").map(expand))
