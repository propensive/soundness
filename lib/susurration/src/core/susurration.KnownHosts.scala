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

import java.nio.charset as jnc

import anticipation.*
import contingency.*
import enigmatic.*
import gossamer.*
import monotonous.*, alphabets.base64Standard
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

// OpenSSH's `known_hosts` file (sshd(8), "SSH_KNOWN_HOSTS FILE FORMAT"): one host key per line
// as `hosts type base64 [comment]`, where `hosts` is a comma-separated list of patterns (`*`
// and `?` wildcards, `!` negation, `[host]:port` for a non-default port) or a hashed entry
// `|1|salt|hash` (HMAC-SHA1 of the hostname under the salt, so the file reveals nothing about
// which hosts it knows). A line may carry the `@revoked` or `@cert-authority` marker. Lines that
// cannot be parsed are skipped, as OpenSSH skips them.
//
// `verify` is the whole of host-key checking: given the host, port and the key the server
// presented, it answers whether the key is the one on file, a different one (the message every
// SSH user has read in capitals), unknown, or revoked.
object KnownHosts:
  enum Verdict:
    case Known, Unknown, Changed, Revoked

  // A line's host specification: the patterns it names, or the hash it hides them behind.
  enum Host:
    case Patterns(patterns: List[Text])
    case Hashed(salt: Data, hash: Data)

    def matches(hostname: Text, port: Int)(using crypto: Crypto): Boolean =
      val bracketed = if port == 22 then hostname else t"[$hostname]:$port"

      this match
        case Hashed(salt, hash) =>
          // HMAC-SHA1 is the only hash OpenSSH ever used here, and a hashed hostname is not
          // security-critical, so it is taken from the provider directly rather than through
          // `hmac`, whose `Sha1` concession is for a user's choice of a weak algorithm.
          // [java-boundary] fresh array returned by the JDK, retained by nobody else
          val name = Array.unsafeFrozen(bracketed.s.getBytes(jnc.StandardCharsets.UTF_8).nn)
          Array.same(crypto.hmac(t"HmacSHA1").mac(salt, name), hash)

        case Patterns(patterns) =>
          def negates(pattern: Text): Boolean =
            pattern.starts(t"!") && glob(pattern.skip(1), bracketed)

          def names(pattern: Text): Boolean =
            !pattern.starts(t"!") && glob(pattern, bracketed)

          !patterns.exists(negates) && patterns.exists(names)

  // One line of the file. The key is kept as OpenSSH's own blob, so an entry can be compared with
  // a presented key of any type, and re-encoded exactly as it was read.
  case class Entry
    ( marker: Optional[Text], host: Host, keyType: Text, key: Data, comment: Optional[Text] ):

    def revoked: Boolean = marker == t"@revoked"
    def authority: Boolean = marker == t"@cert-authority"

  def apply(text: Text): KnownHosts =
    KnownHosts(text.cut(t"\n").map(entry).sweep { case entry: Entry => entry }.to[List])

  def entry(line: Text): Optional[Entry] =
    val trimmed = line.trim

    if trimmed.length == 0 || trimmed.starts(t"#") then Unset else
      val fields = trimmed.cut(t" ").filter(_.length > 0)

      val (marker, rest) = fields match
        case marker :: rest if marker.starts(t"@") => (marker, rest)
        case rest                                  => (Unset, rest)

      rest match
        case hosts :: keyType :: encoded :: comment =>
          // A key blob begins with its own type, which must be the line's: anything else is not
          // a key, however it decodes.
          val key: Optional[Data] = safely(encoded.deserialize[Base64]).let: key =>
            if safely(Wire.Reader(key).text()) == keyType then key else Unset

          val host: Optional[Host] =
            if hosts.starts(t"|1|") then hosts.skip(3).cut(t"|") match
              case salt :: hash :: Nil =>
                safely(salt.deserialize[Base64]).let: salt =>
                  safely(hash.deserialize[Base64]).let(Host.Hashed(salt, _))

              case _ =>
                Unset
            else Host.Patterns(hosts.cut(t","))

          key.let: key =>
            host.let: host =>
              val remark = if comment == Nil then Unset else comment.join(t" ")
              Entry(marker, host, keyType, key, remark)

        case _ =>
          Unset

  // A shell-style glob with `*` and `?`, matched case-insensitively as OpenSSH matches hostnames.
  def glob(pattern: Text, candidate: Text): Boolean =
    val p = pattern.lower.s.toCharArray.nn
    val c = candidate.lower.s.toCharArray.nn

    def recur(pi: Int, ci: Int): Boolean =
      if pi == p.length then ci == c.length
      else p(pi) match
        case '*' => recur(pi + 1, ci) || (ci < c.length && recur(pi, ci + 1))
        case '?' => ci < c.length && recur(pi + 1, ci + 1)
        case ch  => ci < c.length && c(ci) == ch && recur(pi + 1, ci + 1)

    recur(0, 0)

  // A `known_hosts` line for a host and its key, as `ssh-keyscan` would write it.
  def line(hostname: Text, port: Int, keyType: Text, key: Data): Text =
    val host = if port == 22 then hostname else t"[$hostname]:$port"
    t"$host $keyType ${key.serialize[Base64]}"

case class KnownHosts(entries: List[KnownHosts.Entry]):
  import KnownHosts.*

  // The entries naming the host, revoked or not, in file order.
  def lookup(hostname: Text, port: Int = 22)(using Crypto): List[Entry] =
    entries.filter(_.host.matches(hostname, port))

  // The verdict on a key the host presented, given as OpenSSH's blob (`OpenSsh.publicKey`'s
  // input, and what the transport receives). A revoked key is revoked whatever else is on file;
  // otherwise the key is known if any non-authority entry for the host carries it, changed if
  // the host has entries of this key's type but not this key, and unknown if it has none.
  def verify(hostname: Text, port: Int, keyType: Text, key: Data)(using Crypto): Verdict =
    val matching = lookup(hostname, port).filter(!_.authority)
    val same = matching.filter: entry => entry.keyType == keyType && Array.same(entry.key, key)
    val sameType = matching.exists: entry => entry.keyType == keyType && !entry.revoked

    if same.exists(_.revoked) then Verdict.Revoked
    else if same.exists(!_.revoked) then Verdict.Known
    else if sameType then Verdict.Changed
    else Verdict.Unknown
