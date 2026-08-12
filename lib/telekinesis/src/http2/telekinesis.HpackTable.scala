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
package telekinesis

import scala.caps
import proscenium.compat.*

import scala.collection.mutable as scm

import anticipation.*
import gossamer.*
import vacuous.*

// An HPACK header field: a name and value. The encoded byte size used for dynamic
// table accounting is `name + value + 32` (RFC 7541 §4.1).
case class HpackEntry(name: Text, value: Text):
  def size: Int = name.s.length + value.s.length + 32

object HpackTable:
  // The 61-entry static table (RFC 7541, Appendix A). Index 1 is element 0 here.
  val static: Array[HpackEntry]^{} = Array.of[HpackEntry](
    HpackEntry(t":authority", t""),
    HpackEntry(t":method", t"GET"),
    HpackEntry(t":method", t"POST"),
    HpackEntry(t":path", t"/"),
    HpackEntry(t":path", t"/index.html"),
    HpackEntry(t":scheme", t"http"),
    HpackEntry(t":scheme", t"https"),
    HpackEntry(t":status", t"200"),
    HpackEntry(t":status", t"204"),
    HpackEntry(t":status", t"206"),
    HpackEntry(t":status", t"304"),
    HpackEntry(t":status", t"400"),
    HpackEntry(t":status", t"404"),
    HpackEntry(t":status", t"500"),
    HpackEntry(t"accept-charset", t""),
    HpackEntry(t"accept-encoding", t"gzip, deflate"),
    HpackEntry(t"accept-language", t""),
    HpackEntry(t"accept-ranges", t""),
    HpackEntry(t"accept", t""),
    HpackEntry(t"access-control-allow-origin", t""),
    HpackEntry(t"age", t""),
    HpackEntry(t"allow", t""),
    HpackEntry(t"authorization", t""),
    HpackEntry(t"cache-control", t""),
    HpackEntry(t"content-disposition", t""),
    HpackEntry(t"content-encoding", t""),
    HpackEntry(t"content-language", t""),
    HpackEntry(t"content-length", t""),
    HpackEntry(t"content-location", t""),
    HpackEntry(t"content-range", t""),
    HpackEntry(t"content-type", t""),
    HpackEntry(t"cookie", t""),
    HpackEntry(t"date", t""),
    HpackEntry(t"etag", t""),
    HpackEntry(t"expect", t""),
    HpackEntry(t"expires", t""),
    HpackEntry(t"from", t""),
    HpackEntry(t"host", t""),
    HpackEntry(t"if-match", t""),
    HpackEntry(t"if-modified-since", t""),
    HpackEntry(t"if-none-match", t""),
    HpackEntry(t"if-range", t""),
    HpackEntry(t"if-unmodified-since", t""),
    HpackEntry(t"last-modified", t""),
    HpackEntry(t"link", t""),
    HpackEntry(t"location", t""),
    HpackEntry(t"max-forwards", t""),
    HpackEntry(t"proxy-authenticate", t""),
    HpackEntry(t"proxy-authorization", t""),
    HpackEntry(t"range", t""),
    HpackEntry(t"referer", t""),
    HpackEntry(t"refresh", t""),
    HpackEntry(t"retry-after", t""),
    HpackEntry(t"server", t""),
    HpackEntry(t"set-cookie", t""),
    HpackEntry(t"strict-transport-security", t""),
    HpackEntry(t"transfer-encoding", t""),
    HpackEntry(t"user-agent", t""),
    HpackEntry(t"vary", t""),
    HpackEntry(t"via", t""),
    HpackEntry(t"www-authenticate", t"") )

// The HPACK dynamic table: a FIFO of recently-seen header fields, bounded by a
// byte-size limit, with oldest-first eviction. Combined with the static table it
// forms the HPACK address space: index 1..61 is static; 62.. is the dynamic table,
// most-recently-inserted first (RFC 7541 §2.3.3).
class HpackTable(initialMaxSize: Int = 4096):
  // Untracked: the dynamic table is confined to its owning `Hpack` codec, which
  // is itself confined to one connection's reader or writer daemon.
  @caps.unsafe.untrackedCaptures
  private val entries: scm.ArrayDeque[HpackEntry] = scm.ArrayDeque.empty[HpackEntry]
  @caps.unsafe.untrackedCaptures
  private var maxSize: Int = initialMaxSize
  @caps.unsafe.untrackedCaptures
  private var currentSize: Int = 0

  def size: Int = currentSize
  def capacity: Int = maxSize

  // Resize (HPACK dynamic-table-size-update); evicts to fit the new bound.
  def resize(newMax: Int): Unit =
    maxSize = newMax
    evict()

  private def evict(): Unit =
    while currentSize > maxSize && entries.nonEmpty do currentSize -= entries.removeLast().size

  // Insert at the front (most recent). An entry larger than the whole table
  // clears it and is itself not stored (RFC 7541 §4.4).
  def add(entry: HpackEntry): Unit =
    currentSize += entry.size
    entries.prepend(entry)
    evict()

  // Resolve an HPACK index (1-based) to its entry: static for 1..61, then dynamic.
  def lookup(index: Int): Optional[HpackEntry] =
    if index >= 1 && index <= HpackTable.static.length then HpackTable.static(index - 1)
    else
      val dynamicIndex = index - HpackTable.static.length - 1

      if dynamicIndex >= 0 && dynamicIndex < entries.length then entries(dynamicIndex) else Unset
