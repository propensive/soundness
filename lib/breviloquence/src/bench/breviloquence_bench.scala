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
package breviloquence

import anticipation.*
import contingency.*, strategies.throwUnsafely
import proscenium.*

// The direct-parsing schema for the benchmark corpora: case classes whose field names are the
// keys of corpora 2 (users) and 3/7 (log entries), so the same bytes decode through the AST
// (`Cbor.Ast.parse`) and straight off the input (`read[… in Cbor]` with a `Cbor.Parsable`).
// Top-level, and in their own file, so the generated parsers resolve cleanly when the staged
// benchmark bodies are recompiled in a separate compilation unit.

case class BenchUser
   ( id:       Long,
     username: Text,
     email:    Text,
     active:   Boolean,
     role:     Text )
derives CanEqual

case class BenchUsers(users: List[BenchUser]) derives CanEqual

case class LogEntry
   ( timestamp: Long,
     level:     Text,
     service:   Text,
     requestId: Text,
     userId:    Long,
     message:   Text )
derives CanEqual

case class Logs(logs: List[LogEntry]) derives CanEqual

// Generated once, here, and brought into scope *inside* the staged bodies that measure them
// (`given … = breviloquence.benchUsersParsable`), so the AST rows keep resolving to the AST
// path and an operation measures the parser, never the generation of its parser.
val benchUsersParsable: BenchUsers is Cbor.Parsable = Inlinable.parsable[BenchUsers]
val logsParsable:       Logs is Cbor.Parsable       = Inlinable.parsable[Logs]

// A hand-written streaming consumer with a tiny live set: counts the entries of `{"logs": [...]}`
// (definite or indefinite) without materialising any of them, so a constrained-heap stress run
// over a large document measures the parser's buffering alone, not the decoded value.
val countLogs: Long is Cbor.Parsable = Cbor.Parsable: reader =>
  var count = 0L
  var remaining = reader.openMap()

  while remaining != 0 do
    if remaining < 0 && reader.breakEnd() then remaining = 0
    else
      val key = reader.keyName()

      if key != null && key == "logs" then
        var left = reader.openArray()

        while left != 0 do
          if left < 0 && reader.breakEnd() then left = 0
          else
            reader.skipValue()
            count += 1
            if left > 0 then left -= 1
      else reader.skipValue()

      if remaining > 0 then remaining -= 1

  count
