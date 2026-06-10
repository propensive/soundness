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
package nomenclature

import anticipation.*

export nomenclature.internal.Name
export nomenclature.Moniker.Moniker

extension (inline context: StringContext)
  transparent inline def n: Any = ${protointernal.extractor('context)}

// Predicates backing the `DomIdentifier` and `CssIdentifier` constraints. They
// live here (not in the constraint objects) because a `Rule` super-constructor
// argument cannot reference the object's own members. Index loops keep within
// `java.lang`/`scala`/`proscenium` (no `Predef` string-collection extensions).
private[nomenclature] object identifierRules:
  // HTML element id: at least one character and no ASCII whitespace.
  def dom(name: Text): Boolean =
    val text = name.s
    val length = text.length

    if length == 0 then false else
      var index = 0
      var valid = true

      while valid && index < length do
        val char = text.charAt(index)

        if char == '\t' || char == '\n' || char == '\r' || char == ' ' || char.toInt == 12
        then valid = false

        index += 1

      valid

  // CSS `<ident-token>` (CSS Syntax Module Level 3), non-escaped subset: an
  // optional leading `-` (or `--`), an ident-start code point, then ident code
  // points. Escapes and a lone `-` are not accepted.
  def css(name: Text): Boolean =
    val text = name.s
    val length = text.length

    def identStart(char: Char): Boolean =
      char == '_' || char.toInt >= 0x80 || (char >= 'a' && char <= 'z')
      || (char >= 'A' && char <= 'Z')

    def identChar(char: Char): Boolean =
      identStart(char) || char == '-' || (char >= '0' && char <= '9')

    val start =
      if length == 0 then -1
      else if text.charAt(0) != '-' then (if identStart(text.charAt(0)) then 0 else -1)
      else if length >= 2 && text.charAt(1) == '-' then 2
      else if length >= 2 && identStart(text.charAt(1)) then 1
      else -1

    if start < 0 then false else
      var index = start
      var valid = true

      while valid && index < length do
        if !identChar(text.charAt(index)) then valid = false
        index += 1

      valid

transparent inline def disintersect[intersection] =
  ${protointernal.disintersection[intersection]}

transparent inline def staticCompanion[instance]: Matchable =
  ${anteprotointernal.staticCompanion[instance]}
