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
package hyperbole

import dotty.tools.dotc.core.tasty.TastyUnpickler

import anticipation.*
import rudiments.*
import vacuous.*

object TastyFile:
  // A TASTy file is only ever a source of extra detail, so anything unexpected about it—a version
  // this reader does not understand, a truncated file, a section that is absent—means falling
  // back to what the stack trace already said, never failing.
  def apply(data: Data): Optional[TastyFile] =
    try
      val bytes = new scala.Array[Byte](data.length)
      System.arraycopy(Array.unsafeJvm(data), 0, bytes, 0, data.length)
      // The compiler's unpickler takes a pure array; `bytes` is freshly allocated just above.
      val unpickler = TastyUnpickler(scala.caps.unsafe.unsafeAssumePure(bytes))
      val positions = unpickler.unpickle(stacksInternal.PositionSection())

      positions.map: positions =>
        val definitions =
          unpickler.unpickle(stacksInternal.DefinitionSection(positions)).getOrElse(Nil)

        val path = unpickler.unpickle(stacksInternal.AttributeSection()).getOrElse(Unset)

        TastyFile(path, definitions)

      . getOrElse(Unset)

    catch case error: Throwable => Unset

// The definitions the compiler recorded for one top-level class, and the source file they came
// from. `path` is the full path the file was compiled from, of which a stack trace keeps only the
// last segment.
case class TastyFile(path: Optional[Text], definitions: List[TastyDefinition]):
  // Every definition covering `line`, innermost first, where nesting is measured by how much
  // source a definition covers—so an anonymous function comes before the method containing it.
  // Definitions the compiler synthesized, such as a constructor an `object` never declared, are
  // pickled with an empty extent at whatever position was to hand, and so cannot be innermost
  // anything; they sort last, to be reached only when a frame really is one of them.
  def covering(line: Int): List[TastyDefinition] =
    List.of:
      definitions.stdlib.filter(_.covers(line)).sortBy: definition =>
        (if definition.span == 0 then 1 else 0, definition.span)
