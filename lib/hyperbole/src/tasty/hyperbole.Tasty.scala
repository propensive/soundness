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

import anticipation.*
import digression.*
import gossamer.*
import rudiments.*
import vacuous.*

import StackTrace.Frame.Kind

// The TASTy data model: what a `.tasty` file says about the definitions it holds. Reading a
// file into this model needs the compiler's unpickler classes at runtime — supplied by the
// caller's classpath, as for `anthology` — but no compiler `Context`, no classpath and no
// filesystem: `parse` takes raw bytes, so a debugger can feed it TASTy fetched over a wire.
// Presentation (the `Teletypeable` renderings of symbols and trees) lives in `hyperbole.core`,
// above this component's dependencies.
object Tasty:
  // Reads a `.tasty` file's definitions. Anything unexpected — an unknown version, a truncated
  // file, an absent section — yields `Unset`: TASTy is only ever a source of extra detail.
  def parse(data: Data): Optional[File] = TastyFiles(data)

  // TastySymbol → Tasty.Symbol
  case class Symbol
    ( prefix:     Text,
      name:       Text,
      flags:      List[(Text, Boolean)],
      properties: List[(Text, Boolean)],
      details:    List[(Text, List[Text] | Text)] )

  // TastyTree → Tasty.Tree
  case class Tree
    ( tag:          Char,
      typeName:     Text,
      name:         Text,
      expr:         Text,
      source:       Text,
      nodes:        List[Tree],
      param:        Optional[Text],
      term:         Boolean,
      definitional: Boolean ):

    def shortCode: Text =
      val c = expr.upto(_ != '\n')
      if c.length != expr.length then t"$c..." else expr

    def typeNode: Tree = copy(term = false)
    def definition: Tree = copy(definitional = true)

  // TastyDefinition → Tasty.Definition
  // One definition the compiler recorded in a TASTy file, reduced to what resolving a stack frame
  // needs: what the definition is called, what encloses it, and which part of the source it covers.
  // `owners` runs innermost-first, and includes the package.
  case class Definition
    ( name:      Text,
      owners:    List[Text],
      kind:      Kind,
      start:     Int,
      end:       Int,
      firstLine: Int,
      lastLine:  Int ):

    def span: Int = end - start
    def covers(line: Int): Boolean = firstLine <= line && line <= lastLine

  // TastyFile → Tasty.File (the data; the parser stays in `stacks`)
  // The definitions the compiler recorded for one top-level class, and the source file they came
  // from. `path` is the full path the file was compiled from, of which a stack trace keeps only the
  // last segment.
  case class File(path: Optional[Text], definitions: List[Definition]):
    // Every definition covering `line`, innermost first, where nesting is measured by how much
    // source a definition covers—so an anonymous function comes before the method containing it.
    // Definitions the compiler synthesized, such as a constructor an `object` never declared, are
    // pickled with an empty extent at whatever position was to hand, and so cannot be innermost
    // anything; they sort last, to be reached only when a frame really is one of them.
    def covering(line: Int): List[Definition] =
      val covered: List[Definition] = definitions.filter(_.covers(line))

      covered.order: definition =>
        (if definition.span == 0 then 1 else 0, definition.span)
