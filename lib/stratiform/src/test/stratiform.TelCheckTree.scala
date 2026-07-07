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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package stratiform

import anticipation.*
import gossamer.*
import vacuous.*

import CheckFormat.CheckTree

// Projects a Scala Tel.Document into the cross-language CheckTree that the
// upstream `.check` fixtures parse to. The Rust reference implementation
// uses snake_case field names; Scala uses camelCase. Field-name keys here
// follow the Rust naming so the comparison is symmetric.

object TelCheckTree:

  def of(tel: Tel): CheckTree = tel.document.lay(CheckTree.Variant(t"<no-doc>", Unset)): doc =>
    of(doc)

  def of(document: Tel.Document): CheckTree =
    CheckTree.Struct
      ( t"Document",
        List
         ( t"interpreter_directive" -> ofOptional(document.interpreterDirective)(s => CheckTree.Str(s)),
           t"pragma"                -> ofOptional(document.pragma)(ofPragma),
           t"line_endings"          -> ofLineEndings(document.lineEndings),
           t"children"              -> ofIArray(document.children)(ofBlock) ) )

  private def ofPragma(p: Tel.Pragma): CheckTree =
    val (major, minor) = p.version
    CheckTree.Struct
      ( t"Pragma",
        List
         ( t"version" -> CheckTree.Tuple(List(CheckTree.Num(major), CheckTree.Num(minor))),
           t"schema"  -> ofOptional(p.schema)(s => CheckTree.Str(s)),
           t"sigil"   -> ofOptional(p.sigil)(c => CheckTree.Str(Text(c.toString))) ) )

  private def ofLineEndings(lineEndings: Tel.LineEndings): CheckTree = lineEndings match
    case Tel.LineEndings.Lf   => CheckTree.Variant(t"LF", Unset)
    case Tel.LineEndings.Crlf => CheckTree.Variant(t"CRLF", Unset)

  private def ofBlock(block: Tel.Block): CheckTree =
    CheckTree.Struct
      ( t"Block",
        List
         ( t"comments"             -> ofIArray(block.comments)(ofComment),
           t"tabulation"           -> ofOptional(block.tabulation)(ofTabulation),
           t"compounds"            -> ofIArray(block.compounds)(ofCompound),
           t"trailing_blank_lines" -> CheckTree.Num(block.trailingBlankLines) ) )

  private def ofComment(comment: Tel.Comment): CheckTree =
    CheckTree.Struct(t"Comment", List(t"text" -> CheckTree.Str(comment.text)))

  private def ofTabulation(tabulation: Tel.Tabulation): CheckTree =
    CheckTree.Struct
      ( t"Tabulation",
        List
         ( t"marker_offsets" -> CheckTree.Sequence(tabulation.markerOffsets.toList.map(CheckTree.Num(_))),
           t"headings"       -> CheckTree.Sequence(tabulation.headings.toList.map(h => CheckTree.Str(h))) ) )

  private def ofCompound(compound: Tel.Compound): CheckTree =
    CheckTree.Struct
      ( t"Compound",
        List
         ( t"keyword"  -> CheckTree.Str(compound.keyword),
           t"atoms"    -> ofIArray(compound.atoms)(ofAtom),
           t"remark"   -> ofOptional(compound.remark)(s => CheckTree.Str(s)),
           t"children" -> ofIArray(compound.children)(ofBlock) ) )

  private def ofAtom(atom: Tel.Atom): CheckTree = atom match
    case Tel.Atom.Inline(text, precedingSpaces) =>
      CheckTree.Struct
        ( t"Inline",
          List
           ( t"text"             -> CheckTree.Str(text),
             t"preceding_spaces" -> CheckTree.Num(precedingSpaces) ) )

    case Tel.Atom.Source(text) =>
      CheckTree.Struct(t"Source", List(t"text" -> CheckTree.Str(text)))

    case Tel.Atom.Literal(delimiter, text) =>
      CheckTree.Struct
        ( t"Literal",
          List
           ( t"delimiter" -> CheckTree.Str(delimiter),
             t"text"      -> CheckTree.Str(text) ) )

  private def ofOptional[value](opt: Optional[value])(f: value => CheckTree): CheckTree =
    opt.lay(CheckTree.Variant(t"None", Unset))(v => CheckTree.Variant(t"Some", f(v)))

  private def ofIArray[value](items: IArray[value])(f: value => CheckTree): CheckTree =
    CheckTree.Sequence(items.toList.map(f))
