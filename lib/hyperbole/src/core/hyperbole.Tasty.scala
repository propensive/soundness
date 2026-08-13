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
import dendrology.*
import denominative.*
import digression.*
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import iridescence.*
import rudiments.*
import spectacular.*
import vacuous.*

import StackTrace.Frame.Kind

// The TASTy data model: what a `.tasty` file says about the definitions it holds, and how
// symbols and trees within it are presented. Reading a file into this model needs the
// compiler's unpickler, so that lives in `hyperbole.stacks`; the model itself does not.
object Tasty:
  // TastySymbol → Tasty.Symbol
  object Symbol:
    import tableStyles.defaultTableStyle

    given teletypeable: (palette: TastyPalette) => Symbol is Teletypeable =
      symbol =>

        val flags =
          symbol.flags.stdlib.map: (flag, on) =>
            if on then e"${Bg(palette.flagOff)}(${Fg(palette.black)}(·${flag}·))"
            else e"${Fg(palette.flagOff)}($flag)"

          . join(e" ")

        val properties =
          symbol.properties.stdlib.map: (property, on) =>
            if on then e"${Bg(palette.propertyOn)}(${Fg(palette.black)}(·${property}·))"
            else e"${palette.propertyOff}($property)"

          . join(e" ")

        val details =
          symbol.details.stdlib.map: detail =>
            detail.absolve match
              case (key, value: Text) =>
                key -> e"${Fg(palette.outline)}($value)"

              case (key, items: List[Text] @unchecked) =>
                key -> e"${Fg(palette.outline)}(${items.join(t", ")})"

          . to(List)

        val name = (t"Name", e"$Bold(${symbol.prefix}${Fg(palette.foreground)}(${symbol.name}))")

        Scaffold[(Text, Teletype)]
          ( Column(e"$Bold(Property)", textAlign = TextAlignment.Right)(_(0)),
            Column(e"$Bold(Value)", sizing = columnar.ParagraphOrBreak)(_(1)) )

        . tabulate(List.of(name :: (t"Flags", flags) :: (t"Properties", properties) :: details.stdlib))
        . grid(120)
        . render
        . join(e"\n")

  case class Symbol
    ( prefix:     Text,
      name:       Text,
      flags:      List[(Text, Boolean)],
      properties: List[(Text, Boolean)],
      details:    List[(Text, List[Text] | Text)] )

  // TastyTree → Tasty.Tree
  object Tree:
    import tableStyles.minimalTableStyle

    case class Expansion
      ( text: Teletype, typeName: Text, param: Optional[Text], expr: Text, source: Teletype )

    private def expand(tree: Tree)(using palette: TastyPalette): List[Expansion] =
      TreeDiagram.by[Tree](_.nodes)(tree).map: tiles =>
        node =>
          val color = (node.term, node.definitional) match
            case (true, true)   => palette.termDefinition
            case (false, true)  => palette.typeDefinition
            case (true, false)  => palette.termReference
            case (false, false) => palette.typeReference

          val text = e"$color(${node.name})"
          val tag2: Text = if node.tag == ' ' then "▪".tt else "⟨"+node.tag+"⟩"

          Expansion
            ( e"${List.of(tiles.stdlib.drop(1).map(treeStyles.defaultTreeStyle.text(_))).join}$tag2 $text",
              node.typeName,
              node.param,
              node.shortCode,
              node.source.teletype )

      . stdlib.to(List)

    given teletypeable: (palette: TastyPalette) => Tree is Teletypeable =
      tastyTree =>

        val expansions = expand(tastyTree)

        val indents =
          expansions.stdlib.filter(!_.source.nil).map(_.source.plain.keep(_ == ' ').length)

        val crop = indents.min

        Scaffold[Expansion]
          ( Column(e"TASTy"): node =>
              val param =
                node.param.let { param => e"$Italic(${Fg(palette.accented)}($param))" }.or(e"")

              e"${node.text} $param",
            Column(e"Type"): node =>
              val name = StackTrace.rewrite(node.typeName.s, false)
              if node.typeName.nil then e"" else e"${Fg(palette.outline)}(: $Italic(${name}))",
            Column(e"Source")(_.source.skip(crop)) )

        . tabulate(expansions)
        . grid(10000)
        . render
        . join(e"\n")

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
      List.of:
        definitions.stdlib.filter(_.covers(line)).sortBy: definition =>
          (if definition.span == 0 then 1 else 0, definition.span)
