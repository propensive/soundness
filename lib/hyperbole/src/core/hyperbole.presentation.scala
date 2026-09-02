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
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import iridescence.*
import rudiments.*
import spectacular.*
import vacuous.*

import digression.StackTrace

// The presentation of the TASTy model: `Teletypeable` renderings of symbols and trees. These
// are toplevel named givens rather than companions' members because the model now lives in
// `hyperbole.tasty`, beneath the table- and tree-rendering dependencies these need; a
// same-package toplevel given resolves without an import inside `hyperbole`, and by name
// elsewhere.
given tastySymbolTeletypeable: (palette: TastyPalette) => Tasty.Symbol is Teletypeable =
  symbol =>
    import tableStyles.defaultTableStyle

    val flags =
      symbol.flags.map: (flag, on) =>
        if on then e"${Bg(palette.flagOff)}(${Fg(palette.black)}(·${flag}·))"
        else e"${Fg(palette.flagOff)}($flag)"

      . join(e" ")

    val properties =
      symbol.properties.map: (property, on) =>
        if on then e"${Bg(palette.propertyOn)}(${Fg(palette.black)}(·${property}·))"
        else e"${palette.propertyOff}($property)"

      . join(e" ")

    val details =
      symbol.details.map: detail =>
        detail.absolve match
          case (key, value: Text) =>
            key -> e"${Fg(palette.outline)}($value)"

          case (key, items: List[Text] @unchecked) =>
            key -> e"${Fg(palette.outline)}(${items.join(t", ")})"

    val name = (t"Name", e"$Bold(${symbol.prefix}${Fg(palette.foreground)}(${symbol.name}))")

    Scaffold[(Text, Teletype)]
      ( Column(e"$Bold(Property)", textAlign = TextAlignment.Right)(_(0)),
        Column(e"$Bold(Value)", sizing = columnar.ParagraphOrBreak)(_(1)) )

    . tabulate(name :: (t"Flags", flags) :: (t"Properties", properties) :: details)
    . grid(120)
    . render
    . join(e"\n")

// One row of a rendered tree expansion; a private detail of `tastyTreeTeletypeable`.
private case class TastyTreeExpansion
  ( text: Teletype, typeName: Text, param: Optional[Text], expr: Text, source: Teletype )

private def expandTastyTree(tree: Tasty.Tree)(using palette: TastyPalette)
:   List[TastyTreeExpansion] =

  TreeDiagram.by[Tasty.Tree](_.nodes)(tree).map: tiles =>
    node =>
      val color = (node.term, node.definitional) match
        case (true, true)   => palette.termDefinition
        case (false, true)  => palette.typeDefinition
        case (true, false)  => palette.termReference
        case (false, false) => palette.typeReference

      val text = e"$color(${node.name})"
      val tag2: Text = if node.tag == ' ' then "▪".tt else "⟨"+node.tag+"⟩"

      // The root tile is dropped: it prefixes the tree's own line, which is drawn separately.
      val rest: List[TreeTile] = tiles match
        case _ :: rest => rest
        case _         => Nil

      val prefix: Text = rest.map(treeStyles.defaultTreeStyle.text(_)).join

      TastyTreeExpansion
        ( e"$prefix$tag2 $text",
          node.typeName,
          node.param,
          node.shortCode,
          node.source.teletype )

  . to[List]

given tastyTreeTeletypeable: (palette: TastyPalette) => Tasty.Tree is Teletypeable =
  tastyTree =>
    import tableStyles.minimalTableStyle

    val expansions = expandTastyTree(tastyTree)

    val indents =
      expansions.filter(!_.source.nil).map(_.source.plain.keep(_ == ' ').length)

    val crop = indents.least.or(0)

    Scaffold[TastyTreeExpansion]
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
