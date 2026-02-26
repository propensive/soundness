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
package hyperbole

import anticipation.*
import dendrology.*
import denominative.*
import digression.*
import escapade.*
import escritoire.*, tableStyles.minimal, columnAttenuation.ignore
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import iridescence.*
import proscenium.*
import spectacular.*
import vacuous.*

object TastyTree:
  case class Expansion
    ( text: Teletype, typeName: Text, param: Optional[Text], expr: Text, source: Teletype )

  private def expand(tree: TastyTree): List[Expansion] =
    TreeDiagram.by[TastyTree](_.nodes)(tree).map: node =>
      val color = (node.term, node.definitional) match
        case (true, true)   => webColors.Tomato
        case (false, true)  => webColors.YellowGreen
        case (true, false)  => webColors.FireBrick
        case (false, false) => webColors.MediumSeaGreen

      val text = e"$color(${node.name})"
      val tag2: Text = if node.tag == ' ' then "▪".tt else "⟨"+node.tag+"⟩"

      Expansion
        ( e"${tiles.drop(1).map(treeStyles.default.text(_)).join}$tag2 $text",
          node.typeName,
          node.param,
          node.shortCode,
          node.source.teletype )

    . to(List)


  given showable: TastyTree is Teletypeable =
    tastyTree =>

      val expansions = expand(tastyTree)
      val indents = expansions.filter(!_.source.nil).map(_.source.plain.s.takeWhile(_ == ' ').length)
      val crop = indents.min

      Scaffold[Expansion]
        ( Column(e"TASTy"): node =>
            val param = node.param.let { param => e"$Italic(${webColors.Orange}($param))" }.or(e"")
            e"${node.text} $param",
          Column(e"Type"): node =>
            val name = StackTrace.rewrite(node.typeName.s, false)
            if node.typeName.nil then e"" else e"${webColors.Gray}(: $Italic(${name}))",
          Column(e"Source")(_.source.skip(crop)) )

      . tabulate(expansions)
      . grid(10000)
      . render
      . join(e"\n")


case class TastyTree
  ( tag:          Char,
    typeName:     Text,
    name:         Text,
    expr:         Text,
    source:       Text,
    nodes:        List[TastyTree],
    param:        Optional[Text],
    term:         Boolean,
    definitional: Boolean ):

  def shortCode: Text =
    val c = expr.upto(_ != '\n')
    if c.length != expr.length then t"$c..." else expr

  def typeNode: TastyTree = copy(term = false)
  def definition: TastyTree = copy(definitional = true)
