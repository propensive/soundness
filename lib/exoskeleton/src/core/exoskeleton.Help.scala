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
package exoskeleton

import scala.language.experimental.pureFunctions

import anticipation.*
import escapade.*
import escritoire.*
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import polysyllabic.*
import symbolism.*
import rudiments.*
import vacuous.*

object Help:
  case class Param
    ( name:        Text,
      aliases:     List[Text],
      description: Optional[Text | Teletype],
      repeatable:  Boolean,
      global:      Boolean        = false,
      operand:     Optional[Text] = Unset )

  // A line of the rendered body: an indented label with an optional description, laid out so
  // that every description starts in the same column; a bold subheading, which does not
  // participate in that alignment; or a blank spacer between blocks.
  private enum Row:
    case Item(depth: Int, label: Text, bold: Boolean, description: Optional[Text | Teletype])
    case Label(depth: Int, text: Text)
    case Blank

  private[exoskeleton] def label(param: Param): Text =
    val names: Text = ((param.name :: param.aliases): List[Text]).join(t", ")

    val core: Text = param.operand.absolve match
      case Unset         => names
      case operand: Text => t"$names <$operand>"

    if param.repeatable then t"$core..." else core

  // Global flags precede the subcommand on the command line; flags belonging to this command
  // or to any subcommand follow it. Each part appears only if such flags actually exist, and
  // when only a single flag is possible, it is named outright rather than called `options`.
  // Shared by the `teletype` renderer and the manpage renderer, so their synopses agree.
  private[exoskeleton] def summarize(params: scala.List[Param], plural: Text): Text =
    if params.isEmpty then t""
    else if params.length == 1 then
      val param = params.head

      val core: Text = param.operand.absolve match
        case Unset         => param.name
        case operand: Text => t"${param.name} <$operand>"

      if param.repeatable then t" [$core]..." else t" [$core]"
    else
      t" [$plural]"

  private[exoskeleton] def descendants(node: Help): scala.List[Param] =
    node.parameters.stdlib ::: node.subcommands.stdlib.flatMap(descendants)

  private def paramItems(params: scala.List[Param], depth: Int): scala.List[Row] =
    params.map: param => Row.Item(depth, label(param), false, param.description)

  private def commandRows(children: scala.List[Help], depth: Int): scala.List[Row] =
    children.flatMap: sub =>
      val block =
        Row.Item(depth, sub.command, true, sub.description) ::
          paramItems(sub.parameters.stdlib, depth + 1) :::
          commandRows(sub.subcommands.stdlib, depth + 1)

      if sub.parameters.stdlib.isEmpty && sub.subcommands.stdlib.isEmpty then block
      else (Row.Blank :: block) :+ Row.Blank

  // Collapse runs of consecutive spacers and strip them from either end of a section.
  private def compact(rows: scala.List[Row]): scala.List[Row] =
    val trimmed = rows.dropWhile(_ == Row.Blank).reverse.dropWhile(_ == Row.Blank).reverse

    trimmed.foldRight(scala.List[Row]()): (row, done) =>
      if row == Row.Blank && !done.isEmpty && done.head == Row.Blank then done else row :: done

  // Render the tree to a `Teletype`, then borrow escapade's `Teletype is Printable` so that a
  // `Help` value can be passed straight to `Out.println` and print nicely on the terminal,
  // wrapped to its width. `Hyphenation.fallback` satisfies the condition without any import,
  // wrapping only at spaces; import a language's patterns (e.g. `hyphenations.english`) for
  // hyphenated wrapping.
  given printable: (hyphenation: Hyphenation) => Help is Printable =
    (help, termcap) => summon[Teletype is Printable].print(help.teletype(termcap.width), termcap)

case class Help
  ( command:     Text,
    description: Optional[Text | Teletype],
    parameters:  List[Help.Param],
    subcommands: List[Help],
    group:       Optional[CommandGroup] = Unset,
    // The exit statuses this command's `execute` block can return, and the environment
    // variables it consults, both discovered while the tree was built. Empty for a `Help`
    // constructed by hand.
    statuses:    List[Status]           = Nil,
    variables:   List[Text]             = Nil ):

  def teletype(width: Int = Int.MaxValue)(using Hyphenation): Teletype =
    val globalParams: scala.List[Help.Param] = parameters.stdlib.filter(_.global)
    val localParams: scala.List[Help.Param] = parameters.stdlib.filter(!_.global)
    val ungrouped: scala.List[Help] = subcommands.stdlib.filter(_.group == Unset)
    val groupList: scala.List[CommandGroup] = subcommands.stdlib.flatMap(_.group.option).distinct

    // Wrap prose into `available` columns using escritoire's borderless paragraph layout, which
    // breaks at spaces and admissible hyphenation points. Below 20 columns, overrun instead.
    def wrap(teletype: Teletype, available: Int): scala.List[Teletype] =
      columnar.Paragraph.fit(Array(teletype), available.max(20), TextAlignment.Left)
      . stdlib.toList

    // Each section is a heading (one or more unaligned lines) plus its rows, which all
    // participate in the global description-column alignment.
    val groupSections: scala.List[(scala.List[Teletype], scala.List[Help.Row])] =
      groupList.map: group =>
        val members = subcommands.stdlib.filter(_.group == group)

        // A flag borne identically by every member of the group is factored out of the
        // individual subcommands and listed once, as common to the whole group.
        val common: scala.List[Help.Param] =
          if members.length < 2 then scala.List()
          else members.head.parameters.stdlib.filter: param =>
            members.forall(_.parameters.stdlib.contains(param))

        val factored: scala.List[Help] = members.map: member =>
          member.copy(parameters = member.parameters.filter(!common.contains(_)))

        val commonRows: scala.List[Help.Row] =
          if common.isEmpty then scala.List()
          else
            Help.Row.Blank :: Help.Row.Label(1, t"Common options:") :: Help.paramItems(common, 2)

        val title: Text = t"${group.name}:"

        val explanation: scala.List[Teletype] = group.description.absolve match
          case Unset              => scala.List()
          case text: Text         => wrap(e"$text", width - 2)
          case teletype: Teletype => wrap(teletype, width - 2)

        val indented: scala.List[Teletype] = explanation.map: line => e"  $line"

        val heading: scala.List[Teletype] =
          if indented.isEmpty then scala.List(e"$Bold($title)")
          else e"$Bold($title)" :: indented ::: scala.List(e"")

        (heading, Help.compact(Help.commandRows(factored, 1) ::: commonRows))

    // A tool without subcommands never reaches dispatch, so all of its flags count as "global";
    // presenting them as anything other than plain options would be noise.
    val leaf: Boolean = subcommands.stdlib.isEmpty

    val standard: scala.List[(scala.List[Teletype], scala.List[Help.Row])] =
      if leaf then
        scala.List
          ( (scala.List(e"$Bold(Options:)"),
             Help.compact(Help.paramItems(parameters.stdlib, 1))) )
      else
        scala.List
          ( (scala.List(e"$Bold(Global options:)"),
             Help.compact(Help.paramItems(globalParams, 1))),
            (scala.List(e"$Bold(Options:)"),
             Help.compact(Help.paramItems(localParams, 1))),
            (scala.List(e"$Bold(Commands:)"),
             Help.compact(Help.commandRows(ungrouped, 1))) )

    val sections: scala.List[(scala.List[Teletype], scala.List[Help.Row])] =
      standard.filter(!_._2.isEmpty) ::: groupSections

    // The column at which every description starts: two spaces after the widest label.
    val column: Int =
      sections.flatMap(_._2).map:
        case Help.Row.Item(depth, label, _, _) => depth*2 + label.length
        case _                                 => 0

      . maxOption.getOrElse(0) + 2

    def render(row: Help.Row): scala.List[Teletype] = row match
      case Help.Row.Blank => scala.List(e"")

      case Help.Row.Label(depth, text) =>
        val indent: Text = t"  "*depth
        scala.List(e"$indent$Bold($text)")

      case Help.Row.Item(depth, label, bold, description) =>
        val indent: Text = t"  "*depth

        val explanation: Optional[Teletype] = description.absolve match
          case Unset              => Unset
          case text: Text         => e"$text"
          case teletype: Teletype => teletype

        explanation.absolve match
          case Unset =>
            scala.List(if bold then e"$indent$Bold($label)" else e"$indent$label")

          case explanation: Teletype =>
            val fitted: Text = label.fit(column - 2 - indent.length)
            val padded: Teletype = if bold then e"$indent$Bold($fitted)" else e"$indent$fitted"
            val margin: Text = t" "*column
            val lines = wrap(explanation, width - column)

            if lines.isEmpty then scala.List(padded)
            else e"$padded  ${lines.head}" :: lines.tail.map: line => e"$margin$line"

    val usage: Teletype =
      if leaf then e"$Bold(Usage:) $command${Help.summarize(parameters.stdlib, t"options")}"
      else
        val globals: Text = Help.summarize(globalParams, t"global options")

        val posterior =
          (localParams ::: subcommands.stdlib.flatMap(Help.descendants)).distinct

        val locals: Text = Help.summarize(posterior, t"options")

        e"$Bold(Usage:) $command$globals <command>$locals"

    val header: scala.List[Teletype] = description.absolve match
      case Unset              => scala.List(usage)
      case text: Text         => usage :: e"" :: wrap(e"$text", width)
      case teletype: Teletype => usage :: e"" :: wrap(teletype, width)

    val body: scala.List[Teletype] =
      header ::: sections.flatMap: (heading, rows) => e"" :: (heading ::: rows.flatMap(render))

    List.of(body).join(e"\n")
