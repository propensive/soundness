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
import denominative.nil
import escapade.*
import escritoire.*
import gossamer.*
import hypotenuse.maximum
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
      operand:     Optional[Text] = Unset,
      required:    Boolean        = false )

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
  private[exoskeleton] def summarize(params: List[Param], plural: Text): Text =
    params match
      case Nil => t""

      case param :: Nil =>
        val core: Text = param.operand.absolve match
          case Unset         => param.name
          case operand: Text => t"${param.name} <$operand>"

        if param.repeatable then t" [$core]..." else t" [$core]"

      case _ =>
        t" [$plural]"

  private[exoskeleton] def descendants(node: Help): List[Param] =
    node.parameters + node.subcommands.flatMap(descendants)

  private def paramItems(params: List[Param], depth: Int): List[Row] =
    params.map: param =>
      // The `Optional` field is read through a typed local, never directly inside the lambda:
      // the direct form trips dotc's positionless `wildApprox` assertion (scala/scala3#24824).
      val explanation: Optional[Text | Teletype] = param.description

      val description: Optional[Text | Teletype] =
        if !param.required then explanation else explanation.absolve match
          case Unset              => t"(required)"
          case text: Text         => t"$text (required)"
          case teletype: Teletype => e"$teletype (required)"

      Row.Item(depth, label(param), false, description)

  private def commandRows(children: List[Help], depth: Int): List[Row] =
    children.flatMap: sub =>
      // As in `paramItems`, bound before it is read (`wildApprox`).
      val description: Optional[Text | Teletype] = sub.description

      val block =
        Row.Item(depth, sub.command, true, description) ::
          (paramItems(sub.parameters, depth + 1) + commandRows(sub.subcommands, depth + 1))

      if sub.parameters.nil && sub.subcommands.nil then block
      else (Row.Blank :: block) + List(Row.Blank)

  // Collapse runs of consecutive spacers and strip them from either end of a section.
  private def compact(rows: List[Row]): List[Row] =
    val trimmed = rows.span(_ == Row.Blank)._2.reverse.span(_ == Row.Blank)._2.reverse

    // Folded from the left onto a reversed accumulator, so the run-collapsing test looks at the
    // spacer already kept rather than the one still to come; either direction leaves exactly one
    // spacer per run, in the same place.
    val collapsed: List[Row] =
      trimmed.fold(Nil: List[Row]): (done, row) =>
        done match
          case Row.Blank :: _ if row == Row.Blank => done
          case _                                  => row :: done

    collapsed.reverse

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

  // The view of this help tree from inside the given subcommand path, as matched during
  // dispatch: the selected node, its command joined to the full path (so the usage line names
  // the subcommand), and every ancestor's flags carried in as global options. The node's own
  // flags are normalized to local, whatever the probes recorded; from within the section they
  // are its ordinary options. `Unset` when the path does not exist in the tree.
  def local(prefix: List[Text]): Optional[Help] =
    def recur(node: Help, path: List[Text], command: Text, inherited: List[Help.Param])
    :   Optional[Help] =

      path match
        case Nil =>
          // Ancestors first and nearest-root wins on a name collision; built in reverse with
          // `::` and reversed once, since deduplication needs a fold anyway.
          def dedupe(seen: List[Help.Param], param: Help.Param): List[Help.Param] =
            if seen.exists(_.name == param.name) then seen else param :: seen

          val globals: List[Help.Param] =
            inherited.map(_.copy(global = true)).fold(Nil: List[Help.Param])(dedupe)

          val all: List[Help.Param] =
            node.parameters.map(_.copy(global = false)).fold(globals)(dedupe)

          node.copy(command = command, parameters = all.reverse)

        case name :: rest =>
          node.subcommands.seek(_.command == name).let: child =>
            recur(child, rest, t"$command $name", inherited + node.parameters)

    if prefix == Nil then this else recur(this, prefix, command, Nil)

  def teletype(width: Int = Int.MaxValue)(using Hyphenation): Teletype =
    // Each `Optional` field is bound to a typed local before it is read inside a lambda; the
    // direct form trips dotc's positionless `wildApprox` assertion (scala/scala3#24824).
    val globalParams: List[Help.Param] = parameters.filter(_.global)
    val localParams: List[Help.Param] = parameters.filter(!_.global)

    val ungrouped: List[Help] = subcommands.filter: sub =>
      val group: Optional[CommandGroup] = sub.group
      group == Unset

    val groupList: List[CommandGroup] =
      subcommands.bind: sub =>
        val group: Optional[CommandGroup] = sub.group
        group.lay(Nil: List[CommandGroup])(List(_))

      . distinct

    // Wrap prose into `available` columns using escritoire's borderless paragraph layout, which
    // breaks at spaces and admissible hyphenation points. Below 20 columns, overrun instead.
    def wrap(teletype: Teletype, available: Int): List[Teletype] =
      columnar.Paragraph.fit(Array(teletype), available.max(20), TextAlignment.Left).to[List]

    // Each section is a heading (one or more unaligned lines) plus its rows, which all
    // participate in the global description-column alignment. A named method rather than a
    // lambda: `t"…"`/`e"…"` expansions inside a lambda passed to a collection combinator run
    // their implicit searches while the combinator's element type variable is still
    // uninstantiated, which trips dotc's `wildApprox` assertion (scala/scala3#24824).
    def groupSection(group: CommandGroup): (List[Teletype], List[Help.Row]) =
      val members = subcommands.filter: sub =>
        val group2: Optional[CommandGroup] = sub.group
        group2 == group

      // A flag borne identically by every member of the group is factored out of the
      // individual subcommands and listed once, as common to the whole group. Matched on
      // shape rather than counted, so no linear length is needed for the "fewer than two"
      // test.
      val common: List[Help.Param] = members match
        case first :: _ :: _ => first.parameters.filter: param =>
          members.all(_.parameters.has(param))

        case _ => Nil

      val factored: List[Help] = members.map: member =>
        member.copy(parameters = member.parameters.filter(!common.has(_)))

      val commonRows: List[Help.Row] =
        if common.nil then Nil
        else
          Help.Row.Blank :: Help.Row.Label(1, t"Common options:") :: Help.paramItems(common, 2)

      val title: Text = t"${group.name}:"

      // As above: bound before it is read (`wildApprox`).
      val description: Optional[Text | Teletype] = group.description

      val explanation: List[Teletype] = description.absolve match
        case Unset              => Nil
        case text: Text         => wrap(e"$text", width - 2)
        case teletype: Teletype => wrap(teletype, width - 2)

      // Again a named method rather than a lambda, for the `wildApprox` reason above.
      def indent(line: Teletype): Teletype = e"  $line"

      val indented: List[Teletype] = explanation.map(indent)

      val heading: List[Teletype] =
        if indented.nil then List(e"$Bold($title)") else e"$Bold($title)" :: (indented + List(e""))

      (heading, Help.compact(Help.commandRows(factored, 1) + commonRows))

    val groupSections: List[(List[Teletype], List[Help.Row])] = groupList.map(groupSection)

    // A tool without subcommands never reaches dispatch, so all of its flags count as "global";
    // presenting them as anything other than plain options would be noise. A *local* leaf view
    // is the exception: it carries ancestor flags marked global alongside its own local flags,
    // and then the distinction is worth a split — globals precede the subcommand on the command
    // line, locals follow it.
    val leaf: Boolean = subcommands.nil
    val split: Boolean = !leaf || (!globalParams.nil && !localParams.nil)

    val standard: List[(List[Teletype], List[Help.Row])] =
      if leaf && split then
        List
          ( (List(e"$Bold(Global options:)"),
             Help.compact(Help.paramItems(globalParams, 1))),
            (List(e"$Bold(Options:)"),
             Help.compact(Help.paramItems(localParams, 1))) )
      else if leaf then
        List
          ( (List(e"$Bold(Options:)"),
             Help.compact(Help.paramItems(parameters, 1))) )
      else
        List
          ( (List(e"$Bold(Global options:)"),
             Help.compact(Help.paramItems(globalParams, 1))),
            (List(e"$Bold(Options:)"),
             Help.compact(Help.paramItems(localParams, 1))),
            (List(e"$Bold(Commands:)"),
             Help.compact(Help.commandRows(ungrouped, 1))) )

    // Every status reachable anywhere in the tree, deduplicated by code and ordered by it. A
    // status is recorded against the node whose dispatch branch can return it, but an exit code
    // means the same thing wherever it comes from, so the reader wants the whole table in one
    // place rather than scattered through the command listing.
    def reachableStatuses(help: Help): List[Status] =
      help.statuses + help.subcommands.flatMap(reachableStatuses)

    // Ordering first and then dropping later repeats of a code keeps, for each code, the first
    // status in tree order — the same choice a deduplication before a stable sort would make.
    // The fold accumulates in reverse, hence the `reverse` afterwards.
    val distinctStatuses: List[Status] =
      reachableStatuses(this).order(_.code).fold(Nil: List[Status]): (seen, status) =>
        if seen.exists(_.code == status.code) then seen else status :: seen

    // Named, not a lambda, for the `wildApprox` reason noted against `groupSection` above.
    def statusRow(status: Status): Help.Row =
      Help.Row.Item(1, t"${status.code}", false, status.description)

    val statusRows: List[Help.Row] = distinctStatuses.reverse.map(statusRow)

    val statusSection: List[(List[Teletype], List[Help.Row])] =
      if statusRows.nil then Nil else List((List(e"$Bold(Exit statuses:)"), statusRows))

    val sections: List[(List[Teletype], List[Help.Row])] =
      standard.filter(!_._2.nil) + groupSections + statusSection

    // The column at which every description starts: two spaces after the widest label.
    val column: Int =
      sections.flatMap(_._2).map:
        case Help.Row.Item(depth, label, _, _) => depth*2 + label.length
        case _                                 => 0

      . maximum.or(0) + 2

    def render(row: Help.Row): List[Teletype] = row match
      case Help.Row.Blank => List(e"")

      case Help.Row.Label(depth, text) =>
        val indent: Text = t"  "*depth
        List(e"$indent$Bold($text)")

      case Help.Row.Item(depth, label, bold, description) =>
        val indent: Text = t"  "*depth

        val explanation: Optional[Teletype] = description.absolve match
          case Unset              => Unset
          case text: Text         => e"$text"
          case teletype: Teletype => teletype

        explanation.absolve match
          case Unset =>
            List(if bold then e"$indent$Bold($label)" else e"$indent$label")

          case explanation: Teletype =>
            val fitted: Text = label.fit(column - 2 - indent.length)
            val padded: Teletype = if bold then e"$indent$Bold($fitted)" else e"$indent$fitted"
            val margin: Text = t" "*column

            // A named method, not a lambda, for the `wildApprox` reason noted against
            // `groupSection` above.
            def continue(line: Teletype): Teletype = e"$margin$line"

            // Deconstructed rather than length-checked, so the head and tail are total.
            wrap(explanation, width - column) match
              case first :: rest => e"$padded  $first" :: rest.map(continue)
              case _             => List(padded)

    val usage: Teletype =
      if leaf && split then
        val globals: Text = Help.summarize(globalParams, t"global options")
        e"$Bold(Usage:) $command$globals${Help.summarize(localParams, t"options")}"
      else if leaf then
        e"$Bold(Usage:) $command${Help.summarize(parameters, t"options")}"
      else
        val globals: Text = Help.summarize(globalParams, t"global options")

        val posterior: List[Help.Param] =
          (localParams + subcommands.flatMap(Help.descendants)).distinct

        val locals: Text = Help.summarize(posterior, t"options")

        e"$Bold(Usage:) $command$globals <command>$locals"

    val header: List[Teletype] = description.absolve match
      case Unset              => List(usage)
      case text: Text         => usage :: e"" :: wrap(e"$text", width)
      case teletype: Teletype => usage :: e"" :: wrap(teletype, width)

    val body: List[Teletype] =
      header + sections.flatMap: (heading, rows) => e"" :: (heading + rows.flatMap(render))

    body.join(e"\n")
