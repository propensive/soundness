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

import anticipation.*
import denominative.nil
import aviation.*, dateFormats.iso8601DateFormat
import escapade.*
import gossamer.*
import gossamer.collationComparable
import gossamer.collations.codepoints
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*
import virility.*

import Roff.{Block, Inline}
import rudiments.sortingAlgorithms.timsort

// Renders a discovered `Help` tree, plus the static `Manual` metadata, as a manpage. The
// structure (commands, flags, operands, groups) comes entirely from the `Help` tree — the
// same one `Help.teletype` renders — so `--help` and `man` can never disagree; `Manual`
// contributes only what the tree cannot know. Styling in descriptions is stripped with
// `.plain` for now; a Teletype-to-roff font mapping can follow.
extension (help: Help)
  def roff(using manual: Manual = Manual()): Roff =
    def plain(value: Text | Teletype): Text = value.absolve match
      case text: Text         => text
      case teletype: Teletype => teletype.plain

    val leaf: Boolean = help.subcommands.nil
    val globalParams: List[Help.Param] = help.parameters.filter(_.global)
    val localParams: List[Help.Param] = help.parameters.filter(!_.global)

    // Each `Optional` field is read through a typed local rather than directly inside the
    // lambda: the direct form trips dotc's positionless `wildApprox` assertion
    // (scala/scala3#24824).
    val ungrouped: List[Help] = help.subcommands.filter: sub =>
      val group: Optional[CommandGroup] = sub.group
      group == Unset

    val groupList: List[CommandGroup] =
      help.subcommands.bind: sub =>
        val group: Optional[CommandGroup] = sub.group
        group.lay(Nil: List[CommandGroup])(List(_))

      . distinct

    def prose(value: Optional[Text | Teletype]): List[Inline] =
      value.let { value => List(Inline.Plain(plain(value))) }.or(Nil)

    def paragraph(value: Text | Teletype): List[Block] = List(Block.Paragraph(prose(value)))

    def section(title: Text, blocks: List[Block]): List[Block] =
      if blocks.nil then Nil else List(Block.Section(title, blocks))

    val name: Text =
      manual.synopsisName.or(help.description.let(plain(_))).lay(help.command): synopsis =>
        t"${help.command} - $synopsis"

    val synopsis: Text =
      if leaf then Help.summarize(help.parameters, t"options")
      else
        val globals: Text = Help.summarize(globalParams, t"global options")

        val posterior: List[Help.Param] =
          (localParams + help.subcommands.flatMap(Help.descendants)).distinct

        t"$globals <command>${Help.summarize(posterior, t"options")}"

    // The tree's description and the manual's prose, each a paragraph when present. Written as
    // two `lay`s rather than a traversal of a list of `Optional`s: that form leaves the element
    // type variable uninstantiated while the reshaping instance is searched for, which trips
    // dotc's positionless `wildApprox` assertion (scala/scala3#24824).
    val descriptionBlocks: List[Block] =
      val description: Optional[Text | Teletype] = help.description
      val summary: Optional[Text] = manual.prose

      description.lay(Nil: List[Block])(paragraph) + summary.lay(Nil: List[Block])(paragraph)

    def optionBlocks(params: List[Help.Param]): List[Block] =
      params.map: param =>
        // As above: bound before it is read (`wildApprox`).
        val description: Optional[Text | Teletype] = param.description
        Block.Tagged(List(Inline.bold(Help.label(param))), prose(description))

    // Each subcommand is a tagged entry; its own (non-global) flags and nested subcommands
    // are indented beneath it, mirroring the depth-based layout of `Help.teletype`.
    def commandBlocks(subcommands: List[Help]): List[Block] =
      subcommands.flatMap: sub =>
        val nested: List[Block] =
          optionBlocks(sub.parameters.filter(!_.global)) + commandBlocks(sub.subcommands)

        // As above: bound before it is read (`wildApprox`).
        val description: Optional[Text | Teletype] = sub.description
        val entry = Block.Tagged(List(Inline.bold(sub.command)), prose(description))

        if nested.nil then List(entry) else List(entry, Block.Indented(nested))

    val commandSections: List[Block] =
      if leaf then Nil
      else
        val grouped: List[Block] =
          groupList.map: group =>
            // As above: bound before it is read (`wildApprox`).
            val description: Optional[Text | Teletype] = group.description
            val explanation: List[Block] = description.lay(Nil: List[Block])(paragraph)

            val members = help.subcommands.filter: sub =>
              val group2: Optional[CommandGroup] = sub.group
              group2 == group

            Block.Subsection(group.name, explanation + commandBlocks(members))

        section(t"COMMANDS", commandBlocks(ungrouped) + grouped)

    val exampleBlocks: List[Block] =
      manual.examples.flatMap: example =>
        // As above: bound before it is read (`wildApprox`).
        val explanation: Optional[Text] = example.caption
        val caption: List[Block] = explanation.lay(Nil: List[Block])(paragraph)

        caption + List(Block.Example(List(example.command)))

    // Statuses and environment variables are documented once for the whole tool, so they are
    // gathered from every command in the tree rather than just its root. Folded rather than
    // `flatMap`ped: `fold` pins the element type from `initial`, whereas `flatMap` would search
    // for its reshaping instance while the recursive call's element type was still
    // uninstantiated, tripping dotc's `wildApprox` assertion (scala/scala3#24824).
    def gather[element](node: Help)(select: Help => List[element]): List[element] =
      node.subcommands.fold(select(node)): (gathered, sub) => gathered + gather(sub)(select)

    // A status discovered from an `execute` block and one declared in the `Manual` may describe
    // the same code; the hand-written description wins.
    val exitStatusBlocks: List[Block] =
      val declared: Map[Int, Text] =
        manual.exitStatuses.map({ status => status.code -> status.description }).to[Map]

      val detected: List[(Int, Text)] =
        gather(help)(_.statuses).map: status => status.code -> status.description

      // A named method, not a lambda: the `show` expansion inside a lambda passed to a
      // collection combinator would run its implicit search while the combinator's element
      // type variable is still uninstantiated (`wildApprox`).
      def statusBlock(code: Int, description: Text): Block =
        Block.Tagged(List(Inline.bold(code.show)), prose(description))

      val combined: List[(Int, Text)] = detected + declared.to[List]

      val resolved: List[(Int, Text)] =
        combined.map: (code, description) => code -> declared.at(code).or(description)

      // Ordering before dropping later repeats of a code keeps, for each code, the first entry
      // in the concatenated order — the same choice a deduplication before a stable sort makes.
      // The fold accumulates in reverse, hence the `reverse` afterwards.
      val deduplicated: List[(Int, Text)] =
        resolved.order(_._1).fold(Nil: List[(Int, Text)]): (seen, entry) =>
          if seen.exists(_._1 == entry._1) then seen else entry :: seen

      deduplicated.reverse.map: (code, description) => statusBlock(code, description)

    val environmentBlocks: List[Block] =
      val described: Map[Text, Text] =
        manual.environment.map({ variable => variable.name -> variable.description }).to[Map]

      val names: List[Text] =
        (gather(help)(_.variables) + described.keys.to[List]).distinct.sort

      names.map: name => Block.Tagged(List(Inline.bold(name)), prose(described.at(name)))

    val fileBlocks: List[Block] =
      manual.files.map: file =>
        Block.Tagged(List(Inline.italic(file.path)), prose(file.description))

    val authorBlocks: List[Block] =
      manual.authors.map: author => Block.Paragraph(List(Inline.Plain(author)))

    val bugsBlocks: List[Block] =
      val bugs: Optional[Text] = manual.bugs
      bugs.lay(Nil: List[Block])(paragraph)

    val seeAlsoBlocks: List[Block] =
      // Named methods, not lambdas, for the `wildApprox` reason noted above.
      def cite(reference: Manual.Reference, first: Boolean): List[Inline] =
        val separator: List[Inline] = if first then Nil else List(Inline.Plain(t", "))
        separator + List(Inline.bold(reference.name), Inline.Plain(t"(${reference.section})"))

      // Deconstructed rather than indexed, so only the first reference goes unseparated.
      val references: List[Inline] = manual.seeAlso match
        case first :: rest => cite(first, true) + rest.flatMap(cite(_, false))
        case _             => Nil

      val homepage: List[Block] =
        manual.homepage.lay(Nil: List[Block]): url =>
          List(Block.Paragraph(List(Inline.Plain(t"Homepage: ${url.show}"))))

      val referenceBlocks: List[Block] =
        if references.nil then Nil else List(Block.Paragraph(references))

      referenceBlocks + homepage

    val nameSection =
      Block.Section(t"NAME", List(Block.Paragraph(List(Inline.Plain(name)))))

    val synopsisSection =
      Block.Section
        ( t"SYNOPSIS",
          List(Block.Paragraph(List(Inline.bold(help.command), Inline.Plain(synopsis)))) )

    val optionSections: List[Block] =
      if leaf then section(t"OPTIONS", optionBlocks(help.parameters))
      else
        section(t"GLOBAL OPTIONS", optionBlocks(globalParams)) +
          section(t"OPTIONS", optionBlocks(localParams))

    val blocks: List[Block] =
      List
        ( List(nameSection, synopsisSection),
          section(t"DESCRIPTION", descriptionBlocks),
          optionSections,
          commandSections,
          section(t"EXAMPLES", exampleBlocks),
          section(t"EXIT STATUS", exitStatusBlocks),
          section(t"ENVIRONMENT", environmentBlocks),
          section(t"FILES", fileBlocks),
          section(t"AUTHORS", authorBlocks),
          section(t"BUGS", bugsBlocks),
          section(t"SEE ALSO", seeAlsoBlocks) )

      . flat

    Roff
      ( help.command,
        manual.section.number,
        manual.date.let(_.show),
        manual.version.let { version => t"${help.command} ${version.show}" },
        manual.section.title,
        blocks )
