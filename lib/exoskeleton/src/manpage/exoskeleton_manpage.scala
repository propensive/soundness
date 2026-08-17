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
import aviation.*, dateFormats.iso8601DateFormat
import escapade.*
import gossamer.*
import spectacular.*
import vacuous.*
import virility.*

import Roff.{Block, Inline}

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

    val leaf: Boolean = help.subcommands.stdlib.isEmpty
    val globalParams: scala.List[Help.Param] = help.parameters.stdlib.filter(_.global)
    val localParams: scala.List[Help.Param] = help.parameters.stdlib.filter(!_.global)
    val ungrouped: scala.List[Help] = help.subcommands.stdlib.filter(_.group == Unset)

    val groupList: scala.List[CommandGroup] =
      help.subcommands.stdlib.flatMap(_.group.option).distinct

    def prose(value: Optional[Text | Teletype]): List[Inline] =
      value.let { value => List(Inline.Plain(plain(value))) }.or(Nil)

    def section(title: Text, blocks: scala.List[Block]): scala.List[Block] =
      if blocks.isEmpty then scala.List() else scala.List(Block.Section(title, List.of(blocks)))

    val name: Text =
      manual.synopsisName.or(help.description.let(plain(_))).lay(help.command): synopsis =>
        t"${help.command} - $synopsis"

    val synopsis: Text =
      if leaf then Help.summarize(help.parameters.stdlib, t"options")
      else
        val globals: Text = Help.summarize(globalParams, t"global options")

        val posterior =
          (localParams ::: help.subcommands.stdlib.flatMap(Help.descendants)).distinct

        t"$globals <command>${Help.summarize(posterior, t"options")}"

    val descriptionBlocks: scala.List[Block] =
      scala.List(help.description, manual.prose).flatMap(_.option).map: value =>
        Block.Paragraph(prose(value))

    def optionBlocks(params: scala.List[Help.Param]): scala.List[Block] =
      params.map: param =>
        Block.Tagged(List(Inline.bold(Help.label(param))), prose(param.description))

    // Each subcommand is a tagged entry; its own (non-global) flags and nested subcommands
    // are indented beneath it, mirroring the depth-based layout of `Help.teletype`.
    def commandBlocks(subcommands: scala.List[Help]): scala.List[Block] =
      subcommands.flatMap: sub =>
        val nested: scala.List[Block] =
          optionBlocks(sub.parameters.stdlib.filter(!_.global)) :::
            commandBlocks(sub.subcommands.stdlib)

        val entry = Block.Tagged(List(Inline.bold(sub.command)), prose(sub.description))

        if nested.isEmpty then scala.List(entry)
        else scala.List(entry, Block.Indented(List.of(nested)))

    val commandSections: scala.List[Block] =
      if leaf then scala.List()
      else
        val grouped: scala.List[Block] =
          groupList.map: group =>
            val explanation: scala.List[Block] =
              group.description.option.map { value => Block.Paragraph(prose(value)) }.toList

            val members = help.subcommands.stdlib.filter(_.group == group)
            Block.Subsection(group.name, List.of(explanation ::: commandBlocks(members)))

        section(t"COMMANDS", commandBlocks(ungrouped) ::: grouped)

    val exampleBlocks: scala.List[Block] =
      manual.examples.stdlib.flatMap: example =>
        val caption: scala.List[Block] =
          example.caption.option.map { value => Block.Paragraph(prose(value)) }.toList

        caption ::: scala.List(Block.Example(List(example.command)))

    // Statuses and environment variables are documented once for the whole tool, so they are
    // gathered from every command in the tree rather than just its root.
    def gather[element](node: Help)(select: Help => List[element]): scala.List[element] =
      select(node).stdlib ::: node.subcommands.stdlib.flatMap(gather(_)(select))

    // A status discovered from an `execute` block and one declared in the `Manual` may describe
    // the same code; the hand-written description wins.
    val exitStatusBlocks: scala.List[Block] =
      val declared: scala.collection.Map[Int, Text] =
        manual.exitStatuses.stdlib.map { status => status.code -> status.description }.toMap

      val detected: scala.List[(Int, Text)] =
        gather(help)(_.statuses).map { status => status.code -> status.description }

      (detected ::: declared.toList)
      . map { (code, description) => code -> declared.getOrElse(code, description) }
      . distinctBy(_._1)
      . sortBy(_._1)
      . map: (code, description) =>
          Block.Tagged(List(Inline.bold(code.show)), prose(description))

    val environmentBlocks: scala.List[Block] =
      val described: scala.collection.Map[Text, Text] =
        manual.environment.stdlib.map { variable => variable.name -> variable.description }.toMap

      (gather(help)(_.variables) ::: described.keys.toList).distinct.sorted.map: name =>
        Block.Tagged(List(Inline.bold(name)), prose(described.get(name).optional))

    val fileBlocks: scala.List[Block] =
      manual.files.stdlib.map: file =>
        Block.Tagged(List(Inline.italic(file.path)), prose(file.description))

    val authorBlocks: scala.List[Block] =
      manual.authors.stdlib.map: author => Block.Paragraph(List(Inline.Plain(author)))

    val bugsBlocks: scala.List[Block] =
      manual.bugs.option.map { bugs => Block.Paragraph(prose(bugs)) }.toList

    val seeAlsoBlocks: scala.List[Block] =
      val references: scala.List[Inline] =
        manual.seeAlso.stdlib.zipWithIndex.flatMap: (reference, index) =>
          val separator: scala.List[Inline] =
            if index == 0 then scala.List() else scala.List(Inline.Plain(t", "))

          separator :::
            scala.List(Inline.bold(reference.name), Inline.Plain(t"(${reference.section})"))

      val homepage: scala.List[Block] =
        manual.homepage.option.map: url =>
          Block.Paragraph(List(Inline.Plain(t"Homepage: ${url.show}")))

        . toList

      val referenceBlocks: scala.List[Block] =
        if references.isEmpty then scala.List()
        else scala.List(Block.Paragraph(List.of(references)))

      referenceBlocks ::: homepage

    val nameSection =
      Block.Section(t"NAME", List(Block.Paragraph(List(Inline.Plain(name)))))

    val synopsisSection =
      Block.Section
        ( t"SYNOPSIS",
          List(Block.Paragraph(List(Inline.bold(help.command), Inline.Plain(synopsis)))) )

    val optionSections: scala.List[Block] =
      if leaf then section(t"OPTIONS", optionBlocks(help.parameters.stdlib))
      else
        section(t"GLOBAL OPTIONS", optionBlocks(globalParams)) :::
          section(t"OPTIONS", optionBlocks(localParams))

    val blocks: scala.List[Block] =
      scala.List
        ( scala.List(nameSection, synopsisSection),
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

      . flatten

    Roff
      ( help.command,
        manual.section.number,
        manual.date.let(_.show),
        manual.version.let { version => t"${help.command} ${version.show}" },
        manual.section.title,
        List.of(blocks) )
