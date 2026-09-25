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
package cataclysm

import anticipation.*
import gossamer.*
import monotonous.*
import monotonous.alphabets.base64Standard
import phoenicia.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// Renders a font's provision as CSS at-rules. An embedded font becomes a `@font-face` whose
// `src` is a base-64 data URI, so the stylesheet — or the SVG carrying it — is self-contained;
// a linked or local font becomes a `@font-face` naming it; a stylesheet becomes an `@import`;
// a generic family needs nothing.
object FontFace:
  def rules(font: Font): List[Css.Node] = sources(font.typeface.name, font.provision)

  // A stylesheet of the rules for several fonts, each typeface once, `@import`s first.
  def stylesheet[font <: Font](fonts: List[font]): Css =
    def recur(remaining: List[font], seen: List[Text]): List[Css.Node] = remaining match
      case head :: tail =>
        val name = head.typeface.name
        if seen.has(name) then recur(tail, seen) else rules(head) + recur(tail, List(name) + seen)

      case _ =>
        Nil

    val nodes = recur(fonts, Nil)
    Css(select(nodes, imported = true) + select(nodes, imported = false))

  private def select(nodes: List[Css.Node], imported: Boolean): List[Css.Node] = nodes match
    case head :: tail =>
      val isImport = head match
        case Css.Node.At(t"import", _, _) => true
        case _                            => false

      if isImport == imported then List(head) + select(tail, imported) else select(tail, imported)

    case _ =>
      Nil

  private def sources(typeface: Text, provision: Typesettable): List[Css.Node] =
    def recur(remaining: List[Typesettable.Source]): List[Css.Node] = remaining match
      case head :: tail => source(typeface, head, provision.coverage) + recur(tail)
      case _            => Nil

    recur(provision.sources)

  private def source(typeface: Text, source: Typesettable.Source, coverage: Coverage)
  :   List[Css.Node] =

    source match
      case Typesettable.Source.Embedded(sfnt) =>
        val (mime, format) = sfnt match
          case _: Opentype => (t"font/otf", t"opentype")
          case _           => (t"font/ttf", t"truetype")

        val data = sfnt.data.serialize[Base64]
        val src = t"url(data:$mime;base64,$data) format(\"$format\")"
        List(fontFace(typeface, src, Coverage.of(sfnt)))

      case Typesettable.Source.Linked(url, format) =>
        val hint = format.lay(t""): format =>
          val name = format match
            case Typesettable.Source.Format.Ttf   => t"truetype"
            case Typesettable.Source.Format.Otf   => t"opentype"
            case Typesettable.Source.Format.Woff  => t"woff"
            case Typesettable.Source.Format.Woff2 => t"woff2"

          t" format(\"$name\")"

        List(fontFace(typeface, t"url(\"$url\")$hint", coverage))

      case Typesettable.Source.Local(name) =>
        List(fontFace(typeface, t"local(\"$name\")", coverage))

      case Typesettable.Source.Imported(url) =>
        List(Css.Node.At(t"import", t"url(\"$url\")", Unset))

      case Typesettable.Source.Generic =>
        Nil

  private def fontFace(typeface: Text, src: Text, coverage: Coverage): Css.Node =
    val declarations =
      List
        ( Css.Node.Declaration(t"font-family", t"\"$typeface\""),
          Css.Node.Declaration(t"src", src) )

    Css.Node.At(t"font-face", t"", declarations + descriptors(coverage))

  // The faces a `@font-face` covers, as its `font-weight`, `font-style` and `font-stretch`
  // descriptors, merged across the coverage's entries. A browser matches a requested face
  // against these, so they must span what the file has.
  private def descriptors(coverage: Coverage): List[Css.Node] = coverage match
    case Coverage.Unknown => Nil

    case Coverage.Entries(entries) => span(entries).lay(Nil): span =>
      val weight =
        if span.weights.lower == span.weights.upper then span.weights.lower.show
        else t"${span.weights.lower.show} ${span.weights.upper.show}"

      val style =
        if span.italic == span.upright then Nil
        else List(Css.Node.Declaration(t"font-style", if span.italic then t"italic" else t"normal"))

      val stretch =
        if span.stretches.lower == Stretch.Normal && span.stretches.upper == Stretch.Normal then Nil
        else
          val range = t"${span.stretches.lower.show} ${span.stretches.upper.show}"
          List(Css.Node.Declaration(t"font-stretch", range))

      List(Css.Node.Declaration(t"font-weight", weight)) + style + stretch

  private case class Span
    ( weights: Weight.Range, upright: Boolean, italic: Boolean, stretches: Stretch.Range )

  private def span(entries: List[Coverage.Entry]): Optional[Span] = entries match
    case head :: tail =>
      val first = Span(head.weights, head.upright, head.italic, head.stretches)

      span(tail).lay(first): rest =>
        val lower = Weight(first.weights.lower.value.min(rest.weights.lower.value))
        val upper = Weight(first.weights.upper.value.max(rest.weights.upper.value))
        val narrow = Stretch(first.stretches.lower.percentage.min(rest.stretches.lower.percentage))
        val wide = Stretch(first.stretches.upper.percentage.max(rest.stretches.upper.percentage))

        Span
          ( Weight.Range(lower, upper), first.upright || rest.upright, first.italic || rest.italic,
            Stretch.Range(narrow, wide) )

    case _ =>
      Unset
