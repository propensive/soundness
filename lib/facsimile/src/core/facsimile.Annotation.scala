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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

object Annotation:
  extension (annotation: Annotation)
    def rect: Pdf.Rect = annotation match
      case Link(rect, _, _, _)  => rect
      case Note(rect, _, _, _)  => rect
      case Widget(rect, _, _)   => rect
      case Other(_, rect, _)    => rect

    def dictionary: Map[Text, Cos] = annotation match
      case Link(_, _, _, dictionary)  => dictionary
      case Note(_, _, _, dictionary)  => dictionary
      case Widget(_, _, dictionary)   => dictionary
      case Other(_, _, dictionary)    => dictionary

  private[facsimile] def read
    ( value: Cos, pages: Map[Int, Ordinal], named: Text => Optional[Cos], scale: Double )
    ( using pdf: Pdf )
    ( using Tactic[Pdf.Error] )
  :   Optional[Annotation] =

    pdf.resolved(value) match
      case Cos.Dictionary(entries) =>
        Pdf.Rect.read(entries(t"Rect").or(Cos.Nil), scale).let: rect =>
          val action = pdf.resolved(entries(t"A").or(Cos.Nil))
          val kind = action(t"S").let(_.name).or(t"")

          entries(t"Subtype").let(pdf.resolved(_).name).or(t"") match
            case t"Link" =>
              val target = entries(t"Dest")
                . or(if kind == t"GoTo" then action(t"D") else Unset)

              val uri =
                if kind == t"URI" then action(t"URI").let(pdf.resolved(_).text) else Unset

              Link(rect, target.let(Destination.read(_, pages, named)), uri, entries)

            case t"Text" =>
              Note
                ( rect,
                  entries(t"Contents").let(pdf.resolved(_).text),
                  entries(t"Open").let(pdf.resolved(_).truth).or(false),
                  entries )

            case t"Widget" =>
              Widget(rect, entries(t"T").let(pdf.resolved(_).text), entries)

            case subtype =>
              Other(subtype, rect, entries)

      case _ =>
        Unset

// A page annotation (ISO 32000-2 §12.5): links, sticky notes and form widgets as typed
// cases, and the remaining twenty-odd subtypes as `Other`, always with the raw dictionary
// as an escape hatch.
enum Annotation:
  case Link
    ( rect:        Pdf.Rect,
      destination: Optional[Destination],
      uri:         Optional[Text],
      dictionary:  Map[Text, Cos] )

  case Note
    ( rect:       Pdf.Rect,
      contents:   Optional[Text],
      open:       Boolean,
      dictionary: Map[Text, Cos] )

  case Widget(rect: Pdf.Rect, fieldName: Optional[Text], dictionary: Map[Text, Cos])
  case Other(subtype: Text, rect: Pdf.Rect, dictionary: Map[Text, Cos])
