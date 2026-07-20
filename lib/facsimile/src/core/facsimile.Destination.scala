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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

object Destination:
  // As an extension rather than a base-class method, since every case declares a `page`
  // field of its own, which could not otherwise share the name.
  extension (destination: Destination)
    def page: Ordinal = destination match
      case Xyz(page, _, _, _)    => page
      case Fit(page)             => page
      case FitWidth(page, _)     => page
      case FitHeight(page, _)    => page
      case FitRect(page, _)      => page
      case FitBox(page)          => page
      case FitBoxWidth(page, _)  => page
      case FitBoxHeight(page, _) => page

  // An explicit destination array (ISO 32000-2 §12.3.2.3), a dictionary wrapping one at
  // `/D`, or a name/string to look up among the document's named destinations. `named` is
  // consulted at most once, so a self-referential name cannot loop.
  private[facsimile] def read
    ( value: Cos,
      pages: Map[Int, Ordinal],
      named: Text => Optional[Cos],
      following: Boolean = false )
    ( using pdf: Pdf )
  :   Optional[Destination] raises PdfError =

    pdf.resolved(value) match
      case Cos.Sequence(target :: Cos.Name(kind) :: rest) =>
        val page: Optional[Ordinal] = target match
          case Cos.Ref(number, _)  => pages.at(number)
          case Cos.Integral(index) => index.toInt.z
          case _                   => Unset

        def dimension(index: Int): Optional[Double] =
          if index < rest.stdlib.length then pdf.resolved(rest.stdlib(index)).double else Unset

        page.let: page =>
          kind.s match
            case "XYZ"   => Xyz(page, dimension(0), dimension(1), dimension(2))
            case "Fit"   => Fit(page)
            case "FitH"  => FitWidth(page, dimension(0))
            case "FitV"  => FitHeight(page, dimension(0))
            case "FitB"  => FitBox(page)
            case "FitBH" => FitBoxWidth(page, dimension(0))
            case "FitBV" => FitBoxHeight(page, dimension(0))

            case "FitR" =>
              PdfRect.read(Cos.Sequence(rest), 1.0).let(FitRect(page, _))

            case _ =>
              Unset

      case dictionary: Cos.Dictionary =>
        dictionary(t"D").let(read(_, pages, named, following))

      case other =>
        if following then Unset else
          val key = other match
            case Cos.Name(name) => name
            case cos            => cos.text

          key.let(named(_)).let(read(_, pages, named, following = true))

// Where a link, bookmark or navigation action lands: a page — by position in the flattened
// page sequence — and how the viewer should fit it.
enum Destination:
  case Xyz(page: Ordinal, left: Optional[Double], top: Optional[Double], zoom: Optional[Double])
  case Fit(page: Ordinal)
  case FitWidth(page: Ordinal, top: Optional[Double])
  case FitHeight(page: Ordinal, left: Optional[Double])
  case FitRect(page: Ordinal, rect: PdfRect)
  case FitBox(page: Ordinal)
  case FitBoxWidth(page: Ordinal, top: Optional[Double])
  case FitBoxHeight(page: Ordinal, left: Optional[Double])
