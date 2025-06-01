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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package turbulence

import anticipation.*

object LineSeparation:
  inline def readByte
              (inline read:      => Byte,
                      next:      => Unit,
               inline mkNewline: => Unit,
               inline put: Byte  => Unit)
              (lineSeparators: LineSeparation)
  : Unit =

      val action: Action = read match
        case 10 =>
          next

          read match
            case 13 => next; lineSeparators.lfcr
            case ch => lineSeparators.lf

        case 13 =>
          next

          read match
            case 10 => next; lineSeparators.crlf
            case ch => lineSeparators.cr

        case ch =>
          put(ch)
          Action.Skip

      action match
        case Action.Nl   => mkNewline
        case Action.NlCr => mkNewline; put(13)
        case Action.NlLf => mkNewline; put(10)
        case Action.CrNl => put(13); mkNewline
        case Action.NlNl => mkNewline; mkNewline
        case Action.Cr   => put(13)
        case Action.Lf   => put(10)
        case Action.LfNl => put(10); mkNewline
        case Action.Skip => ()

  enum NewlineSeq:
    case Cr, Lf, CrLf, LfCr

    def text: Text = this match
      case Cr   => "\r".tt
      case Lf   => "\n".tt
      case CrLf => "\r\n".tt
      case LfCr => "\n\r".tt

  enum Action:
    case Nl, NlCr, NlLf, LfNl, CrNl, NlNl, Cr, Lf, Skip

case class LineSeparation
   (newline: LineSeparation.NewlineSeq,
    cr:      LineSeparation.Action,
    lf:      LineSeparation.Action,
    crlf:    LineSeparation.Action,
    lfcr:    LineSeparation.Action):

  def newlineBytes = newline match
    case LineSeparation.NewlineSeq.Cr   => Bytes(13)
    case LineSeparation.NewlineSeq.Lf   => Bytes(10)
    case LineSeparation.NewlineSeq.CrLf => Bytes(13, 10)
    case LineSeparation.NewlineSeq.LfCr => Bytes(10, 13)
