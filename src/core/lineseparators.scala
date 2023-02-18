package turbulence

import rudiments.*

package lineSeparation:
  import LineSeparators.Action.*
  import LineSeparators.NewlineSeq
  given carriageReturn: LineSeparators(NewlineSeq.Cr, Nl, Skip, Nl, Nl)
  given strictCarriageReturn: LineSeparators(NewlineSeq.Cr, Nl, Lf, NlLf, LfNl)
  given linefeed: LineSeparators(NewlineSeq.Lf, Skip, Nl, Nl, Nl)
  given strictLinefeeds: LineSeparators(NewlineSeq.Lf, Nl, Lf, NlLf, LfNl)
  given carriageReturnLinefeed: LineSeparators(NewlineSeq.CrLf, Skip, Lf, Nl, LfNl)
  given adaptiveLinefeed: LineSeparators(NewlineSeq.Lf, Nl, Nl, Nl, Nl)
  
  given jvm: LineSeparators = System.lineSeparator.nn match
    case "\r\n" => carriageReturnLinefeed
    case "\r"   => carriageReturn
    case "\n"   => linefeed
    case _      => adaptiveLinefeed

object LineSeparators:
  inline def readByte(inline read: => Byte, next: => Byte, inline mkNewline: => Unit, inline put: Byte => Unit)
                     (lineSeparators: LineSeparators): Unit =
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

  enum NewlineSeq:
    case Cr, Lf, CrLf, LfCr

  enum Action:
    case Nl, NlCr, NlLf, LfNl, CrNl, NlNl, Cr, Lf, Skip

case class LineSeparators(newline: LineSeparators.NewlineSeq, cr: LineSeparators.Action,
                              lf: LineSeparators.Action, crlf: LineSeparators.Action,
                              lfcr: LineSeparators.Action):
  def process(seq: LineSeparators.NewlineSeq): LineSeparators.Action
  
  def newlineBytes = newline match
    case LineSeparators.NewlineSeq.Cr   => Bytes(13)
    case LineSeparators.NewlineSeq.Lf   => Bytes(10)
    case LineSeparators.NewlineSeq.CrLf => Bytes(13, 10)
    case LineSeparators.NewlineSeq.LfCr => Bytes(10, 13)
