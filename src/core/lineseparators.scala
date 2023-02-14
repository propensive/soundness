package turbulence

package lineSeparators:
  given carriageReturn: LineSeparators(Cr, Nl, Skip, Nl, Nl)
  given strictCarriageReturn: LineSeparators(Cr, Nl, Lf, NlLf, LfNl)
  given linefeed: LineSeparators(Lf, Skip, Nl, Nl, Nl)
  given strictLinefeeds: LineSeparators(Lf, Nl, Lf, NlLf, LfNl)
  given carriageReturnLinefeed: LineSeparators(CrLf, Skip, Lf, Nl, LfNl)
  given adaptiveLinefeed: LineSeparators(Lf, Nl, Nl, Nl, Nl)
  
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
        Skip
    
    action match
      case Nl   => mkNewline
      case NlCr => mkNewline; put(13)
      case NlLf => mkNewline; put(10)
      case CrNl => put(13); mkNewline
      case NlNl => mkNewline; mkNewline
      case Cr   => put(13)
      case Lf   => put(10)
        
  enum Action:
    case Nl, NlCr, NlLf, LfNl, CrNl, NlNl, Cr, Lf, Skip

case class LineSeparators(newline: Action.Cr | Action.Lf | Action.CrLf | Action.LfCr, cr: Action, lf: Action,
                              crlf: Action, lfcr: Action):
  def process(seq: LineSeparators.NewlineSeq): LineSeparators.NewlineAction
  
  def newlineBytes = newline match
    case Action.Cr   => Bytes(13)
    case Action.Lf   => Bytes(10)
    case Action.CrLf => Bytes(13, 10)
    case Action.LfCr => Bytes(10, 13)
