package caesura

import fulminate.*

object DsvError:
  given Reason is Communicable =
    case Reason.MisplacedQuote => m"a quote was found after the start of a cell"

  enum Reason:
    case MisplacedQuote

case class DsvError(format: DsvFormat, reason: DsvError.Reason)
extends Error(m"Could not parse row data because $reason")
