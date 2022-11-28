package cellulose

import rudiments.*
import gossamer.*

export CodlParseError.Issue.*
export CodlValidationError.Issue.*

import language.experimental.captureChecking

sealed trait CodlError extends Exception

object CodlParseError:
  given Show[Issue] =
    case UnexpectedCarriageReturn =>
      t"a carriage return character ('\\r') was followed by a character other than a newline ('\\n')"
    case CarriageReturnMismatch(true) =>
      txt"""a newline character ('\\n') was found without a preceding carriage return ('\\r'), which does not
            match the document's prior newline convention"""
    case CarriageReturnMismatch(false) =>
      t"a carriage return ('\\r') was encountered, which does not match the document's prior newline convention"
    case UnevenIndent(initial, indent) =>
      t"the indentation level of $indent (with a margin of $initial) is not an exact multiple of 2"
    case IndentAfterComment =>
      t"indentation was given after a comment; the comment should be aligned with its next key"
    case SurplusIndent =>
      t"too much indentation was given"
    case InsufficientIndent =>
      t"insufficient indentation was specified"
  
  enum Issue:
    case UnexpectedCarriageReturn
    case CarriageReturnMismatch(required: Boolean)
    case UnevenIndent(initial: Int, indent: Int)
    case IndentAfterComment, SurplusIndent, InsufficientIndent

case class BinaryError(expectation: Text, pos: Int)
extends Exception(s"expected $expectation at position $pos")

case class CodlParseError(line: Int, col: Int, issue: CodlParseError.Issue)
extends Error(err"could not parse CoDL document at $line:$col: ${issue.show}"), CodlError

object CodlValidationError:
  object Issue:
    given Show[Issue] =
      case MissingKey(key)    => t"the value $key was missing"
      case DuplicateKey(key)  => t"the unique key $key has already been used"
      case SurplusParams(key) => t"too many parameters were given to the key $key"
      case InvalidKey(key)    => t"the key $key was invalid"
      case DuplicateId(id)    => t"the id $id has been used more than once"

  enum Issue:
    case MissingKey(key: Text)
    case DuplicateKey(key: Text)
    case SurplusParams(cmd: Text)
    case InvalidKey(key: Text)
    case DuplicateId(id: Text)

case class CodlValidationError(word: Maybe[Text], issue: CodlValidationError.Issue)
extends Error(err"the CoDL document did not conform to the schema at $word because ${issue.show}"), CodlError

case class MultipleIdentifiersError(key: Text)
extends Exception(s"multiple parameters of $key have been marked as identifiers")

case class MissingValueError(key: Text)
extends Error(err"the key $key does not exist in the CoDL document")

case class MissingIndexValueError(index: Int)
extends Error(err"the index $index does not exist in the CoDL document")

case class AggregateError(errors: List[CodlError])
extends Error(err"aggregation of errors: ${errors.map(_.toString.show).join.s}")
