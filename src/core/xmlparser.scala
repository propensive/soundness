package xylophone

import contextual.*
import rudiments.*

enum Input:
  case StringLike(str: String)
  case XmlLike(xml: Ast.Element)

enum ContextType:
  case AttributeValue, InTagName, SelfClosingTagName, TagClose, ClosingTag, InAttributeName,
      InTagBody, AttributeEquals, Body, InBodyEntity, InAttributeEntity

case class CharExtractor(chars: Set[Char]):
  val charSet = chars.to(Set)
  def unapply(char: Char): Boolean = charSet.contains(char)

case class ParseState(offset: Int, context: ContextType, stack: List[String], current: String,
                          source: String):
  def apply(newContext: ContextType, char: Char) =
    copy(context = newContext, current = current + char, offset = offset + 1)
  
  def apply(newContext: ContextType) = copy(context = newContext, offset = offset + 1)
  def apply(char: Char) = copy(offset = offset + 1, current = current + char)
  def apply() = copy(offset = offset + 1)
  def reset() = copy(current = "")
  def push() = copy(stack = current :: stack, current = "")
  def rollback(difference: Int) = copy(offset = offset - difference)
  
  def pop() = stack.headOption match
    case Some(`current`) => copy(stack = stack.tail)
    case Some(tag)       => throw InterpolationError(s"closing tag '$current' does not match expected tag '$tag'", offset - current.length, current.length)
    case None            => throw InterpolationError(s"spurious closing tag: $current", offset - current.length, current.length)

given Insertion[Input, String] = Input.StringLike(_)

given genInsert[T](using writer: XmlWriter[T]): Insertion[Input, T] =
  value => Input.XmlLike(writer.write(value))

object XmlInterpolator extends Interpolator[Input, ParseState, Doc]:
  import ContextType.*
  val Letters = ('a' to 'z').to(Set) ++ ('A' to 'Z').to(Set)
  val Digits = ('0' to '9').to(Set)
  val TagChar = CharExtractor(Letters ++ Digits + '.' + '-' + '_')
  val WhitespaceChar = CharExtractor(Set(' ', '\t', '\n', '\r'))

  def initial: ParseState = ParseState(0, ContextType.Body, Nil, "", "")

  private def escape(str: String): String =
    str.replaceAll("\"", "&quot;").nn
      .replaceAll("'", "&apos;").nn
      .replaceAll("<", "&lt;").nn
      .replaceAll(">", "&gt;").nn
      .replaceAll("&", "&amp;").nn

  def insert(state: ParseState, value: Option[Input]): ParseState =
    state.context match
      case AttributeValue | Body =>
        parse(state, value.fold("") {
          case Input.StringLike(str) => escape(str)
          case Input.XmlLike(xml)    => xml.toString
        })
      case AttributeEquals =>
        parse(state, value.fold("""""""") {
          case Input.StringLike(str) => "\""+escape(str)+"\""
          case Input.XmlLike(xml)    => "\""+escape(xml.toString)+"\""
        })
      case _ => throw InterpolationError(s"a substitution cannot be made in this position")
  
  def complete(state: ParseState): Doc =
    if state.stack.nonEmpty then throw InterpolationError(s"expected closing tag: ${state.stack.head}")
    Xml.parse(state.source)

  def parse(state: ParseState, string: String): ParseState = string.foldLeft(state.copy(offset = 0)) {
    case (state@ParseState(_, _, _, _, _), char) => state.context match
      
      case InTagName          => char match
        case TagChar()          => state(char)
        case WhitespaceChar()   => state.push()(InTagBody).reset()
        case ':'                => state(char) // FIXME: Namespaces
        case '/'                => if state.current.isEmpty then state(ClosingTag)
                                   else state(SelfClosingTagName)
        case '>'                => state.push()(Body)
        case _                  => throw InterpolationError("not a valid tag name character", state.offset, 1)
      
      case SelfClosingTagName => char match
        case TagChar()          => state(char)
        case ':'                => state(char) // FIXME: Namespaces
        case '>'                => state(Body)
        case _                  => throw InterpolationError("expected '>'", state.offset, 1)
      
      case ClosingTag         => char match
        case TagChar()          => state(char)
        case ':'                => state(char) // FIXME: Namespaces
        case '>'                => state.pop()(Body)
        case WhitespaceChar()   => state()
        case _                  => throw InterpolationError("expected '>' or whitespace", state.offset, 1)

      case InAttributeName    => char match
        case TagChar()          => state(char)
        case WhitespaceChar()   => state(InAttributeName)
        case '>'                => throw InterpolationError("attribute value has not been specified", state.offset, 1)
        case '='                => state(AttributeEquals).reset()
        case ':'                => state(char) // FIXME: Namespaces
        case ch                 => throw InterpolationError(s"character '$ch' is not valid in an attribute name", state.offset, 1)
      
      case AttributeEquals    => char match
        case WhitespaceChar()   => state()
        case '"'                => state(AttributeValue)
        case _                  => throw InterpolationError("expected '\"'", state.offset, 1)
      
      case AttributeValue     => char match
        case '"'                => state(InTagBody).reset()
        case '&'                => state(InAttributeEntity)
        case char               => state(char)
      
      case InTagBody          => char match
        case WhitespaceChar()   => state(InTagBody)
        case TagChar()          => state(InAttributeName, char)
        case '>'                => state(Body)
        case '/'                => state(TagClose)
        case ch                 => throw InterpolationError(s"character '$ch' is not permitted in a tag name", state.offset)

      case TagClose           => char match
        case '>'                => state(Body)
        case _                  => throw InterpolationError("expected '>'", state.offset, 1)
      
      case Body               => char match
        case '<'                => state(InTagName).reset()
        case '&'                => state(InBodyEntity).reset()
        case _                  => state()
      
      case InBodyEntity       => char match
        case ';'                => state()
        case ch                 => throw InterpolationError(s"character '$ch' is not valid in an entity name", state.offset, 1)

      case InAttributeEntity  => char match
        case ';'                => state()
        case TagChar()          => state()
        case ch                 => throw InterpolationError(s"character '$ch' is not valid in an entity name", state.offset, 1)
  }.copy(source = state.source+string)