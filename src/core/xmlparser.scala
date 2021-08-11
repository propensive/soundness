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
    case Some(tag)       => throw ParseError(s"closing tag '$current' does not match expected tag '$tag'")
    case None            => throw ParseError(s"spurious closing tag: $current")

given Insertion[Input, String] = Input.StringLike(_)

given [T](using writer: XmlWriter[? >: T]): Insertion[Input, T] =
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
      case _ => throw ParseError(s"a substitution cannot be made in this position")
  
  def complete(state: ParseState): Doc =
    if state.stack.nonEmpty then throw ParseError(s"expected closing tag: ${state.stack.head}")
    Xml.parse(state.source)

  def parse(state: ParseState, string: String): ParseState = string.foldLeft(state) {
    case (state@ParseState(_, _, _, _, _), char) => state.context match
      
      case InTagName          => char match
        case TagChar()          => state(char)
        case WhitespaceChar()   => state.push()(InTagBody).reset()
        case ':'                => state(char) // FIXME: Namespaces
        case '/'                => if state.current.isEmpty then state(ClosingTag)
                                   else state(SelfClosingTagName)
        case '>'                => state.push()(Body)
        case _                  => throw ParseError("not a valid tag name character")
      
      case SelfClosingTagName => char match
        case TagChar()          => state(char)
        case ':'                => state(char) // FIXME: Namespaces
        case '>'                => state(Body)
        case _                  => throw ParseError("expected '>'")
      
      case ClosingTag         => char match
        case TagChar()          => state(char)
        case ':'                => state(char) // FIXME: Namespaces
        case '>'                => state.pop()(Body)
        case _                  => throw ParseError("expected '>'")

      case InAttributeName    => char match
        case TagChar()          => state(char)
        case WhitespaceChar()   => state(InAttributeName)
        case '>'                => throw ParseError("attribute value has not been specified")
        case '='                => state(AttributeEquals).reset()
        case ':'                => state(char) // FIXME: Namespaces
        case _                  => throw ParseError("not a valid attribute name character")
      
      case AttributeEquals    => char match
        case WhitespaceChar()   => state()
        case '"'                => state(AttributeValue)
        case _                  => throw ParseError("expected '='")
      
      case AttributeValue     => char match
        case '"'                => state(InTagBody).reset()
        case '&'                => state(InAttributeEntity)
        case char               => state(char)
      
      case InTagBody          => char match
        case WhitespaceChar()   => state(InTagBody)
        case TagChar()          => state(InAttributeName, char)
        case '>'                => state(Body)
        case '/'                => state(TagClose)
        case _                  => throw ParseError("character not permitted in a tag name")

      case TagClose           => char match
        case '>'                => state(Body)
        case _                  => throw ParseError("expected '>'")
      
      case Body               => char match
        case '<'                => state(InTagName).reset()
        case '&'                => state(InBodyEntity).reset()
        case _                  => state()
      
      case InBodyEntity       => char match
        case ';'                => state()
        case _                  => throw ParseError("not a valid entity name character")

      case InAttributeEntity  => char match
        case ';'                => state()
        case TagChar()          => state()
        case _                  => throw ParseError("not a valid entity name character")
  }.copy(source = state.source+string)