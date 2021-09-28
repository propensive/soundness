package xylophone

import contextual.*
import rudiments.*
import gossamer.*

object XmlInterpolation:

  enum Input:
    case StringLike(str: String)
    case XmlLike(xml: Ast.Element)

  enum ContextType:
    case AttributeValue, InTagName, SelfClosingTagName, TagClose, ClosingTag, InAttributeName,
        InTagBody, AttributeEquals, Body, InBodyEntity, InAttributeEntity

  case class CharExtractor(chars: Set[Char]):
    val charSet = chars.to(Set)
    def unapply(char: Char): Boolean = charSet.contains(char)

  case class ParseStateNode(name: String, namespaces: Set[String])

  case class ParseState(offset: Int, context: ContextType, stack: List[ParseStateNode], current: String,
                            source: String, ns: Boolean):
    def apply(newContext: ContextType, char: Char) =
      copy(context = newContext, current = current + char, offset = offset + 1)
    
    def apply(newContext: ContextType): ParseState =
      copy(context = newContext, offset = offset + 1, ns = false)
    
    def apply(char: Char): ParseState = copy(offset = offset + 1, current = current + char)
    def apply(): ParseState = copy(offset = offset + 1)
    def reset: ParseState = copy(current = "")
    def namespace: ParseState = copy(ns = true)
    def push: ParseState = copy(stack = ParseStateNode(current, Set()) :: stack, current = "")
    
    def addNamespace(ns: String): ParseState =
      copy(stack = stack.head.copy(namespaces = stack.head.namespaces + ns) :: stack.tail)

    def checkNs: Boolean =
      !stack.head.name.contains(':') || stack.flatMap(_.namespaces).contains(stack.head.name.cut(":")(0))

    def rollback(difference: Int): ParseState = copy(offset = offset - difference)
    
    def pop: ParseState = stack.headOption match
      case Some(tag) if tag.name == current =>
        copy(stack = stack.tail)
      
      case Some(tag) =>
        throw InterpolationError(s"closing tag '$current' does not match expected tag '${tag.name}'", offset - current.length, current.length)
      
      case None =>
        throw InterpolationError(s"spurious closing tag: $current", offset - current.length, current.length)

  given Substitution[Input, String, "str"] with
    def embed(value: String) = Input.StringLike(value)

  given genInsert[T](using writer: XmlWriter[T]): Insertion[Input, T] =
    value => Input.XmlLike(writer.write(value))

  object XmlInterpolator extends Interpolator[Input, ParseState, Doc]:
    import ContextType.*
    val Letters = ('a' to 'z').to(Set) ++ ('A' to 'Z').to(Set)
    val Digits = ('0' to '9').to(Set)
    val TagChar = CharExtractor(Letters ++ Digits + '.' + '-' + '_')
    val WhitespaceChar = CharExtractor(Set(' ', '\t', '\n', '\r'))

    def initial: ParseState = ParseState(0, ContextType.Body, Nil, "", "", false)

    private def escape(str: String): String =
      str.replaceAll("\"", "&quot;").nn
        .replaceAll("'", "&apos;").nn
        .replaceAll("<", "&lt;").nn
        .replaceAll(">", "&gt;").nn
        .replaceAll("&", "&amp;").nn

    def skip(state: ParseState): ParseState = state.context match
      case AttributeValue | Body => parse(state, "")
      case AttributeEquals       => parse(state, "\"\"")
      case _                     => throw InterpolationError(s"a substitution cannot be made in this"+
                                        " position")

    def insert(state: ParseState, value: Input): ParseState =
      state.context match
        case AttributeValue | Body => value match
          case Input.StringLike(str) => parse(state, escape(str))
          case Input.XmlLike(xml)    => parse(state, xml.toString)
        case AttributeEquals       => value match
            case Input.StringLike(str) => parse(state, "\""+escape(str)+"\"")
            case Input.XmlLike(xml)    => parse(state, "\""+escape(xml.toString)+"\"")
        case _ =>
          throw InterpolationError(s"a substitution cannot be made in this position")
    
    def complete(state: ParseState): Doc =
      if state.stack.nonEmpty then throw InterpolationError(s"expected closing tag: ${state.stack.head}")
      try Xml.parse(state.source)
      catch case XmlParseError(_, _) => throw InterpolationError("the XML could not be parsed")

    def parse(state: ParseState, string: String): ParseState = string.foldLeft(state.copy(offset = 0)) {
      case (state@ParseState(_, _, _, _, _, _), char) => state.context match
        
        case InTagName          => char match
          case TagChar()          => state(char)
          case WhitespaceChar()   => state.push(InTagBody)
          case ':'                => if state.ns
                                    then throw InterpolationError("the tag name can contain at most one ':' character to indicate a namespace", state.offset, 1)
                                    else state(char).namespace
          case '/'                => if state.current.isEmpty then state(ClosingTag)
                                    else state(SelfClosingTagName)
          case '>'                => if state.push.checkNs then state.push(Body)
                                    else throw InterpolationError(s"the tag uses a namespace that has not been declared with an xmlns attribute")
          case _                  => throw InterpolationError("not a valid tag name character", state.offset, 1)
        
        case SelfClosingTagName => char match
          case TagChar()          => state(char)
          case ':'                => if state.ns
                                    then throw InterpolationError("the tag name can contain at most one ':' character to indicate a namespace", state.offset, 1)
                                    else state(char).namespace
          case '>'                => if state.checkNs then state(Body)
                                    else throw InterpolationError(s"the tag uses a namespace that has not been declared with an xmlns attribute")
          case _                  => throw InterpolationError("expected '>'", state.offset, 1)
        
        case ClosingTag         => char match
          case TagChar()          => state(char)
          case ':'                => if state.ns
                                    then throw InterpolationError("the tag name can contain at most one ':' character to indicate a namespace", state.offset, 1)
                                    else state(char).namespace
          case '>'                => state.pop(Body)
          case WhitespaceChar()   => state()
          case _                  => throw InterpolationError("expected '>' or whitespace", state.offset, 1)

        case InAttributeName    => char match
          case TagChar()          => state(char)
          case WhitespaceChar()   => state(InAttributeName)
          case '>'                => throw InterpolationError("attribute value has not been specified", state.offset, 1)
          case '='                => if state.current.startsWith("xmlns:")
                                    then state.addNamespace(state.current.drop(6))(AttributeEquals)
                                    else state(AttributeEquals)
          case ':'                => if state.ns
                                    then throw InterpolationError("the attribute name can contain at most one ':' character to indicate a namespace", state.offset, 1)
                                    else state(char).namespace
          case ch                 => throw InterpolationError(s"character '$ch' is not valid in an attribute name", state.offset, 1)
        
        case AttributeEquals    => char match
          case WhitespaceChar()   => state()
          case '"'                => state(AttributeValue)
          case _                  => throw InterpolationError("expected '\"'", state.offset, 1)
        
        case AttributeValue     => char match
          case '"'                => state(InTagBody)
          case '&'                => state(InAttributeEntity)
          case char               => state(char)
        
        case InTagBody          => char match
          case WhitespaceChar()   => state(InTagBody)
          case TagChar()          => state(InAttributeName, char)
          case '>'                => if state.checkNs then state(Body)
                                    else throw InterpolationError(s"the tag uses a namespace that has not been declared with an xmlns attribute")
          case '/'                => state(TagClose)
          case ch                 => throw InterpolationError(s"character '$ch' is not permitted in a tag name", state.offset)

        case TagClose           => char match
          case '>'                => state.pop(Body)
          case _                  => throw InterpolationError("expected '>'", state.offset, 1)
        
        case Body               => char match
          case '<'                => state(InTagName)
          case '&'                => state(InBodyEntity)
          case _                  => state()
        
        case InBodyEntity       => char match
          case ';'                => state()
          case ch                 => throw InterpolationError(s"character '$ch' is not valid in an entity name", state.offset, 1)

        case InAttributeEntity  => char match
          case ';'                => state()
          case TagChar()          => state()
          case ch                 => throw InterpolationError(s"character '$ch' is not valid in an entity name", state.offset, 1)
    }.copy(source = state.source+string)