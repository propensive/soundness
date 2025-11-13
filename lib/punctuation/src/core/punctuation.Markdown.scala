package punctuation

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import distillate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import proscenium.*
import turbulence.*
import vacuous.*
import zephyrine.*

enum Blocks extends Markdown.Node:
  case ThematicBreak
  case Paragraph(children: Phrases*)
  case Heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Phrases*)
  case FencedCode(lang: Optional[Text], meta: Optional[Text], value: Text)
  case IndentedCode(code: Text)
  case Blockquote(children: Blocks*)
  case Reference(id: Text, location: Text)

enum Phrases extends Markdown.Node:
  case Prose(text: Text)


object Markdown:
  trait Node

  given decodable: (Markdown of Blocks) is Aggregable:
    type Operand = Text

    enum Break:
      case Newline, Code, Paragraph

    def aggregate(stream: Stream[Text]): Markdown of Blocks =
      val cursor = Cursor(stream.iterator)
      var blocks: List[Blocks] = Nil
      val buffer = StringBuilder()

      def break(continue: Boolean, newlines: Int = 0, spaces: Int = 0): Break =
        if continue then cursor.datum match
          case '\n'  => break(cursor.next(), newlines + 1, 0)
          case ' '   => if spaces == 3 then Break.Code
                        else break(cursor.next(), newlines, spaces + 1)
          case other => if newlines == 0 then Break.Newline else Break.Paragraph
            
        else Break.Paragraph
      
      def code(): Unit = cursor.retain:
        def recur(start: Cursor.Mark, spaces: Int, continue: Boolean): Unit =
          if continue then cursor.datum match
            case '\n' =>
              cursor.extract(start, cursor.last)(buffer.append(_))
              buffer.append("\n")
              recur(cursor.mark, 0, cursor.next())
            
            case ' ' =>
              if spaces == -1 then recur(start, -1, cursor.next())
              else if spaces == 4 then recur(cursor.mark, -1, cursor.next())
              else recur(start, spaces + 1, cursor.next())
            
            case other =>
              if spaces == 4 then recur(cursor.mark, -1, cursor.next())
              else if spaces == -1 then recur(start, -1, cursor.next())
              else
                blocks ::= Blocks.IndentedCode(buffer.toString.tt)
                buffer.clear()
              
        recur(cursor.mark, 4, !cursor.finished)
        
      def block(): Unit = cursor.retain:
        def recur(start: Cursor.Mark, spaces: Int, continue: Boolean): Unit =
          if continue then cursor.datum match
            case ' ' => recur(start, spaces + 1, cursor.next())
            
            case '\n' =>
              val last = cursor.last
              cursor.extract(start, last)(buffer.append(_))
              
              break(cursor.next()) match
                case Break.Newline   =>
                  buffer.append("\n")
                  recur(cursor.mark, 0, cursor.next())
                  
                case Break.Paragraph =>
                  blocks ::= Blocks.Paragraph(Phrases.Prose(buffer.toString))
                  buffer.clear()
                  recur(cursor.mark, 0, cursor.next())
                  
                case Break.Code      =>
                  blocks ::= Blocks.Paragraph(Phrases.Prose(buffer.toString.tt))
                  buffer.clear()
                  recur(cursor.mark, 0, cursor.next())
                
            case other =>
              if spaces != 0 then recur(cursor.mark, 0, cursor.next())
              else recur(start, 0, cursor.next())
        
        recur(cursor.mark, 0, !cursor.finished)
              
      break(cursor.next(), 1) match
        case Break.Code => code()
        case _          => block()

      Markdown(blocks.reverse*)


  given renderable: (Markdown of Blocks) is Renderable to html5.Flow = markdown =>
    import Markdown.*
    import html5.*


    markdown.children.map:
      case Blocks.Paragraph(children*) =>
        P(children.map { case Phrases.Prose(text) => text }.join)

      case Blocks.IndentedCode(code) =>
        Pre(Code(code))
        
      case _ =>
        P("unknown")


  def apply(blocks: Blocks*): Markdown of Blocks = new Markdown:
    type Topic = Blocks
    val children: Seq[Blocks] = blocks

  @targetName("applyPhrases")
  def apply(phrases: Phrases*): Markdown of Phrases = new Markdown:
    type Topic = Phrases
    val children: Seq[Phrases] = phrases

abstract class Markdown:
  type Topic <: Markdown.Node

  val children: Seq[Topic]

  override def equals(that: Any): Boolean = that match
    case that: Markdown => that.children == children
    case _              => false

  override def hashCode: Int = children.hashCode
