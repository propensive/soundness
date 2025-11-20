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
  case BlockQuote(children: Blocks*)
  case List(ordered: Boolean, start: Text, tight: Boolean, period: Boolean, items: Blocks*)
  case CodeBlock(content: Text)
  case Paragraph(children: Prose*)
  case Heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Blocks*)
  case ThematicBreak
  case HtmlBlock(html: Text)
  //case CustomBlock

enum Prose extends Markdown.Node:
  case Textual(text: Text)
  case Softbreak
  case Linebreak
  case Code(code: Text)
  case Emph(prose: Prose*)
  case Strong(prose: Prose*)
  case Link(destination: Text, title: Text, prose: Prose*)
  case Image(destination: Text, title: Text, prose: Prose*)
  case HtmlInline(html: Text)


object Markdown:
  trait Node

  given decodable: Markdown of Blocks is Aggregable:
    type Operand = Text

    enum Kind:
      case Unknown, Paragraph, Code

    def aggregate(stream: Stream[Text]): Markdown of Blocks =
      val cursor = Cursor(stream.iterator)
      var blocks: List[Blocks] = Nil
      val buffer = java.lang.StringBuilder()
      
      
      def line(): Unit = cursor.hold:
        var continue: Boolean = true
        var started: Boolean = false
        var begin: Mark = Mark.Initial
        while continue do
          cursor.datum match
            case ' '  =>
            case '\n' =>
              continue = false
            case char =>
              if !started then
                begin = cursor.mark
          
          continue &&= cursor.next()
        
        
      while cursor.next() do line()
        
      Markdown(blocks.reverse*)


  given renderable: (Markdown of Blocks) is Renderable to html5.Flow = markdown =>
    import Markdown.*
    import html5.*


    markdown.children.map:
      case Blocks.Paragraph(children*) =>
        val nodes =
          children.map:
            case Prose.Textual(text) => text
            case Prose.Softbreak     => Br
        P(nodes*)

      case Blocks.CodeBlock(code) =>
        Pre(Code(code))
        
      case _ =>
        P("unknown")


  def apply(blocks: Blocks*): Markdown of Blocks = new Markdown:
    type Topic = Blocks
    val children: Seq[Blocks] = blocks

  @targetName("applyProse")
  def apply(prose: Prose*): Markdown of Prose = new Markdown:
    type Topic = Prose
    val children: Seq[Prose] = prose

abstract class Markdown:
  type Topic <: Markdown.Node

  val children: Seq[Topic]

  override def equals(that: Any): Boolean = that match
    case that: Markdown => that.children == children
    case _              => false

  override def hashCode: Int = children.hashCode
