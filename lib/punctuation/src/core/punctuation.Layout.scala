package punctuation

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import doms.html.whatwg
import attributives.textAttributes

enum Layout extends Markdown.Node:
  case BlockQuote(line: Ordinal, layout: Layout*)

  case OrderedList
        (line: Ordinal, start: Int, tight: Boolean, delimiter: Optional['.' | ')'], items: List[Layout]*)

  case BulletList(line: Ordinal, tight: Boolean, items: List[Layout]*)
  case CodeBlock(line: Ordinal, info: List[Text], content: Text)
  case Paragraph(line: Ordinal, prose: Prose*)
  case Heading(line: Ordinal, level: 1 | 2 | 3 | 4 | 5 | 6, prose: Prose*)
  case ThematicBreak(line: Ordinal)
  case HtmlBlock(line: Ordinal, html: Text)

  def line: Ordinal

  def children: Seq[Prose] = this match
    case Paragraph(_, children*)  => children
    case BlockQuote(_, children*) => children.flatMap(_.children)
    case Heading(_, _, children*) => children
    case _                        => Nil
