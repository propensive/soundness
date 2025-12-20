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
import attributives.attributiveText

enum Prose extends Markdown.Node:
  case Textual(text: Text)
  case Softbreak
  case Linebreak
  case Code(code: Text)
  case Emphasis(prose: Prose*)
  case Strong(prose: Prose*)
  case Link(destination: Text, title: Optional[Text], prose: Prose*)
  case Image(destination: Text, title: Optional[Text], prose: Prose*)
  case HtmlInline(html: Text)

  def children: Seq[Prose] = this match
    case Link(_, _, prose*)  => prose
    case Image(_, _, prose*) => prose
    case _                   => Nil
