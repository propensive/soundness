package honeycomb

import hieroglyph.*
import vacuous.*
import rudiments.*
import anticipation.*
import spectacular.*
import gossamer.*

case class HtmlDoc(root: Node["html"])

object HtmlDoc:
  given generic(using encoder: CharEncoder): GenericHttpResponseStream[HtmlDoc] =
    new GenericHttpResponseStream[HtmlDoc]:
      def mediaType: Text = t"text/html; charset=${encoder.encoding.name}"
      def content(value: HtmlDoc): LazyList[IArray[Byte]] = LazyList(HtmlDoc.serialize(value).bytes)

  def serialize[OutputType](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[OutputType]): OutputType =
    summon[HtmlSerializer[OutputType]].serialize(doc, maxWidth)
  
  def simple[Stylesheet](title: Text, stylesheet: Stylesheet = false)(content: (Html[Flow] | Seq[Html[Flow]])*)
      (using att: HtmlAttribute["href", Stylesheet, ?])
          : HtmlDoc =
    
    val link = (att.convert(stylesheet): @unchecked) match
      case Unset      => Nil
      case text: Text => Seq(Link(rel = Text("stylesheet"), href = text))

    HtmlDoc(Html(Head(Title(title), link), Body(content*)))