package honeycomb

import anticipation.*

object Renderable:
  given [ValueType: GenericHtmlRenderable] => ValueType is Renderable = new Renderable:
    type Self = ValueType
    type Result = Html[?]

    def html(value: ValueType): List[Html[?]] = ValueType.html(value).map(convert)

    private def convert(html: GenericHtml): Html[?] = html match
      case GenericHtml.Textual(text) =>
        text

      case GenericHtml.Node(label, attributes, children) =>
        val attributes2 = attributes.map(_.s -> _).to(Map)
        val children2 = children.map(convert(_))

        Node(label, attributes2, children2)

trait Renderable:
  type Self
  type Result <: Html[?]
  def html(value: Self): List[Result]
