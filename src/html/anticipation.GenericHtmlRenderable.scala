package anticipation

trait GenericHtmlRenderable[ValueType]:
  def html(value: ValueType): GenericHtml
